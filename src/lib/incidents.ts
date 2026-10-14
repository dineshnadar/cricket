import { MongoServerError, ObjectId } from "mongodb";
import { z } from "zod";
import { collections } from "./db";
import { env } from "./env";
import { structured } from "./llm";
import { incidentMessage, notifyAgent } from "./notify";
import * as sn from "./servicenow";
import { redact } from "./redact";
import * as splunk from "./splunk";
import type { Agent, Incident, IncidentAnalysis } from "./types";
import { errMsg, shortSha, truncate } from "./util";

const CHANGE_LOOKBACK_DAYS = 14;

// ---------- ingestion ----------

export async function pollServiceNow(): Promise<number> {
  const { agents, incidents, meta } = await collections();
  const list = await agents.find({ enabled: true, "servicenow.enabled": true }).toArray();
  let created = 0;
  for (const agent of list) {
    const key = `sn:${agent._id}`;
    const last = await meta.findOne({ _id: key });
    const lastAt = last ? new Date(last.value as string).getTime() : 0;
    if (Date.now() - lastAt < env.servicenowPollMinutes * 60_000) continue;
    const minutes = lastAt ? (Date.now() - lastAt) / 60_000 + 5 : env.servicenowLookbackMinutes;
    try {
      const found = await sn.recentIncidents(agent.servicenow.query, minutes);
      for (const i of found) {
        const now = new Date();
        try {
          await incidents.insertOne({
            _id: new ObjectId(),
            agentId: agent._id,
            sysId: i.sys_id,
            number: i.number,
            shortDescription: i.short_description,
            description: i.description ?? "",
            priority: i.priority,
            state: i.state,
            openedAt: sn.parseSnDate(i.opened_at),
            url: sn.incidentUrl(i.sys_id),
            status: "new",
            splunkEvents: [],
            createdAt: now,
            updatedAt: now,
          });
          created++;
        } catch (e) {
          if (!(e instanceof MongoServerError && e.code === 11000)) throw e;
        }
      }
      await meta.updateOne({ _id: key }, { $set: { value: new Date().toISOString(), updatedAt: new Date() } }, { upsert: true });
    } catch (e) {
      console.warn(`[servicenow] ${agent.name}: ${errMsg(e)}`);
    }
  }
  return created;
}

/** Manual entry (UI / API) for teams without ServiceNow API access. */
export async function createManualIncident(
  agent: Agent,
  input: { number: string; shortDescription: string; description: string; openedAt?: Date },
): Promise<Incident> {
  const now = new Date();
  const inc: Incident = {
    _id: new ObjectId(),
    agentId: agent._id,
    sysId: `manual-${new ObjectId()}`,
    number: input.number,
    shortDescription: input.shortDescription,
    description: input.description,
    openedAt: input.openedAt ?? now,
    status: "new",
    splunkEvents: [],
    createdAt: now,
    updatedAt: now,
  };
  const { incidents } = await collections();
  await incidents.insertOne(inc);
  return inc;
}

// ---------- analysis ----------

const termsValidator = z.object({ terms: z.array(z.string()).max(6) });

async function extractSearchTerms(inc: Incident): Promise<string[]> {
  const { data } = await structured({
    purpose: `incident terms ${inc.number}`,
    system: "You turn production incident tickets into precise log search terms.",
    prompt: `Incident ${inc.number}: ${redact(inc.shortDescription, { emails: true })}\n\n${redact(truncate(inc.description, 4000), { emails: true })}\n\nReturn up to 6 exact strings likely to appear in application logs for this problem: error codes, exception class names, endpoint paths, service names, distinctive message fragments. Avoid generic words like "error" or "failed".`,
    toolName: "record_terms",
    toolDescription: "Record log search terms.",
    inputSchema: {
      type: "object",
      properties: { terms: { type: "array", items: { type: "string" }, maxItems: 6 } },
      required: ["terms"],
    },
    validator: termsValidator,
    maxTokens: 400,
  });
  return data.terms.map((t) => t.trim()).filter((t) => t.length > 2);
}

const analysisValidator = z.object({
  summary: z.string(),
  likelyCauses: z
    .array(
      z.object({
        changeId: z.string().optional(),
        repo: z.string().optional(),
        sha: z.string().optional(),
        reason: z.string(),
        confidence: z.number().min(0).max(1),
      }),
    )
    .default([]),
  evidence: z.array(z.string()).default([]),
  suggestedActions: z.array(z.string()).default([]),
});

const analysisSchema = {
  type: "object",
  properties: {
    summary: { type: "string", description: "Markdown, 3-6 sentences: what is happening and the most likely explanation." },
    likelyCauses: {
      type: "array",
      description: "Recent changes that plausibly caused this, most likely first. Empty if nothing fits.",
      items: {
        type: "object",
        properties: {
          changeId: { type: "string", description: "The change id exactly as given." },
          repo: { type: "string" },
          sha: { type: "string" },
          reason: { type: "string", description: "Why this change fits the symptoms; cite log lines or files." },
          confidence: { type: "number", minimum: 0, maximum: 1 },
        },
        required: ["reason", "confidence"],
      },
    },
    evidence: { type: "array", items: { type: "string" }, description: "Specific log lines/facts supporting the analysis." },
    suggestedActions: { type: "array", items: { type: "string" }, description: "Concrete next steps for the on-call engineer." },
  },
  required: ["summary", "likelyCauses", "evidence", "suggestedActions"],
};

/** Version of the incident-analysis prompt; stored with each analysis and in eval reports. Bump when the prompt changes. */
export const INCIDENT_PROMPT_VERSION = "incident-v1";

export interface ComputedAnalysis {
  analysis: IncidentAnalysis;
  splunkQuery?: string;
  events: Incident["splunkEvents"];
  splunkError: string;
  candidateIds: string[];
}

/**
 * Produce an analysis without saving or notifying anything.
 * `reuseEvidence` uses the Splunk events already stored on the incident (evals must be repeatable).
 */
export async function computeIncidentAnalysis(inc: Incident, agent: Agent, opts: { reuseEvidence?: boolean } = {}): Promise<ComputedAnalysis> {
  const { changes, runs } = await collections();

  // 1. Splunk evidence
  let terms: string[] = inc.analysis?.searchTerms ?? [];
  let events = inc.splunkEvents;
  let splunkQuery = inc.splunkQuery;
  let splunkError = "";
  if (!opts.reuseEvidence && agent.splunk.enabled && agent.splunk.baseSearch) {
    try {
      terms = await extractSearchTerms(inc);
      splunkQuery = splunk.buildSearch(agent.splunk.baseSearch, terms, agent.splunk.maxEvents);
      const earliest = new Date(inc.openedAt.getTime() - agent.splunk.windowBeforeMinutes * 60_000);
      const latest = new Date(inc.openedAt.getTime() + agent.splunk.windowAfterMinutes * 60_000);
      events = (await splunk.search(splunkQuery, earliest, latest)).map((e) => ({ ...e, raw: redact(e.raw, { emails: true }) }));
    } catch (e) {
      splunkError = errMsg(e);
    }
  }

  // 2. Candidate changes: relevant changes deployed before the incident
  const since = new Date(inc.openedAt.getTime() - CHANGE_LOOKBACK_DAYS * 86400_000);
  const candidates = await changes
    .find({ agentId: agent._id, receivedAt: { $gte: since, $lte: inc.openedAt }, "triage.relevant": true })
    .sort({ receivedAt: -1 })
    .limit(40)
    .toArray();
  const failedRuns = await runs
    .find({ agentId: agent._id, triggeredAt: { $gte: since }, status: { $in: ["failure", "unstable", "error"] } })
    .limit(20)
    .toArray();

  const prompt = `<module name="${agent.module}">${agent.description}</module>
<incident number="${inc.number}" opened="${inc.openedAt.toISOString()}" priority="${inc.priority ?? ""}">
${redact(inc.shortDescription, { emails: true })}

${redact(truncate(inc.description, 6000), { emails: true })}
</incident>
<splunk query="${splunkQuery ?? "not run"}" events="${events.length}"${splunkError ? ` error="${splunkError}"` : ""}>
${events.map((e) => `[${e.time ?? ""}] ${e.host ?? ""} ${e.raw}`).join("\n").slice(0, 40_000)}
</splunk>
<recent_changes lookback_days="${CHANGE_LOOKBACK_DAYS}">
${candidates
  .map(
    (c) =>
      `<change id="${c._id}" repo="${c.repo}" branch="${c.branch}" sha="${c.after}" received="${c.receivedAt.toISOString()}" risk="${c.triage?.riskLevel}" authors="${[...new Set(c.commits.map((x) => x.author))].join(", ")}">
${truncate(c.triage?.summary ?? "", 900)}
files: ${c.matchedFiles.slice(0, 25).join(", ")}
</change>`,
  )
  .join("\n") || "(no relevant changes in window)"}
</recent_changes>
<failed_pipeline_runs>
${failedRuns.map((r) => `- ${r.pipelineName} ${r.status} for change ${r.changeId} at ${r.triggeredAt.toISOString()}`).join("\n") || "(none)"}
</failed_pipeline_runs>

Correlate the incident with the logs and recent changes. Note that a change being received does not prove it was deployed; weigh timing accordingly. Call record_incident_analysis.`;

  const { data, model } = await structured({
    purpose: `incident analysis ${inc.number}`,
    system:
      "You are the incident commander's analyst for a business-critical financial module. You correlate production incidents with log evidence and recent code changes. " +
      "Be calibrated: low confidence when the link is only temporal. Never invent change ids, shas or log lines.",
    prompt,
    toolName: "record_incident_analysis",
    toolDescription: "Record the incident analysis.",
    inputSchema: analysisSchema,
    validator: analysisValidator,
    maxTokens: 3000,
  });

  const validIds = new Map(candidates.map((c) => [String(c._id), c]));
  const analysis: IncidentAnalysis = {
    ...data,
    // Drop causes that cite a change id we never gave Claude; keep uncited ones (e.g. config/infra).
    likelyCauses: data.likelyCauses
      .filter((lc) => !lc.changeId || validIds.has(lc.changeId))
      .map((lc) => {
        const c = lc.changeId ? validIds.get(lc.changeId) : undefined;
        return c ? { ...lc, repo: c.repo, sha: c.after } : lc;
      })
      .sort((a, b) => b.confidence - a.confidence),
    searchTerms: terms,
    model,
    promptVersion: INCIDENT_PROMPT_VERSION,
    at: new Date(),
  };
  return { analysis, splunkQuery, events, splunkError, candidateIds: [...validIds.keys()] };
}

export async function analyzeIncident(inc: Incident, agent: Agent): Promise<Incident> {
  const { incidents } = await collections();
  await incidents.updateOne({ _id: inc._id }, { $set: { status: "analyzing", updatedAt: new Date() } });

  try {
    const { analysis, splunkQuery, events, splunkError } = await computeIncidentAnalysis(inc, agent);
    const updated: Incident = {
      ...inc,
      splunkQuery,
      splunkEvents: events,
      analysis,
      status: "analyzed",
      error: splunkError ? `Splunk: ${splunkError}` : undefined,
      updatedAt: new Date(),
    };

    if (!inc.notified) {
      await notifyAgent(agent, incidentMessage(agent, updated));
      updated.notified = true;
    }
    if (agent.servicenow.postWorkNotes && !inc.workNotePosted && !inc.sysId.startsWith("manual-") && !env.dryRun) {
      try {
        await sn.addWorkNote(inc.sysId, workNote(updated));
        updated.workNotePosted = true;
      } catch (e) {
        updated.error = [updated.error, `Work note: ${errMsg(e)}`].filter(Boolean).join("; ");
      }
    }
    const { _id, ...rest } = updated;
    await incidents.updateOne({ _id }, { $set: rest });
    return updated;
  } catch (e) {
    await incidents.updateOne({ _id: inc._id }, { $set: { status: "error", error: errMsg(e), updatedAt: new Date() } });
    throw e;
  }
}

export function workNote(inc: Incident): string {
  const a = inc.analysis!;
  const lines = [
    "[Wisdom Ops automated analysis]",
    a.summary,
    "",
    "Likely causes:",
    ...(a.likelyCauses.length
      ? a.likelyCauses.map((c) => `- ${c.repo ?? "?"}@${shortSha(c.sha)} (${Math.round(c.confidence * 100)}%): ${c.reason}`)
      : ["- none identified"]),
    "",
    "Suggested actions:",
    ...a.suggestedActions.map((s) => `- ${s}`),
    "",
    `Details: ${env.appBaseUrl}/incidents/${inc._id}`,
  ];
  return lines.join("\n");
}

/** Worker job: analyse new incidents. */
export async function analyzeNewIncidents(limit = 5): Promise<number> {
  const { incidents, agents } = await collections();
  const list = await incidents.find({ status: "new" }).sort({ openedAt: 1 }).limit(limit).toArray();
  for (const inc of list) {
    const agent = await agents.findOne({ _id: inc.agentId });
    if (!agent) continue;
    try {
      await analyzeIncident(inc, agent);
    } catch (e) {
      console.warn(`[incident] ${inc.number}: ${errMsg(e)}`);
    }
  }
  return list.length;
}
