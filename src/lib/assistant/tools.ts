import { ObjectId, type Filter } from "mongodb";
import { z } from "zod";
import { createAgent, updateAgent } from "../agents";
import { requeueChange } from "../changes";
import { collections, toId } from "../db";
import { env, integrationStatus } from "../env";
import * as github from "../github";
import { analyzeIncident } from "../incidents";
import * as jenkins from "../jenkins";
import * as teamcity from "../teamcity";
import { findRepoConfig } from "../paths";
import { triggerPipeline } from "../pipelines";
import { redact, redactDeep } from "../redact";
import { agentInputSchema, formatZodError, type AgentInput } from "../schemas";
import * as splunk from "../splunk";
import type { Agent, Change } from "../types";
import { shortSha, truncate } from "../util";
import { latestWisdom, refreshWisdom } from "../wisdom";
import { workflowYaml } from "../workflow-template";

export interface ToolContext {
  user: string;
  defaultAgentId?: ObjectId;
}

export interface ReadResult {
  data: unknown;
  summary: string;
}

export interface ActionPreview {
  title: string;
  details: string[];
  /** Normalised input stored with the pending action. */
  input: Record<string, unknown>;
}

export interface ActionOutcome {
  text: string;
  secret?: string;
}

interface BaseTool {
  name: string;
  description: string;
  input_schema: Record<string, unknown>;
  label: (input: Record<string, unknown>) => string;
}
export interface ReadTool extends BaseTool {
  kind: "read";
  run: (ctx: ToolContext, input: Record<string, unknown>) => Promise<ReadResult>;
}
export interface WriteTool extends BaseTool {
  kind: "write";
  /** Validate and describe; throws with a message Claude can act on. */
  preview: (ctx: ToolContext, input: Record<string, unknown>) => Promise<ActionPreview>;
  execute: (ctx: ToolContext, input: Record<string, unknown>) => Promise<ActionOutcome>;
}
export type Tool = ReadTool | WriteTool;

export const MAX_RESULT_CHARS = 12_000;

// ---------- helpers ----------

/** Server-side repo check for agent setup: Claude can't skip it. */
async function requireRepos(fullNames: string[]) {
  if (!fullNames.length) return;
  const missing = await github.findMissingRepos(fullNames);
  if (missing.length) {
    throw new Error(
      `These repos don't exist or the GitHub token can't see them: ${missing.join(", ")}. Check the names with search_github_repos, or ask the user to grant access.`,
    );
  }
}

/** Repos in the new config that the agent doesn't already watch. */
function newRepos(a: { repos: { fullName: string }[] }, next: { repos: { fullName: string }[] }): string[] {
  const have = new Set(a.repos.map((r) => r.fullName.toLowerCase()));
  return next.repos.map((r) => r.fullName).filter((n) => !have.has(n.toLowerCase()));
}

const str = (v: unknown) => (typeof v === "string" ? v.trim() : "");
const num = (v: unknown, d: number, max: number) => {
  const n = Number(v);
  return Number.isFinite(n) && n > 0 ? Math.min(n, max) : d;
};
const since = (days: number) => new Date(Date.now() - days * 86400_000);
const esc = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

export async function resolveAgent(ctx: ToolContext, ref?: unknown): Promise<Agent> {
  const { agents } = await collections();
  const r = str(ref);
  if (r) {
    const id = toId(r);
    const byId = id && (await agents.findOne({ _id: id }));
    if (byId) return byId;
    const rx = new RegExp(`^${esc(r)}$`, "i");
    const byName = await agents.findOne({ $or: [{ name: rx }, { module: rx }] });
    if (byName) return byName;
    const fuzzy = await agents.findOne({ $or: [{ name: new RegExp(esc(r), "i") }, { module: new RegExp(esc(r), "i") }] });
    if (fuzzy) return fuzzy;
    throw new Error(`No agent matches "${r}". Call list_agents to see the options.`);
  }
  if (ctx.defaultAgentId) {
    const a = await agents.findOne({ _id: ctx.defaultAgentId });
    if (a) return a;
  }
  const all = await agents.find({}).limit(2).toArray();
  if (all.length === 1) return all[0];
  throw new Error("Several agents exist; pass `agent` (name, module or id). Call list_agents to see them.");
}

async function agentFilter(ctx: ToolContext, ref?: unknown): Promise<{ agentId?: ObjectId }> {
  if (!str(ref) && !ctx.defaultAgentId) return {};
  const a = await resolveAgent(ctx, ref);
  return { agentId: a._id };
}

function changeBrief(c: Change) {
  return {
    change_id: String(c._id),
    repo: c.repo,
    branch: c.branch,
    sha: shortSha(c.after),
    received: c.receivedAt.toISOString(),
    status: c.status,
    risk: c.triage?.riskLevel,
    relevant: c.triage?.relevant,
    summary: c.triage ? truncate(c.triage.summary, 600) : undefined,
    authors: [...new Set(c.commits.map((x) => x.author))],
    first_commit: c.commits[0]?.message.split("\n")[0],
    watched_files: c.matchedFiles.slice(0, 15),
  };
}

// ---------- read tools ----------

const readTools: ReadTool[] = [
  {
    kind: "read",
    name: "list_agents",
    description: "List the configured agents (one per module) with their repos and pipelines. Use first when unsure which module the user means.",
    input_schema: { type: "object", properties: {} },
    label: () => "Listed agents",
    run: async () => {
      const { agents } = await collections();
      const list = await agents.find({}).toArray();
      return {
        data: list.map((a) => ({
          agent_id: String(a._id),
          name: a.name,
          module: a.module,
          enabled: a.enabled,
          repos: a.repos.map((r) => r.fullName),
          pipelines: a.pipelines.map((p) => p.name),
        })),
        summary: `${list.length} agent${list.length === 1 ? "" : "s"}`,
      };
    },
  },
  {
    kind: "read",
    name: "get_agent_config",
    description: "Full configuration of one agent: scope description, repos (branches, watched paths, owner notes), pipelines, notification and incident settings.",
    input_schema: { type: "object", properties: { agent: { type: "string", description: "Agent name, module or id" } } },
    label: (i) => `Read config${i.agent ? ` for ${i.agent}` : ""}`,
    run: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      const { ingestTokenHash: _h, ...rest } = a;
      return { data: { ...rest, _id: String(a._id), teamsWebhookUrl: a.teamsWebhookUrl ? "(set)" : "" }, summary: a.name };
    },
  },
  {
    kind: "read",
    name: "search_changes",
    description:
      "Search recorded pushes (newest first). Filter by agent, repo, free-text (matches Claude's summary, commit messages and file paths), risk, author, and age. Returns brief rows with change_id for follow-up.",
    input_schema: {
      type: "object",
      properties: {
        agent: { type: "string" },
        repo: { type: "string", description: "owner/name or a fragment" },
        query: { type: "string", description: "Words to look for, e.g. 'expiry' or 'CipVerification'" },
        risk: { type: "string", enum: ["low", "medium", "high"] },
        author: { type: "string" },
        since_days: { type: "number", description: "Default 14" },
        relevant_only: { type: "boolean", description: "Default true" },
        limit: { type: "number", description: "Default 15, max 40" },
      },
    },
    label: (i) => `Searched changes${i.query ? ` for "${i.query}"` : ""}${i.repo ? ` in ${i.repo}` : ""}`,
    run: async (ctx, i) => {
      const { changes } = await collections();
      const f: Filter<Change> = { ...(await agentFilter(ctx, i.agent)), receivedAt: { $gte: since(num(i.since_days, 14, 365)) } };
      if (i.relevant_only !== false) f["triage.relevant"] = true;
      if (str(i.repo)) f.repo = new RegExp(esc(str(i.repo)), "i");
      if (str(i.risk)) f["triage.riskLevel"] = str(i.risk) as "low";
      if (str(i.author)) f["commits.author"] = new RegExp(esc(str(i.author)), "i");
      if (str(i.query)) {
        const rx = new RegExp(str(i.query).split(/\s+/).map(esc).join("|"), "i");
        f.$or = [{ "triage.summary": rx }, { "commits.message": rx }, { matchedFiles: rx }, { "triage.impactedAreas": rx }];
      }
      const list = await changes.find(f).sort({ receivedAt: -1 }).limit(num(i.limit, 15, 40)).toArray();
      return { data: list.map(changeBrief), summary: `${list.length} change${list.length === 1 ? "" : "s"}` };
    },
  },
  {
    kind: "read",
    name: "get_change",
    description: "Full detail of one change: Claude's triage (summary, risk reasons, test focus), commits, file list, triggered pipeline runs and notifications.",
    input_schema: { type: "object", properties: { change_id: { type: "string" } }, required: ["change_id"] },
    label: (i) => `Opened change ${String(i.change_id ?? "").slice(-6)}`,
    run: async (_ctx, i) => {
      const { changes, runs } = await collections();
      const id = toId(str(i.change_id));
      const c = id && (await changes.findOne({ _id: id }));
      if (!c) throw new Error(`No change with id ${i.change_id}`);
      const rs = await runs.find({ changeId: c._id }).toArray();
      return {
        data: {
          ...changeBrief(c),
          sha: c.after,
          compare_url: c.compareUrl,
          triage: c.triage,
          commits: c.commits.map((x) => ({ sha: shortSha(x.sha), author: x.author, message: truncate(x.message, 400), url: x.url })),
          files: c.files.map((f) => ({ file: f.filename, status: f.status, add: f.additions, del: f.deletions, watched: c.matchedFiles.includes(f.filename) })),
          runs: rs.map((r) => ({ run_id: String(r._id), pipeline: r.pipelineName, status: r.status, build_url: r.buildUrl, tests: r.tests })),
          notifications: c.notifications,
          link: `${env.appBaseUrl}/changes/${c._id}`,
        },
        summary: `${c.repo}@${shortSha(c.after)}`,
      };
    },
  },
  {
    kind: "read",
    name: "get_diff",
    description: "The patch (diff) of one change, optionally for a single file. Use when you need the exact lines changed.",
    input_schema: {
      type: "object",
      properties: { change_id: { type: "string" }, file: { type: "string", description: "Path or fragment; omit for all watched files" } },
      required: ["change_id"],
    },
    label: (i) => `Read diff${i.file ? ` of ${String(i.file).split("/").pop()}` : ""}`,
    run: async (_ctx, i) => {
      const { changes } = await collections();
      const id = toId(str(i.change_id));
      const c = id && (await changes.findOne({ _id: id }));
      if (!c) throw new Error(`No change with id ${i.change_id}`);
      const want = str(i.file).toLowerCase();
      const files = c.files.filter((f) => (want ? f.filename.toLowerCase().includes(want) : c.matchedFiles.includes(f.filename) || c.matchedFiles.length === 0));
      return {
        data: files.slice(0, 10).map((f) => ({ file: f.filename, patch: f.patch ? redact(truncate(f.patch, 5000)) : "(no patch: binary or too large)" })),
        summary: `${files.length} file${files.length === 1 ? "" : "s"}`,
      };
    },
  },
  {
    kind: "read",
    name: "read_repo_file",
    description: "Read a file from a watched repo on GitHub Enterprise (current default branch unless ref is given). Use for code the diff doesn't show.",
    input_schema: {
      type: "object",
      properties: { repo: { type: "string", description: "owner/name" }, path: { type: "string" }, ref: { type: "string", description: "branch or sha" } },
      required: ["repo", "path"],
    },
    label: (i) => `Read ${String(i.path ?? "").split("/").pop()}`,
    run: async (ctx, i) => {
      const repo = str(i.repo);
      const { agents } = await collections();
      const watched = await agents.findOne({ "repos.fullName": new RegExp(`^${esc(repo)}$`, "i") });
      if (!watched) throw new Error(`${repo} is not watched by any agent; only watched repos can be read.`);
      const text = await github.readFile(repo, str(i.path), str(i.ref) || undefined);
      return { data: redact(truncate(text, 10_000)), summary: `${text.split("\n").length} lines` };
    },
  },
  {
    kind: "read",
    name: "list_runs",
    description: "Recent test pipeline runs (Jenkins / GitHub Actions), newest first, with status, test counts and the change that triggered them.",
    input_schema: {
      type: "object",
      properties: {
        agent: { type: "string" },
        pipeline: { type: "string" },
        status: { type: "string", enum: ["queued", "running", "success", "failure", "unstable", "aborted", "error"] },
        since_days: { type: "number", description: "Default 7" },
        limit: { type: "number", description: "Default 15" },
      },
    },
    label: (i) => `Listed runs${i.pipeline ? ` of ${i.pipeline}` : ""}${i.status ? ` (${i.status})` : ""}`,
    run: async (ctx, i) => {
      const { runs } = await collections();
      const f: Record<string, unknown> = { ...(await agentFilter(ctx, i.agent)), triggeredAt: { $gte: since(num(i.since_days, 7, 90)) } };
      if (str(i.pipeline)) f.pipelineName = new RegExp(esc(str(i.pipeline)), "i");
      if (str(i.status)) f.status = str(i.status);
      const list = await runs.find(f).sort({ triggeredAt: -1 }).limit(num(i.limit, 15, 50)).toArray();
      return {
        data: list.map((r) => ({
          run_id: String(r._id),
          pipeline: r.pipelineName,
          status: r.status,
          build: r.buildNumber,
          build_url: r.buildUrl,
          tests: r.tests,
          triggered: r.triggeredAt.toISOString(),
          change_id: String(r.changeId),
          error: r.error,
        })),
        summary: `${list.length} run${list.length === 1 ? "" : "s"}`,
      };
    },
  },
  {
    kind: "read",
    name: "get_build_log",
    description: "Tail of a build's console log for a run (Jenkins or TeamCity). Use to see why a run failed.",
    input_schema: { type: "object", properties: { run_id: { type: "string" }, lines: { type: "number", description: "Default 80, max 300" } }, required: ["run_id"] },
    label: () => "Read build log",
    run: async (_ctx, i) => {
      const { runs } = await collections();
      const id = toId(str(i.run_id));
      const r = id && (await runs.findOne({ _id: id }));
      if (!r) throw new Error(`No run with id ${i.run_id}`);
      const lines = num(i.lines, 80, 300);
      let log: string;
      if (r.type === "jenkins" && r.buildUrl) log = await jenkins.consoleTail(r.buildUrl, lines);
      else if (r.type === "teamcity" && r.tcBuildId) log = await teamcity.logTail(r.tcBuildId, lines);
      else throw new Error("Build logs are available for Jenkins and TeamCity runs that have started; GitHub Actions logs aren't fetched.");
      return { data: redact(log, { emails: true }), summary: `${log.split("\n").length} lines` };
    },
  },
  {
    kind: "read",
    name: "search_incidents",
    description: "Search production incidents recorded for an agent, with Claude's analysis summary and top suspected change.",
    input_schema: {
      type: "object",
      properties: { agent: { type: "string" }, query: { type: "string" }, since_days: { type: "number", description: "Default 30" }, limit: { type: "number" } },
    },
    label: (i) => `Searched incidents${i.query ? ` for "${i.query}"` : ""}`,
    run: async (ctx, i) => {
      const { incidents } = await collections();
      const f: Record<string, unknown> = { ...(await agentFilter(ctx, i.agent)), openedAt: { $gte: since(num(i.since_days, 30, 365)) } };
      if (str(i.query)) {
        const rx = new RegExp(str(i.query).split(/\s+/).map(esc).join("|"), "i");
        f.$or = [{ number: rx }, { shortDescription: rx }, { description: rx }, { "analysis.summary": rx }];
      }
      const list = await incidents.find(f).sort({ openedAt: -1 }).limit(num(i.limit, 10, 30)).toArray();
      return {
        data: list.map((x) => ({
          incident_id: String(x._id),
          number: x.number,
          opened: x.openedAt.toISOString(),
          short_description: redact(x.shortDescription, { emails: true }),
          status: x.status,
          summary: x.analysis ? redact(truncate(x.analysis.summary, 400), { emails: true }) : undefined,
          top_cause: x.analysis?.likelyCauses[0],
        })),
        summary: `${list.length} incident${list.length === 1 ? "" : "s"}`,
      };
    },
  },
  {
    kind: "read",
    name: "get_incident",
    description: "Full detail of one incident: description, Splunk evidence, Claude's analysis, likely causes and suggested actions. Accepts the incident id or its ServiceNow number.",
    input_schema: { type: "object", properties: { incident: { type: "string", description: "id or number like INC0099213" } }, required: ["incident"] },
    label: (i) => `Opened ${i.incident}`,
    run: async (_ctx, i) => {
      const { incidents } = await collections();
      const ref = str(i.incident);
      const id = toId(ref);
      const x = (id && (await incidents.findOne({ _id: id }))) || (await incidents.findOne({ number: new RegExp(`^${esc(ref)}$`, "i") }));
      if (!x) throw new Error(`No incident ${ref} recorded. If it's new, ServiceNow sync may not have pulled it yet.`);
      return {
        data: redactDeep(
          {
            incident_id: String(x._id),
            number: x.number,
            opened: x.openedAt.toISOString(),
            priority: x.priority,
            short_description: x.shortDescription,
            description: truncate(x.description, 3000),
            status: x.status,
            analysis: x.analysis,
            splunk_query: x.splunkQuery,
            splunk_events: x.splunkEvents.slice(0, 15).map((e) => `[${e.time ?? ""}] ${e.host ?? ""} ${truncate(e.raw, 300)}`),
            link: `${env.appBaseUrl}/incidents/${x._id}`,
          },
          { emails: true },
        ),
        summary: x.number,
      };
    },
  },
  {
    kind: "read",
    name: "search_logs",
    description:
      "Run a Splunk search within the agent's base search, for a time window. Use sparingly, for questions about current production behaviour. Terms are ORed and added to the base search.",
    input_schema: {
      type: "object",
      properties: {
        agent: { type: "string" },
        terms: { type: "array", items: { type: "string" }, description: "Exact strings: error codes, exception names, endpoints" },
        minutes_back: { type: "number", description: "Default 60, max 1440" },
        max_events: { type: "number", description: "Default 20, max 50" },
      },
      required: ["terms"],
    },
    label: (i) => `Searched logs for ${(i.terms as string[] | undefined)?.slice(0, 2).join(", ") ?? ""}`,
    run: async (ctx, i) => {
      if (!integrationStatus().splunk) throw new Error("Splunk is not configured on the server.");
      const a = await resolveAgent(ctx, i.agent);
      if (!a.splunk.enabled || !a.splunk.baseSearch) throw new Error(`Splunk is not enabled for ${a.name}.`);
      const terms = Array.isArray(i.terms) ? i.terms.map(String) : [];
      const spl = splunk.buildSearch(a.splunk.baseSearch, terms, num(i.max_events, 20, 50));
      const minutes = num(i.minutes_back, 60, 1440);
      const events = await splunk.search(spl, new Date(Date.now() - minutes * 60_000), new Date());
      return {
        data: { query: spl, events: events.map((e) => `[${e.time ?? ""}] ${e.host ?? ""} ${redact(truncate(e.raw, 400), { emails: true })}`) },
        summary: `${events.length} event${events.length === 1 ? "" : "s"}`,
      };
    },
  },
  {
    kind: "read",
    name: "get_wisdom",
    description: "The latest module knowledge summary: module overview plus per-repo purpose, key components, hotspots, risks and testing notes. Good starting point for 'how does X work' or onboarding questions.",
    input_schema: { type: "object", properties: { agent: { type: "string" } } },
    label: () => "Read module wisdom",
    run: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      const w = await latestWisdom(a._id);
      if (!w) return { data: "No wisdom generated yet for this agent. Suggest refresh_wisdom.", summary: "none yet" };
      return { data: { generated: w.generatedAt.toISOString(), module: w.moduleSummary, repos: w.repos }, summary: `generated ${w.generatedAt.toISOString().slice(0, 10)}` };
    },
  },
  {
    kind: "read",
    name: "get_stats",
    description: "Counts for a period: pushes, relevant changes by risk, pipeline results, incidents. Use for 'how are we doing' or weekly summaries.",
    input_schema: { type: "object", properties: { agent: { type: "string" }, days: { type: "number", description: "Default 7" } } },
    label: (i) => `Counted activity (${num(i.days, 7, 90)}d)`,
    run: async (ctx, i) => {
      const { changes, runs, incidents } = await collections();
      const af = await agentFilter(ctx, i.agent);
      const from = since(num(i.days, 7, 90));
      const ch = await changes.find({ ...af, receivedAt: { $gte: from } }, { projection: { status: 1, "triage.riskLevel": 1, repo: 1 } }).toArray();
      const rs = await runs.find({ ...af, triggeredAt: { $gte: from } }, { projection: { status: 1, pipelineName: 1 } }).toArray();
      const inc = await incidents.countDocuments({ ...af, openedAt: { $gte: from } });
      const count = <T,>(xs: T[], key: (x: T) => string | undefined) =>
        xs.reduce<Record<string, number>>((m, x) => {
          const k = key(x) ?? "none";
          m[k] = (m[k] ?? 0) + 1;
          return m;
        }, {});
      return {
        data: {
          days: num(i.days, 7, 90),
          pushes: ch.length,
          by_status: count(ch, (c) => c.status),
          relevant_by_risk: count(ch.filter((c) => c.status === "actioned"), (c) => c.triage?.riskLevel),
          by_repo: count(ch, (c) => c.repo),
          runs_by_status: count(rs, (r) => r.status),
          incidents: inc,
        },
        summary: `${ch.length} pushes, ${rs.length} runs`,
      };
    },
  },
  {
    kind: "read",
    name: "search_github_repos",
    description: "Search GitHub Enterprise repositories by name (for setting up or extending an agent).",
    input_schema: { type: "object", properties: { query: { type: "string" } }, required: ["query"] },
    label: (i) => `Searched GitHub for "${i.query}"`,
    run: async (_ctx, i) => {
      const list = await github.searchRepos(str(i.query));
      return {
        data: list.slice(0, 20).map((r) => ({ repo: r.fullName, description: r.description, default_branch: r.defaultBranch })),
        summary: `${list.length} repo${list.length === 1 ? "" : "s"}`,
      };
    },
  },
];

// ---------- write tools (confirmation required) ----------

const partialAgentSchema = agentInputSchema.partial();

function mergeAgent(a: Agent, patch: Partial<AgentInput>): AgentInput {
  const base: AgentInput = {
    name: a.name,
    module: a.module,
    description: a.description,
    repos: a.repos,
    mode: a.mode,
    pollingIntervalMinutes: a.pollingIntervalMinutes,
    emailRecipients: a.emailRecipients,
    teamsWebhookUrl: a.teamsWebhookUrl,
    relevanceThreshold: a.relevanceThreshold,
    pipelines: a.pipelines,
    notifyOnPipeline: a.notifyOnPipeline,
    servicenow: a.servicenow,
    splunk: a.splunk,
    enabled: a.enabled,
  };
  return { ...base, ...patch } as AgentInput;
}

const agentConfigSchemaDoc = {
  type: "object",
  description: "Agent configuration",
  properties: {
    name: { type: "string" },
    module: { type: "string" },
    description: { type: "string", description: "What belongs to the module and what is out of scope (Claude uses this for relevance)" },
    repos: {
      type: "array",
      items: {
        type: "object",
        properties: {
          fullName: { type: "string", description: "owner/name" },
          branches: { type: "array", items: { type: "string" } },
          paths: { type: "array", items: { type: "string" }, description: "picomatch globs; prefix ! to exclude" },
          notes: { type: "string" },
        },
        required: ["fullName"],
      },
    },
    mode: { type: "string", enum: ["action", "poll", "both"] },
    pollingIntervalMinutes: { type: "number" },
    emailRecipients: { type: "array", items: { type: "string" } },
    teamsWebhookUrl: { type: "string" },
    relevanceThreshold: { type: "number" },
    pipelines: {
      type: "array",
      items: {
        type: "object",
        properties: {
          name: { type: "string" },
          type: { type: "string", enum: ["jenkins", "github-actions", "teamcity"] },
          jobPath: { type: "string", description: "Jenkins job path" },
          buildTypeId: { type: "string", description: "TeamCity build configuration id" },
          paramPrefix: { type: "string", enum: ["env.", "system.", ""], description: "TeamCity prefix for params (default env.)" },
          repo: { type: "string" },
          workflow: { type: "string" },
          ref: { type: "string" },
          params: { type: "object", description: "Name → value; names without dots" },
          runWhen: { type: "string", enum: ["always", "recommended"] },
          onlyRepos: { type: "array", items: { type: "string" } },
        },
        required: ["name", "type"],
      },
    },
    notifyOnPipeline: { type: "string", enum: ["failure", "always", "never"] },
    servicenow: { type: "object", properties: { enabled: { type: "boolean" }, query: { type: "string" }, postWorkNotes: { type: "boolean" } } },
    splunk: {
      type: "object",
      properties: {
        enabled: { type: "boolean" },
        baseSearch: { type: "string" },
        windowBeforeMinutes: { type: "number" },
        windowAfterMinutes: { type: "number" },
        maxEvents: { type: "number" },
      },
    },
    enabled: { type: "boolean" },
  },
};

const writeTools: WriteTool[] = [
  {
    kind: "write",
    name: "trigger_pipeline",
    description: "Start one of the agent's configured test pipelines for a specific change. Requires the user's confirmation in the UI.",
    input_schema: { type: "object", properties: { change_id: { type: "string" }, pipeline: { type: "string", description: "Pipeline name as configured" } }, required: ["change_id", "pipeline"] },
    label: (i) => `Requested run of ${i.pipeline}`,
    preview: async (_ctx, i) => {
      const { changes, agents } = await collections();
      const id = toId(str(i.change_id));
      const c = id && (await changes.findOne({ _id: id }));
      if (!c) throw new Error(`No change with id ${i.change_id}`);
      const a = await agents.findOne({ _id: c.agentId });
      const p = a?.pipelines.find((x) => x.name.toLowerCase() === str(i.pipeline).toLowerCase());
      if (!a || !p) throw new Error(`No pipeline "${i.pipeline}" on this agent. Options: ${a?.pipelines.map((x) => x.name).join(", ") || "none"}`);
      return {
        title: `Run ${p.name}`,
        details: [`For ${c.repo} @ ${shortSha(c.after)} (${c.branch})`, p.type === "jenkins" ? `Jenkins job ${p.jobPath}` : p.type === "teamcity" ? `TeamCity build ${p.buildTypeId}${p.ref ? ` on ${p.ref === "$CHANGE_BRANCH" ? c.branch : p.ref}` : ""}` : `GitHub workflow ${p.repo}/${p.workflow}`],
        input: { change_id: String(c._id), pipeline: p.name },
      };
    },
    execute: async (_ctx, i) => {
      const { changes, agents } = await collections();
      const c = await changes.findOne({ _id: new ObjectId(String(i.change_id)) });
      const a = c && (await agents.findOne({ _id: c.agentId }));
      const p = a?.pipelines.find((x) => x.name === i.pipeline);
      if (!c || !a || !p) throw new Error("Change or pipeline no longer exists");
      const run = await triggerPipeline(a, c, p);
      await changes.updateOne({ _id: c._id }, { $push: { pipelineRunIds: run._id } });
      if (run.status === "error") throw new Error(run.error ?? "Trigger failed");
      const where = run.queueUrl ? " (queued in Jenkins)" : run.tcBuildId ? ` (TeamCity build ${run.tcBuildId})` : "";
      return { text: `Triggered ${p.name}${where}. It will show on the change page as it runs.` };
    },
  },
  {
    kind: "write",
    name: "rerun_triage",
    description: "Re-run Claude's triage (and the resulting pipelines/notifications) for a change.",
    input_schema: { type: "object", properties: { change_id: { type: "string" } }, required: ["change_id"] },
    label: () => "Requested re-triage",
    preview: async (_ctx, i) => {
      const { changes } = await collections();
      const id = toId(str(i.change_id));
      const c = id && (await changes.findOne({ _id: id }));
      if (!c) throw new Error(`No change with id ${i.change_id}`);
      return {
        title: "Re-run triage",
        details: [`${c.repo} @ ${shortSha(c.after)}`, "Pipelines and notifications will fire again if it's judged relevant"],
        input: { change_id: String(c._id) },
      };
    },
    execute: async (_ctx, i) => {
      const ok = await requeueChange(new ObjectId(String(i.change_id)));
      if (!ok) throw new Error("Change is already being processed");
      return { text: "Queued. The worker will re-triage it within about 15 seconds." };
    },
  },
  {
    kind: "write",
    name: "analyze_incident",
    description: "Run (or re-run) the incident analysis: Splunk search plus correlation with recent changes. Does not re-notify the team.",
    input_schema: { type: "object", properties: { incident: { type: "string", description: "id or number" } }, required: ["incident"] },
    label: (i) => `Requested analysis of ${i.incident}`,
    preview: async (_ctx, i) => {
      const { incidents } = await collections();
      const ref = str(i.incident);
      const id = toId(ref);
      const x = (id && (await incidents.findOne({ _id: id }))) || (await incidents.findOne({ number: new RegExp(`^${esc(ref)}$`, "i") }));
      if (!x) throw new Error(`No incident ${ref}`);
      return { title: `Analyse ${x.number}`, details: [x.shortDescription, "Searches Splunk and recent changes (about a minute)"], input: { incident_id: String(x._id) } };
    },
    execute: async (_ctx, i) => {
      const { incidents, agents } = await collections();
      const x = await incidents.findOne({ _id: new ObjectId(String(i.incident_id)) });
      const a = x && (await agents.findOne({ _id: x.agentId }));
      if (!x || !a) throw new Error("Incident or agent no longer exists");
      const r = await analyzeIncident({ ...x, notified: true }, a);
      const top = r.analysis?.likelyCauses[0];
      return { text: `Analysis done.${top ? ` Top suspect: ${top.repo ?? "?"}@${shortSha(top.sha)} (${Math.round(top.confidence * 100)}%).` : " No change fits."}` };
    },
  },
  {
    kind: "write",
    name: "refresh_wisdom",
    description: "Regenerate the module wisdom (reads every repo; takes 1-2 minutes).",
    input_schema: { type: "object", properties: { agent: { type: "string" } } },
    label: () => "Requested wisdom refresh",
    preview: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      return { title: `Refresh wisdom for ${a.name}`, details: [`Reads ${a.repos.length} repos and recent changes`, "Takes 1-2 minutes"], input: { agent: String(a._id) } };
    },
    execute: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      const w = await refreshWisdom(a);
      return { text: `Wisdom regenerated for ${w.repos.length} repos.` };
    },
  },
  {
    kind: "write",
    name: "create_agent",
    description:
      "Create a new agent. Before calling, gather from the user: name, module, a scope description, repos (verify with search_github_repos), branches/paths if known, pipelines, email recipients. Defaults are applied for anything omitted.",
    input_schema: { type: "object", properties: { config: agentConfigSchemaDoc }, required: ["config"] },
    label: (i) => `Requested new agent ${(i.config as { name?: string } | undefined)?.name ?? ""}`,
    preview: async (_ctx, i) => {
      const parsed = agentInputSchema.safeParse(i.config);
      if (!parsed.success) throw new Error(`Config invalid: ${formatZodError(parsed.error)}`);
      const c = parsed.data;
      await requireRepos(c.repos.map((r) => r.fullName));
      return {
        title: `Create agent "${c.name}"`,
        details: [
          `Module: ${c.module}`,
          `Repos: ${c.repos.map((r) => r.fullName).join(", ")}`,
          `Pipelines: ${c.pipelines.map((p) => p.name).join(", ") || "none"}`,
          `Email: ${c.emailRecipients.join(", ") || "none"} · Teams: ${c.teamsWebhookUrl ? "yes" : "no"}`,
          `Detection: ${c.mode}`,
        ],
        input: { config: c },
      };
    },
    execute: async (_ctx, i) => {
      const parsed = agentInputSchema.parse(i.config);
      await requireRepos(parsed.repos.map((r) => r.fullName));
      const { agent, token } = await createAgent(parsed);
      return {
        text: `Created agent ${agent.name} (id ${agent._id}). Next: add the ingest token as the WISDOM_TOKEN secret, then open the setup PRs.`,
        secret: token,
      };
    },
  },
  {
    kind: "write",
    name: "update_agent",
    description:
      "Change an existing agent's configuration. Pass only the fields to change; arrays (repos, pipelines, emailRecipients) replace the whole list, so include existing entries you want to keep (read them with get_agent_config first).",
    input_schema: { type: "object", properties: { agent: { type: "string" }, changes: { ...agentConfigSchemaDoc, description: "Fields to change" } }, required: ["changes"] },
    label: (i) => `Requested update to ${i.agent ?? "agent"}`,
    preview: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      const patch = partialAgentSchema.safeParse(i.changes);
      if (!patch.success) throw new Error(`Changes invalid: ${formatZodError(patch.error)}`);
      // Only keep keys the caller actually sent (partial() still applies inner defaults).
      const sent = Object.keys((i.changes ?? {}) as object);
      const changes = Object.fromEntries(Object.entries(patch.data).filter(([k]) => sent.includes(k))) as Partial<AgentInput>;
      const merged = agentInputSchema.safeParse(mergeAgent(a, changes));
      if (!merged.success) throw new Error(`Resulting config invalid: ${formatZodError(merged.error)}`);
      await requireRepos(newRepos(a, merged.data));
      const details = Object.entries(changes).map(([k, v]) => {
        if (k === "repos") return `repos → ${(v as AgentInput["repos"]).map((r) => r.fullName).join(", ")}`;
        if (k === "pipelines") return `pipelines → ${(v as AgentInput["pipelines"]).map((p) => p.name).join(", ")}`;
        return `${k} → ${truncate(typeof v === "string" ? v : JSON.stringify(v), 160)}`;
      });
      return { title: `Update ${a.name}`, details, input: { agent: String(a._id), changes } };
    },
    execute: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      const merged = agentInputSchema.parse(mergeAgent(a, i.changes as Partial<AgentInput>));
      await requireRepos(newRepos(a, merged));
      await updateAgent(a._id, merged);
      return { text: `Updated ${a.name}.` };
    },
  },
  {
    kind: "write",
    name: "open_setup_prs",
    description:
      "Open a pull request in each of the agent's repos (or the listed ones) adding the Wisdom Ops workflow. Requires the server's GitHub token to have write access. Reuses an existing open PR.",
    input_schema: { type: "object", properties: { agent: { type: "string" }, repos: { type: "array", items: { type: "string" } } } },
    label: () => "Requested setup PRs",
    preview: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      if (a.mode === "poll") throw new Error(`${a.name} uses poll mode; no workflow is needed.`);
      const want = Array.isArray(i.repos) && i.repos.length ? i.repos.map(String) : a.repos.map((r) => r.fullName);
      const missing = want.filter((r) => !findRepoConfig(a, r));
      if (missing.length) throw new Error(`Not on this agent: ${missing.join(", ")}. Add them with update_agent first.`);
      return {
        title: `Open setup PRs for ${a.name}`,
        details: [...want.map((r) => `${r}: add .github/workflows/wisdom-ops.yml`), "Each repo still needs the WISDOM_TOKEN secret"],
        input: { agent: String(a._id), repos: want },
      };
    },
    execute: async (ctx, i) => {
      const a = await resolveAgent(ctx, i.agent);
      const lines: string[] = [];
      for (const r of (i.repos as string[]) ?? []) {
        const cfg = findRepoConfig(a, r);
        if (!cfg) continue;
        try {
          const pr = await github.openWorkflowPr(cfg.fullName, workflowYaml(a, cfg, env.appBaseUrl));
          lines.push(`${cfg.fullName}: ${pr.reused ? "existing" : "opened"} PR #${pr.number} ${pr.url}`);
        } catch (e) {
          lines.push(`${cfg.fullName}: failed (${e instanceof Error ? e.message.slice(0, 160) : e})`);
        }
      }
      return { text: lines.join("\n") };
    },
  },
];

export const TOOLS: Tool[] = [...readTools, ...writeTools];
export const TOOL_MAP = new Map(TOOLS.map((t) => [t.name, t]));

/** Tool definitions as sent to Claude. Write tools are marked so the model knows they need confirmation. */
export function toolDefinitions() {
  return TOOLS.map((t) => ({
    name: t.name,
    description: t.kind === "write" ? `${t.description} [ACTION: the user must confirm before it runs]` : t.description,
    input_schema: t.input_schema,
  }));
}

export const inputRecord = z.record(z.string(), z.unknown());
