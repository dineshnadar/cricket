import { ObjectId } from "mongodb";
import { z } from "zod";
import { collections } from "./db";
import { env } from "./env";
import * as github from "./github";
import { freeText, structured } from "./llm";
import { matchWatchedFiles } from "./paths";
import type { Agent, RepoWisdom, Wisdom } from "./types";
import { errMsg, shortSha, truncate } from "./util";

const repoValidator = z.object({
  purpose: z.string(),
  keyComponents: z.array(z.string()).default([]),
  recentActivity: z.string(),
  hotspots: z.array(z.string()).default([]),
  risks: z.array(z.string()).default([]),
  testingNotes: z.string().default(""),
});

const repoSchema = {
  type: "object",
  properties: {
    purpose: { type: "string", description: "What this repo does for the module, 1-3 sentences." },
    keyComponents: {
      type: "array",
      items: { type: "string" },
      description: "Key packages/classes/services relevant to the module, each with a few words of context.",
    },
    recentActivity: { type: "string", description: "Markdown: what has been changing recently and who is active." },
    hotspots: { type: "array", items: { type: "string" }, description: "Files/areas with frequent or risky change." },
    risks: { type: "array", items: { type: "string" }, description: "Operational or regression risks worth watching." },
    testingNotes: { type: "string", description: "How changes here should be tested; gaps noticed." },
  },
  required: ["purpose", "keyComponents", "recentActivity", "hotspots", "risks", "testingNotes"],
};

const SYSTEM =
  "You maintain the living knowledge base ('wisdom') for a business-critical module built across many repos by different developers. " +
  "Write for an engineer joining the on-call rotation tomorrow. Be specific and factual; only state what the material supports.";

async function summarizeRepo(agent: Agent, repoIdx: number): Promise<RepoWisdom> {
  const repo = agent.repos[repoIdx];
  const info = await github.getRepo(repo.fullName);
  const branch = repo.branches.find((b) => !/[*?[\]{}]/.test(b)) ?? info.defaultBranch;
  const [readme, commits, files] = await Promise.all([
    github.readme(repo.fullName),
    github.recentCommits(repo.fullName, branch, 40),
    github.tree(repo.fullName, branch).catch(() => [] as string[]),
  ]);
  const watched = matchWatchedFiles(repo, files).slice(0, 400);

  const { changes } = await collections();
  const recent = await changes
    .find({ agentId: agent._id, repo: repo.fullName, "triage.relevant": true })
    .sort({ receivedAt: -1 })
    .limit(15)
    .toArray();

  const prompt = `<module name="${agent.module}">${agent.description}</module>
<repo name="${repo.fullName}" branch="${branch}" description="${info.description ?? ""}">
${repo.notes ? `Team notes: ${repo.notes}` : ""}
</repo>
<readme>${truncate(readme, 12_000)}</readme>
<watched_files count="${watched.length}">
${watched.join("\n")}
</watched_files>
<recent_commits>
${commits.map((c) => `- ${shortSha(c.sha)} ${c.timestamp?.slice(0, 10) ?? ""} ${c.author}: ${truncate(c.message.split("\n")[0], 160)}`).join("\n")}
</recent_commits>
<recent_triaged_changes>
${recent.map((c) => `- ${c.receivedAt.toISOString().slice(0, 10)} ${shortSha(c.after)} [${c.triage?.riskLevel}] ${truncate(c.triage?.summary ?? "", 600)}`).join("\n") || "(none yet)"}
</recent_triaged_changes>

Summarise this repo's role in the ${agent.module} module. Call record_repo_wisdom.`;

  const { data } = await structured({
    purpose: `wisdom ${repo.fullName}`,
    system: SYSTEM,
    prompt,
    toolName: "record_repo_wisdom",
    toolDescription: "Record the knowledge summary for one repo.",
    inputSchema: repoSchema,
    validator: repoValidator,
    maxTokens: 2500,
  });
  return { repo: repo.fullName, headSha: commits[0]?.sha, ...data };
}

export async function refreshWisdom(agent: Agent): Promise<Wisdom> {
  const repos: RepoWisdom[] = [];
  for (let i = 0; i < agent.repos.length; i++) {
    try {
      repos.push(await summarizeRepo(agent, i));
    } catch (e) {
      repos.push({
        repo: agent.repos[i].fullName,
        purpose: `Could not summarise: ${errMsg(e)}`,
        keyComponents: [],
        recentActivity: "",
        hotspots: [],
        risks: [],
        testingNotes: "",
      });
    }
  }
  const moduleSummary = await freeText({
    purpose: `wisdom overview ${agent.name}`,
    system: SYSTEM,
    prompt: `<module name="${agent.module}">${agent.description}</module>
<repo_summaries>
${JSON.stringify(repos, null, 1)}
</repo_summaries>

Write the module overview in markdown (max ~400 words): how the repos fit together, where the risk is concentrated right now, and what someone on call should watch this week. Use short sections with headings.`,
    maxTokens: 1500,
  });
  const doc: Wisdom = {
    _id: new ObjectId(),
    agentId: agent._id,
    moduleSummary,
    repos,
    generatedAt: new Date(),
    model: env.anthropicModel,
  };
  const { wisdom } = await collections();
  await wisdom.insertOne(doc);
  return doc;
}

export async function latestWisdom(agentId: ObjectId): Promise<Wisdom | null> {
  const { wisdom } = await collections();
  return wisdom.findOne({ agentId }, { sort: { generatedAt: -1 } });
}

/** Worker job. */
export async function refreshDueWisdom(hours: number): Promise<number> {
  const { agents } = await collections();
  const list = await agents.find({ enabled: true }).toArray();
  let n = 0;
  for (const a of list) {
    const last = await latestWisdom(a._id);
    if (last && Date.now() - last.generatedAt.getTime() < hours * 3600_000) continue;
    try {
      await refreshWisdom(a);
      n++;
    } catch (e) {
      console.warn(`[wisdom] ${a.name}: ${errMsg(e)}`);
    }
  }
  return n;
}

/** "Ask the module" — grounded Q&A over wisdom, recent changes, runs and incidents. */
export async function ask(agent: Agent, question: string): Promise<string> {
  const { changes, incidents, runs } = await collections();
  const [w, recent, incs, failed] = await Promise.all([
    latestWisdom(agent._id),
    changes.find({ agentId: agent._id, "triage.relevant": true }).sort({ receivedAt: -1 }).limit(40).toArray(),
    incidents.find({ agentId: agent._id }).sort({ openedAt: -1 }).limit(15).toArray(),
    runs
      .find({ agentId: agent._id, status: { $in: ["failure", "unstable", "error"] } })
      .sort({ triggeredAt: -1 })
      .limit(15)
      .toArray(),
  ]);
  const prompt = `<module name="${agent.module}">${agent.description}</module>
<wisdom generated="${w?.generatedAt.toISOString() ?? "never"}">
${w ? w.moduleSummary + "\n\n" + JSON.stringify(w.repos, null, 1) : "(not generated yet)"}
</wisdom>
<recent_relevant_changes>
${recent.map((c) => `- [change:${c._id}] ${c.receivedAt.toISOString()} ${c.repo}@${c.branch} ${shortSha(c.after)} by ${[...new Set(c.commits.map((x) => x.author))].join(", ")} risk=${c.triage?.riskLevel}\n  ${truncate(c.triage?.summary ?? "", 700).replace(/\n/g, "\n  ")}`).join("\n")}
</recent_relevant_changes>
<recent_failed_runs>
${failed.map((r) => `- ${r.triggeredAt.toISOString()} ${r.pipelineName} ${r.status} change:${r.changeId} ${r.tests ? `${r.tests.failed}/${r.tests.total} failed` : ""}`).join("\n") || "(none)"}
</recent_failed_runs>
<recent_incidents>
${incs.map((i) => `- [incident:${i._id}] ${i.number} ${i.openedAt.toISOString()} ${i.shortDescription}\n  ${truncate(i.analysis?.summary ?? "", 500)}`).join("\n") || "(none)"}
</recent_incidents>

Question: ${question}`;
  return freeText({
    purpose: `ask ${agent.name}`,
    system:
      SYSTEM +
      " Answer the question using only the material provided. Cite changes as [change:<id>] and incidents as [incident:<id>] when you rely on them. If the material doesn't answer it, say what's missing.",
    prompt,
    maxTokens: 2000,
  });
}
