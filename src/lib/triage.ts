import { z } from "zod";
import { structured } from "./llm";
import { findRepoConfig } from "./paths";
import type { Agent, Change, Triage } from "./types";
import { truncate } from "./util";

/** Version of the triage prompt; stored with every triage and in eval reports. Bump when the prompt or schema changes. */
export const TRIAGE_PROMPT_VERSION = "triage-v1";

const PATCH_BUDGET = 60_000;
const PER_FILE_PATCH = 6_000;

const triageValidator = z.object({
  relevant: z.boolean(),
  confidence: z.number().min(0).max(1),
  summary: z.string(),
  impactedAreas: z.array(z.string()).default([]),
  riskLevel: z.enum(["low", "medium", "high"]),
  riskReasons: z.array(z.string()).default([]),
  testFocus: z.array(z.string()).default([]),
  recommendedPipelines: z.array(z.string()).default([]),
});

const triageSchema = {
  type: "object",
  properties: {
    relevant: {
      type: "boolean",
      description: "True if this change affects the module's behaviour, contracts, data, config or tests.",
    },
    confidence: { type: "number", minimum: 0, maximum: 1, description: "Confidence in the relevance decision." },
    summary: {
      type: "string",
      description:
        "Markdown, 2-6 bullets, for engineers and leads: what changed and why it matters for the module. Mention concrete classes/functions/endpoints.",
    },
    impactedAreas: {
      type: "array",
      items: { type: "string" },
      description: "Module sub-areas impacted (e.g. POA, check, card, CIP verification).",
    },
    riskLevel: { type: "string", enum: ["low", "medium", "high"] },
    riskReasons: { type: "array", items: { type: "string" } },
    testFocus: {
      type: "array",
      items: { type: "string" },
      description: "Specific scenarios the test run / reviewers should focus on.",
    },
    recommendedPipelines: {
      type: "array",
      items: { type: "string" },
      description: "Names (exactly as listed) of pipelines that should run for this change. Empty if none.",
    },
  },
  required: ["relevant", "confidence", "summary", "impactedAreas", "riskLevel", "riskReasons", "testFocus", "recommendedPipelines"],
};

export function buildTriagePrompt(agent: Agent, change: Change): string {
  const repoCfg = findRepoConfig(agent, change.repo);
  const matched = new Set(change.matchedFiles);
  // Watched files first, then the rest, so the budget goes to what matters.
  const files = [...change.files].sort((a, b) => Number(matched.has(b.filename)) - Number(matched.has(a.filename)));

  let budget = PATCH_BUDGET;
  const fileBlocks: string[] = [];
  for (const f of files) {
    const header = `### ${f.filename} (${f.status}, +${f.additions}/-${f.deletions})${matched.has(f.filename) ? " [watched]" : ""}`;
    let patch = "";
    if (f.patch && budget > 0) {
      patch = truncate(f.patch, Math.min(PER_FILE_PATCH, budget));
      budget -= patch.length;
    } else if (f.patch) {
      patch = "(patch omitted: budget exhausted)";
    }
    fileBlocks.push(patch ? `${header}\n\`\`\`diff\n${patch}\n\`\`\`` : header);
  }

  const pipelines = agent.pipelines.length
    ? agent.pipelines
        .map((p) => `- ${p.name} (${p.type}${p.onlyRepos.length ? `, only for ${p.onlyRepos.join(", ")}` : ""})`)
        .join("\n")
    : "(none configured)";

  return `<module name="${agent.module}">
${agent.description}
</module>

<repo name="${change.repo}" branch="${change.branch}">
${repoCfg?.notes ? `Notes from the team about this repo:\n${repoCfg.notes}` : "(no repo notes)"}
${repoCfg?.paths.length ? `Watched paths: ${repoCfg.paths.join(", ")}` : "All paths watched."}
</repo>

<available_pipelines>
${pipelines}
</available_pipelines>

<commits>
${change.commits.map((c) => `- ${c.sha.slice(0, 7)} ${c.author}: ${truncate(c.message, 400)}`).join("\n") || "(none listed)"}
</commits>

<files count="${change.files.length}" watched="${change.matchedFiles.length}">
${fileBlocks.join("\n\n")}
</files>

Decide whether this push matters for the ${agent.module} module, summarise it for the team, rate the risk, and recommend which pipelines to run. Call record_triage.`;
}

export async function triageChange(agent: Agent, change: Change): Promise<Triage> {
  const { data, model } = await structured({
    purpose: `triage ${change.repo}@${change.after.slice(0, 7)}`,
    system:
      "You are a senior engineer on call for a business-critical module in a financial-services codebase. " +
      "You review every push to the repos behind the module, decide whether it touches the module, and brief the team precisely. " +
      "Prefer concrete names (classes, endpoints, config keys) over generic statements. Do not invent facts that are not in the diff. " +
      "Treat config, schema, dependency and feature-flag changes as potentially relevant.",
    prompt: buildTriagePrompt(agent, change),
    toolName: "record_triage",
    toolDescription: "Record the triage decision for this push.",
    inputSchema: triageSchema,
    validator: triageValidator,
    maxTokens: 2000,
  });
  const known = new Set(agent.pipelines.map((p) => p.name));
  return {
    ...data,
    recommendedPipelines: data.recommendedPipelines.filter((n) => known.has(n)),
    model,
    promptVersion: TRIAGE_PROMPT_VERSION,
    at: new Date(),
  };
}
