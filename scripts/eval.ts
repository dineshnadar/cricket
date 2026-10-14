/**
 * npm run eval -- [options]
 *
 * Measures Wisdom AI against the labels people gave with the "Was this right?" buttons.
 *
 *   --kind triage|incident|all   what to evaluate (default all)
 *   --stored                     score the answers stored at the time; no AI calls, free
 *                                (default: re-run the CURRENT prompt and model on the same items)
 *   --agent <id or name>         only one agent
 *   --limit <n>                  newest n labels per kind (default all)
 *   --min-recall <0..1>          exit 1 if triage recall is below this (use as a gate)
 *   --min-hit3 <0..1>            exit 1 if incident top-3 hit rate is below this
 *   --out <dir>                  where to write the report (default eval-results/)
 *
 * Re-runs call the real model through the masking gateway, so they cost tokens and need ANTHROPIC_*.
 */
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { loadEnvFiles } from "../src/lib/load-env";

loadEnvFiles();

function arg(name: string): string | undefined {
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 ? process.argv[i + 1] : undefined;
}
const flag = (name: string) => process.argv.includes(`--${name}`);

async function main() {
  const { collections, toId } = await import("../src/lib/db");
  const { env } = await import("../src/lib/env");
  const ev = await import("../src/lib/evals");

  const kind = (arg("kind") ?? "all") as "triage" | "incident" | "all";
  const stored = flag("stored");
  const limit = arg("limit") ? Number(arg("limit")) : undefined;
  const minRecall = arg("min-recall") ? Number(arg("min-recall")) : undefined;
  const minHit3 = arg("min-hit3") ? Number(arg("min-hit3")) : undefined;
  const outDir = arg("out") ?? "eval-results";

  let agentId;
  const agentArg = arg("agent");
  if (agentArg) {
    const { agents } = await collections();
    const a = (toId(agentArg) && (await agents.findOne({ _id: toId(agentArg)! }))) || (await agents.findOne({ name: agentArg }));
    if (!a) throw new Error(`No agent "${agentArg}"`);
    agentId = a._id;
  }

  const mode = stored ? "stored answers (no AI calls)" : `re-run with current prompt · model ${env.anthropicModel}`;
  const progress = (label: string) => (d: number, t: number) => process.stderr.write(`\r${label}: ${d}/${t}`);
  const startedAt = new Date();
  const md: string[] = [`# Wisdom AI eval · ${startedAt.toISOString()}`, "", `Mode: **${mode}**${agentArg ? ` · agent ${agentArg}` : ""}`, ""];
  const json: Record<string, unknown> = { startedAt, mode, stored, model: stored ? undefined : env.anthropicModel, agent: agentArg };
  let failed = false;

  if (kind === "triage" || kind === "all") {
    const r = await ev.runTriageEval({ agentId, limit, stored, onProgress: progress("triage") });
    process.stderr.write("\n");
    md.push(`## Triage (${r.promptVersion})`, "", r.metrics.n || r.metrics.errors ? ev.triageReport(r.metrics) : "_No triage labels yet. Use the “Was this triage right?” buttons on change pages._", "");
    json.triage = { promptVersion: r.promptVersion, metrics: { ...r.metrics, falseNegatives: undefined, falsePositives: undefined }, rows: r.rows };
    if (minRecall !== undefined && r.metrics.recall !== null && r.metrics.recall < minRecall) {
      md.push(`**FAILED:** recall ${Math.round(r.metrics.recall * 100)}% is below the minimum ${Math.round(minRecall * 100)}%.`, "");
      failed = true;
    }
  }

  if (kind === "incident" || kind === "all") {
    const r = await ev.runIncidentEval({ agentId, limit, stored, onProgress: progress("incidents") });
    process.stderr.write("\n");
    md.push(`## Incident analysis (${r.promptVersion})`, "", r.metrics.n || r.metrics.errors ? ev.incidentReport(r.metrics) : "_No incident labels yet. Use the Feedback box on incident pages._", "");
    json.incident = { promptVersion: r.promptVersion, metrics: { ...r.metrics, misses: undefined }, rows: r.rows };
    if (minHit3 !== undefined && r.metrics.hitAt3 !== null && r.metrics.hitAt3 < minHit3) {
      md.push(`**FAILED:** top-3 hit rate ${Math.round(r.metrics.hitAt3 * 100)}% is below the minimum ${Math.round(minHit3 * 100)}%.`, "");
      failed = true;
    }
  }

  mkdirSync(outDir, { recursive: true });
  const stamp = startedAt.toISOString().replace(/[:.]/g, "-");
  const base = join(outDir, `eval-${stamp}${stored ? "-stored" : ""}`);
  writeFileSync(`${base}.md`, md.join("\n"));
  writeFileSync(`${base}.json`, JSON.stringify(json, null, 2));
  console.log(md.join("\n"));
  console.log(`\nSaved ${base}.md and ${base}.json`);
  process.exit(failed ? 1 : 0);
}

main().catch((e) => {
  console.error(e instanceof Error ? e.message : e);
  process.exit(2);
});
