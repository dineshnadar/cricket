/**
 * Background worker. Run alongside the web app: `npm run worker`.
 *  - processes the change queue (triage -> pipelines -> notify)
 *  - tracks Jenkins / GitHub Actions runs and notifies on completion
 *  - polls repos for agents in poll/both mode
 *  - polls ServiceNow and analyses new incidents (Splunk + recent changes)
 *  - refreshes module wisdom on a schedule
 */
import { loadEnvFiles } from "../lib/load-env";
loadEnvFiles();
import { processQueue } from "../lib/changes";
import { closeDb, collections } from "../lib/db";
import { env, integrationStatus } from "../lib/env";
import { analyzeNewIncidents, pollServiceNow } from "../lib/incidents";
import { refreshActiveRuns } from "../lib/pipelines";
import { pollDueAgents } from "../lib/poller";
import { errMsg } from "../lib/util";
import { refreshDueWisdom } from "../lib/wisdom";

type Job = { name: string; everyMs: number; run: () => Promise<unknown>; lastAt: number; busy: boolean };

const jobs: Job[] = [
  { name: "changes", everyMs: env.workerTickSeconds * 1000, run: () => processQueue(), lastAt: 0, busy: false },
  { name: "pipelines", everyMs: env.pipelineRefreshSeconds * 1000, run: () => refreshActiveRuns(), lastAt: 0, busy: false },
  { name: "poller", everyMs: 60_000, run: () => pollDueAgents(), lastAt: 0, busy: false },
  {
    name: "servicenow",
    everyMs: 60_000,
    run: async () => {
      if (integrationStatus().servicenow) await pollServiceNow();
      return analyzeNewIncidents();
    },
    lastAt: 0,
    busy: false,
  },
  { name: "wisdom", everyMs: 15 * 60_000, run: () => refreshDueWisdom(env.wisdomRefreshHours), lastAt: 0, busy: false },
];

async function runJob(j: Job) {
  if (j.busy) return;
  j.busy = true;
  j.lastAt = Date.now();
  try {
    const r = await j.run();
    if (typeof r === "number" && r > 0) console.log(`[worker] ${j.name}: ${r}`);
  } catch (e) {
    console.error(`[worker] ${j.name} failed: ${errMsg(e)}`);
  } finally {
    j.busy = false;
  }
}

async function heartbeat() {
  const { meta } = await collections();
  await meta.updateOne(
    { _id: "worker:heartbeat" },
    { $set: { value: new Date().toISOString(), updatedAt: new Date() } },
    { upsert: true },
  );
}

async function main() {
  const once = process.argv.includes("--once");
  console.log("[worker] starting", integrationStatus());
  await collections(); // connect + indexes

  if (once) {
    for (const j of jobs) await runJob(j);
    await heartbeat();
    await closeDb();
    return;
  }

  let stopping = false;
  const stop = async () => {
    if (stopping) return;
    stopping = true;
    console.log("[worker] shutting down");
    const deadline = Date.now() + 30_000;
    while (jobs.some((j) => j.busy) && Date.now() < deadline) await new Promise((r) => setTimeout(r, 250));
    await closeDb();
    process.exit(0);
  };
  process.on("SIGINT", stop);
  process.on("SIGTERM", stop);

  while (!stopping) {
    const now = Date.now();
    for (const j of jobs) if (now - j.lastAt >= j.everyMs) void runJob(j);
    await heartbeat().catch(() => {});
    await new Promise((r) => setTimeout(r, 5_000));
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
