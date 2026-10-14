import { ObjectId } from "mongodb";
import { collections } from "./db";
import { env } from "./env";
import * as github from "./github";
import * as jenkins from "./jenkins";
import * as teamcity from "./teamcity";
import { notifyAgent, runMessage } from "./notify";
import type { Agent, Change, PipelineConfig, PipelineRun, RunStatus } from "./types";
import { errMsg } from "./util";

/** Which configured pipelines should run for this change. */
export function selectPipelines(agent: Agent, change: Change): PipelineConfig[] {
  const recommended = new Set(change.triage?.recommendedPipelines ?? []);
  return agent.pipelines.filter((p) => {
    if (p.onlyRepos.length && !p.onlyRepos.some((r) => r.toLowerCase() === change.repo.toLowerCase())) return false;
    return p.runWhen === "always" || recommended.has(p.name);
  });
}

export function changeParams(agent: Agent, change: Change, p: PipelineConfig): Record<string, string> {
  return {
    ...p.params,
    WISDOM_CHANGE_ID: String(change._id),
    WISDOM_AGENT: agent.name,
    WISDOM_REPO: change.repo,
    WISDOM_BRANCH: change.branch,
    WISDOM_SHA: change.after,
    WISDOM_RISK: change.triage?.riskLevel ?? "",
  };
}

export async function triggerPipeline(agent: Agent, change: Change, p: PipelineConfig): Promise<PipelineRun> {
  const params = changeParams(agent, change, p);
  const run: PipelineRun = {
    _id: new ObjectId(),
    agentId: agent._id,
    changeId: change._id,
    pipelineName: p.name,
    type: p.type,
    status: "queued",
    params,
    triggeredAt: new Date(),
  };
  try {
    if (env.dryRun) {
      run.status = "success";
      run.error = "dry-run: not triggered";
      run.finishedAt = new Date();
      run.notified = true;
    } else if (p.type === "jenkins") {
      run.queueUrl = await jenkins.triggerBuild(p.jobPath!, params);
    } else if (p.type === "teamcity") {
      // ref: "$CHANGE_BRANCH" builds the pushed branch; a literal builds that branch; empty = config default.
      const branch = p.ref === "$CHANGE_BRANCH" ? change.branch : p.ref || undefined;
      const b = await teamcity.triggerBuild(p.buildTypeId!, params, branch, p.paramPrefix ?? "env.");
      run.tcBuildId = b.id;
      run.buildUrl = b.webUrl;
    } else {
      // GitHub workflow_dispatch inputs are limited to 10 (25 on newer servers) and must be declared;
      // send only the static params plus the change id/sha which the workflow can declare.
      const inputs: Record<string, string> = {
        ...p.params,
        wisdom_change_id: String(change._id),
        wisdom_sha: change.after,
      };
      run.ghRepo = p.repo;
      run.ghWorkflow = p.workflow;
      run.ghRef = p.ref || "main";
      run.params = inputs;
      await github.dispatchWorkflow(p.repo!, p.workflow!, p.ref || "main", inputs);
    }
  } catch (e) {
    run.status = "error";
    run.error = errMsg(e);
    run.finishedAt = new Date();
  }
  const { runs } = await collections();
  await runs.insertOne(run);
  return run;
}

function jenkinsResult(result: string | null): RunStatus {
  switch (result) {
    case "SUCCESS":
      return "success";
    case "UNSTABLE":
      return "unstable";
    case "ABORTED":
    case "NOT_BUILT":
      return "aborted";
    default:
      return "failure";
  }
}

function teamcityResult(b: teamcity.TcBuild): RunStatus {
  if (b.canceledInfo) return "aborted";
  if (b.status === "SUCCESS") return "success";
  if (b.status === "UNKNOWN") return "aborted";
  return "failure";
}

function ghResult(conclusion: string | null): RunStatus {
  switch (conclusion) {
    case "success":
      return "success";
    case "cancelled":
    case "skipped":
      return "aborted";
    case "neutral":
      return "unstable";
    default:
      return "failure";
  }
}

const STALE_MS = 24 * 3600_000;

/** Poll one in-flight run and persist the new state. Returns the updated run. */
export async function refreshRun(run: PipelineRun): Promise<PipelineRun> {
  const update: Partial<PipelineRun> = { lastCheckedAt: new Date() };
  try {
    if (run.type === "jenkins") {
      if (!run.buildUrl && run.queueUrl) {
        const q = await jenkins.queueItem(run.queueUrl);
        if (q.cancelled) {
          update.status = "aborted";
          update.finishedAt = new Date();
        } else if (q.buildUrl) {
          update.buildUrl = q.buildUrl;
          update.buildNumber = q.buildNumber;
          update.status = "running";
        }
      }
      const buildUrl = update.buildUrl ?? run.buildUrl;
      if (buildUrl && update.status !== "aborted") {
        const b = await jenkins.build(buildUrl);
        if (b.building) update.status = "running";
        else {
          update.status = jenkinsResult(b.result);
          update.finishedAt = new Date();
          update.tests = await jenkins.testReport(buildUrl);
        }
      }
    } else if (run.type === "teamcity" && run.tcBuildId) {
      const b = await teamcity.getBuild(run.tcBuildId);
      if (b.webUrl) update.buildUrl = b.webUrl;
      if (b.number) update.buildNumber = Number(b.number) || undefined;
      if (b.state === "finished") {
        update.status = teamcityResult(b);
        update.finishedAt = new Date();
        update.tests = teamcity.testsOf(b);
      } else {
        update.status = b.state === "running" ? "running" : "queued";
        // Live counts while running are useful in the UI too.
        const t = teamcity.testsOf(b);
        if (t) update.tests = t;
      }
    } else if (run.ghRepo && run.ghWorkflow) {
      let runId = run.ghRunId;
      if (!runId) {
        const found = await github.findDispatchedRun(
          run.ghRepo,
          run.ghWorkflow,
          run.ghRef || "main",
          run.triggeredAt,
        );
        if (found) {
          runId = found.id;
          update.ghRunId = found.id;
          update.buildUrl = found.html_url;
          update.buildNumber = found.run_number;
        }
      }
      if (runId) {
        const r = await github.getWorkflowRun(run.ghRepo, runId);
        if (r.status === "completed") {
          update.status = ghResult(r.conclusion);
          update.finishedAt = new Date();
        } else update.status = r.status === "queued" ? "queued" : "running";
      }
    }
  } catch (e) {
    update.error = errMsg(e);
  }
  if (!update.finishedAt && Date.now() - run.triggeredAt.getTime() > STALE_MS) {
    update.status = "error";
    update.error = "Gave up tracking after 24h";
    update.finishedAt = new Date();
  }
  const { runs } = await collections();
  await runs.updateOne({ _id: run._id }, { $set: update });
  return { ...run, ...update };
}

const FINISHED: RunStatus[] = ["success", "failure", "unstable", "aborted", "error"];

/** Worker job: refresh all in-flight runs, and notify on completion per agent policy. */
export async function refreshActiveRuns(): Promise<number> {
  const { runs, agents, changes } = await collections();
  const active = await runs.find({ status: { $in: ["queued", "running"] } }).limit(200).toArray();
  for (const run of active) {
    const updated = await refreshRun(run);
    if (FINISHED.includes(updated.status) && !updated.notified) {
      await maybeNotifyRun(updated, agents, changes);
    }
  }
  // Runs that finished at trigger time (errors) still need a notification decision.
  const unnotified = await runs.find({ status: { $in: FINISHED }, notified: { $ne: true } }).limit(100).toArray();
  for (const run of unnotified) await maybeNotifyRun(run, agents, changes);
  return active.length;
}

async function maybeNotifyRun(
  run: PipelineRun,
  agents: Awaited<ReturnType<typeof collections>>["agents"],
  changes: Awaited<ReturnType<typeof collections>>["changes"],
) {
  const { runs } = await collections();
  const agent = await agents.findOne({ _id: run.agentId });
  const policy = agent?.notifyOnPipeline ?? "never";
  const isFailure = run.status !== "success";
  if (agent && (policy === "always" || (policy === "failure" && isFailure))) {
    const change = await changes.findOne({ _id: run.changeId });
    await notifyAgent(agent, runMessage(agent, run, change));
  }
  await runs.updateOne({ _id: run._id }, { $set: { notified: true } });
}
