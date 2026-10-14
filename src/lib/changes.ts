import { MongoServerError, ObjectId } from "mongodb";
import { collections } from "./db";
import { changeMessage, notifyAgent } from "./notify";
import { findRepoConfig, matchWatchedFiles } from "./paths";
import { selectPipelines, triggerPipeline } from "./pipelines";
import { triageChange } from "./triage";
import type { Agent, Change, ChangeCommit, ChangeFile } from "./types";
import { errMsg, truncate } from "./util";

const MAX_ATTEMPTS = 3;
const MAX_PATCH = 20_000;

export interface NewChange {
  agent: Agent;
  repo: string;
  branch: string;
  before: string;
  after: string;
  pusher?: string;
  compareUrl?: string;
  source: Change["source"];
  commits: ChangeCommit[];
  files: ChangeFile[];
}

/** Insert a change (deduped on agent+repo+head sha). Returns null if it already existed. */
export async function recordChange(n: NewChange): Promise<Change | null> {
  const repoCfg = findRepoConfig(n.agent, n.repo);
  const files = n.files.map((f) => ({ ...f, patch: f.patch ? truncate(f.patch, MAX_PATCH) : undefined }));
  const matchedFiles = repoCfg ? matchWatchedFiles(repoCfg, files.map((f) => f.filename)) : [];
  const now = new Date();
  const change: Change = {
    _id: new ObjectId(),
    agentId: n.agent._id,
    repo: n.repo,
    branch: n.branch,
    before: n.before,
    after: n.after,
    pusher: n.pusher,
    compareUrl: n.compareUrl,
    source: n.source,
    commits: n.commits.slice(0, 250),
    files,
    matchedFiles,
    status: "received",
    attempts: 0,
    nextAttemptAt: now,
    pipelineRunIds: [],
    notifications: [],
    receivedAt: now,
    updatedAt: now,
  };
  const { changes } = await collections();
  try {
    await changes.insertOne(change);
    return change;
  } catch (e) {
    if (e instanceof MongoServerError && e.code === 11000) return null;
    throw e;
  }
}

/**
 * Full pipeline for one change:
 *   path filter -> Claude triage -> (if relevant) trigger pipelines -> notify.
 */
export async function processChange(change: Change, agent: Agent): Promise<Change> {
  const { changes } = await collections();
  const set = async (patch: Partial<Change>) => {
    Object.assign(change, patch, { updatedAt: new Date() });
    await changes.updateOne({ _id: change._id }, { $set: { ...patch, updatedAt: change.updatedAt } });
  };

  const repoCfg = findRepoConfig(agent, change.repo);
  if (!repoCfg) {
    await set({ status: "filtered", error: `${change.repo} is not configured on agent ${agent.name}` });
    return change;
  }
  if (!change.matchedFiles.length && change.files.length) {
    await set({ status: "filtered" });
    return change;
  }

  const triage = await triageChange(agent, change);
  await set({ triage });

  if (!triage.relevant || triage.confidence < agent.relevanceThreshold) {
    await set({ status: "not-relevant" });
    return change;
  }

  const selected = selectPipelines(agent, change);
  const runs = [];
  for (const p of selected) runs.push(await triggerPipeline(agent, change, p));
  await set({ pipelineRunIds: runs.map((r) => r._id) });

  const notifications = await notifyAgent(agent, changeMessage(agent, change, runs));
  await set({ notifications, status: "actioned", error: undefined });
  return change;
}

/** Worker job: claim and process due changes one at a time. */
export async function processQueue(limit = 20): Promise<number> {
  const { changes, agents } = await collections();
  let done = 0;
  for (; done < limit; done++) {
    const claimed = await changes.findOneAndUpdate(
      { status: "received", nextAttemptAt: { $lte: new Date() } },
      { $set: { status: "processing", updatedAt: new Date() }, $inc: { attempts: 1 } },
      { sort: { receivedAt: 1 }, returnDocument: "after" },
    );
    if (!claimed) break;
    const agent = await agents.findOne({ _id: claimed.agentId });
    if (!agent || !agent.enabled) {
      await changes.updateOne(
        { _id: claimed._id },
        { $set: { status: "filtered", error: "agent missing or disabled", updatedAt: new Date() } },
      );
      continue;
    }
    try {
      await processChange(claimed, agent);
    } catch (e) {
      const retry = claimed.attempts < MAX_ATTEMPTS;
      await changes.updateOne(
        { _id: claimed._id },
        {
          $set: {
            status: retry ? "received" : "error",
            error: errMsg(e),
            nextAttemptAt: new Date(Date.now() + 60_000 * 2 ** claimed.attempts),
            updatedAt: new Date(),
          },
        },
      );
      console.error(`[changes] ${claimed._id} failed (attempt ${claimed.attempts}): ${errMsg(e)}`);
    }
  }
  // Recover changes stuck in "processing" (worker crashed mid-flight).
  await changes.updateMany(
    { status: "processing", updatedAt: { $lt: new Date(Date.now() - 15 * 60_000) } },
    { $set: { status: "received", nextAttemptAt: new Date() } },
  );
  return done;
}

export async function requeueChange(id: ObjectId): Promise<boolean> {
  const { changes } = await collections();
  const r = await changes.updateOne(
    { _id: id, status: { $ne: "processing" } },
    { $set: { status: "received", attempts: 0, nextAttemptAt: new Date(), updatedAt: new Date() }, $unset: { error: "" } },
  );
  return r.modifiedCount === 1;
}
