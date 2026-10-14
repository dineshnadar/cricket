import { ObjectId } from "mongodb";
import { recordChange } from "./changes";
import { collections } from "./db";
import * as github from "./github";
import type { Agent } from "./types";
import { errMsg } from "./util";

/**
 * Central poller for agents in "poll" or "both" mode. For repos where the
 * per-repo GitHub Action can't be installed, this compares the last seen head
 * with the current head and records the difference as a change.
 * Dedupe on (agent, repo, head sha) means "both" mode never double-processes.
 */
export async function pollAgent(agent: Agent): Promise<{ checked: number; recorded: number; errors: string[] }> {
  const { pollState } = await collections();
  let checked = 0;
  let recorded = 0;
  const errors: string[] = [];

  for (const repo of agent.repos) {
    let branches = repo.branches.filter((b) => !/[*?[\]{}]/.test(b)); // globs can't be polled directly
    try {
      if (!branches.length) branches = [(await github.getRepo(repo.fullName)).defaultBranch];
    } catch (e) {
      errors.push(`${repo.fullName}: ${errMsg(e)}`);
      continue;
    }
    for (const branch of branches) {
      checked++;
      try {
        const head = await github.headSha(repo.fullName, branch);
        const state = await pollState.findOne({ agentId: agent._id, repo: repo.fullName, branch });
        if (!state) {
          // First sight: set a baseline instead of flooding with history.
          await pollState.insertOne({
            _id: new ObjectId(),
            agentId: agent._id,
            repo: repo.fullName,
            branch,
            lastSha: head,
            lastPolledAt: new Date(),
          });
          continue;
        }
        if (state.lastSha !== head) {
          const diff = await github.compare(repo.fullName, state.lastSha, head);
          const rec = await recordChange({
            agent,
            repo: repo.fullName,
            branch,
            before: state.lastSha,
            after: head,
            compareUrl: diff.url,
            source: "poll",
            commits: diff.commits,
            files: diff.files,
          });
          if (rec) recorded++;
        }
        await pollState.updateOne({ _id: state._id }, { $set: { lastSha: head, lastPolledAt: new Date() } });
      } catch (e) {
        errors.push(`${repo.fullName}@${branch}: ${errMsg(e)}`);
      }
    }
  }
  return { checked, recorded, errors };
}

/** Worker job: poll every agent whose interval has elapsed. */
export async function pollDueAgents(): Promise<number> {
  const { agents, meta } = await collections();
  const list = await agents.find({ enabled: true, mode: { $in: ["poll", "both"] } }).toArray();
  let polled = 0;
  for (const agent of list) {
    const key = `poll:${agent._id}`;
    const last = await meta.findOne({ _id: key });
    const lastAt = last ? new Date(last.value as string).getTime() : 0;
    if (Date.now() - lastAt < agent.pollingIntervalMinutes * 60_000) continue;
    await meta.updateOne({ _id: key }, { $set: { value: new Date().toISOString(), updatedAt: new Date() } }, { upsert: true });
    const r = await pollAgent(agent);
    if (r.errors.length) console.warn(`[poll] ${agent.name}: ${r.errors.join(" | ")}`);
    polled++;
  }
  return polled;
}
