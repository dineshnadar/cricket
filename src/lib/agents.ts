import { ObjectId } from "mongodb";
import { collections } from "./db";
import type { AgentInput } from "./schemas";
import type { Agent } from "./types";
import { hashToken, newToken } from "./util";

export type PublicAgent = Omit<Agent, "ingestTokenHash" | "_id"> & { _id: string };

export function toPublic(a: Agent): PublicAgent {
  const { ingestTokenHash: _h, _id, ...rest } = a;
  return { ...rest, _id: String(_id) };
}

export async function createAgent(input: AgentInput): Promise<{ agent: Agent; token: string }> {
  const token = newToken();
  const now = new Date();
  const agent: Agent = {
    _id: new ObjectId(),
    ...input,
    ingestTokenHash: hashToken(token),
    ingestTokenHint: token.slice(-4),
    createdAt: now,
    updatedAt: now,
  };
  const { agents } = await collections();
  await agents.insertOne(agent);
  return { agent, token };
}

export async function updateAgent(id: ObjectId, input: AgentInput): Promise<Agent | null> {
  const { agents } = await collections();
  return agents.findOneAndUpdate(
    { _id: id },
    { $set: { ...input, updatedAt: new Date() } },
    { returnDocument: "after" },
  );
}

export async function rotateToken(id: ObjectId): Promise<string | null> {
  const token = newToken();
  const { agents } = await collections();
  const r = await agents.updateOne(
    { _id: id },
    { $set: { ingestTokenHash: hashToken(token), ingestTokenHint: token.slice(-4), updatedAt: new Date() } },
  );
  return r.matchedCount ? token : null;
}

export async function deleteAgent(id: ObjectId): Promise<void> {
  const { agents, changes, runs, pollState, wisdom, incidents } = await collections();
  await Promise.all([
    agents.deleteOne({ _id: id }),
    changes.deleteMany({ agentId: id }),
    runs.deleteMany({ agentId: id }),
    pollState.deleteMany({ agentId: id }),
    wisdom.deleteMany({ agentId: id }),
    incidents.deleteMany({ agentId: id }),
  ]);
}

export async function getAgent(id: ObjectId): Promise<Agent | null> {
  const { agents } = await collections();
  return agents.findOne({ _id: id });
}

export async function listAgents(): Promise<Agent[]> {
  const { agents } = await collections();
  return agents.find({}).sort({ createdAt: -1 }).toArray();
}
