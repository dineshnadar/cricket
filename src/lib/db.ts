import { MongoClient, ObjectId, type Db } from "mongodb";
import { env } from "./env";
import type { Agent, AuditEntry, Change, Conversation, Incident, Meta, PendingAction, PipelineRun, PollState, Wisdom, EvalLabel } from "./types";

declare global {
  // eslint-disable-next-line no-var
  var __wisdomMongo: Promise<MongoClient> | undefined;
  // eslint-disable-next-line no-var
  var __wisdomIndexes: Promise<void> | undefined;
}

function client(): Promise<MongoClient> {
  if (!globalThis.__wisdomMongo) {
    // Forget a failed connect so the next request retries (e.g. Mongo started after the app).
    globalThis.__wisdomMongo = new MongoClient(env.mongoUri, { serverSelectionTimeoutMS: 5000 }).connect().catch((e) => {
      globalThis.__wisdomMongo = undefined;
      throw e;
    });
  }
  return globalThis.__wisdomMongo;
}

export async function getDb(): Promise<Db> {
  const db = (await client()).db(env.mongoDb);
  if (!globalThis.__wisdomIndexes) {
    globalThis.__wisdomIndexes = ensureIndexes(db).catch((e) => {
      globalThis.__wisdomIndexes = undefined;
      throw e;
    });
  }
  await globalThis.__wisdomIndexes;
  return db;
}

export async function collections() {
  const db = await getDb();
  return {
    agents: db.collection<Agent>("agents"),
    changes: db.collection<Change>("changes"),
    runs: db.collection<PipelineRun>("pipeline_runs"),
    pollState: db.collection<PollState>("poll_state"),
    wisdom: db.collection<Wisdom>("wisdom"),
    incidents: db.collection<Incident>("incidents"),
    meta: db.collection<Meta>("meta"),
    conversations: db.collection<Conversation>("conversations"),
    actions: db.collection<PendingAction>("assistant_actions"),
    audit: db.collection<AuditEntry>("audit"),
    evalLabels: db.collection<EvalLabel>("eval_labels"),
  };
}

async function ensureIndexes(db: Db) {
  await Promise.all([
    db.collection("changes").createIndex({ agentId: 1, repo: 1, after: 1 }, { unique: true, name: "dedupe" }),
    db.collection("changes").createIndex({ status: 1, nextAttemptAt: 1 }),
    db.collection("changes").createIndex({ agentId: 1, receivedAt: -1 }),
    db.collection("pipeline_runs").createIndex({ status: 1 }),
    db.collection("pipeline_runs").createIndex({ changeId: 1 }),
    db.collection("pipeline_runs").createIndex({ agentId: 1, triggeredAt: -1 }),
    db.collection("poll_state").createIndex({ agentId: 1, repo: 1, branch: 1 }, { unique: true }),
    db.collection("wisdom").createIndex({ agentId: 1, generatedAt: -1 }),
    db.collection("incidents").createIndex({ agentId: 1, sysId: 1 }, { unique: true }),
    db.collection("incidents").createIndex({ status: 1 }),
    db.collection("conversations").createIndex({ user: 1, updatedAt: -1 }),
    db.collection("assistant_actions").createIndex({ conversationId: 1 }),
    db.collection("audit").createIndex({ at: -1 }),
    db.collection("eval_labels").createIndex({ kind: 1, targetId: 1 }, { unique: true }),
    db.collection("eval_labels").createIndex({ agentId: 1, kind: 1, at: -1 }),
  ]);
}

export function toId(id: string): ObjectId | null {
  return ObjectId.isValid(id) ? new ObjectId(id) : null;
}

export async function closeDb() {
  if (globalThis.__wisdomMongo) {
    const c = await globalThis.__wisdomMongo;
    await c.close();
    globalThis.__wisdomMongo = undefined;
    globalThis.__wisdomIndexes = undefined;
  }
}
