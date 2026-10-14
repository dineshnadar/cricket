import { ObjectId } from "mongodb";
import { collections } from "../db";
import type { AuditEntry, Conversation, DisplayMessage, PendingAction, PendingActionView } from "../types";
import { errMsg } from "../util";
import { TOOL_MAP, type ToolContext, type WriteTool } from "./tools";

export async function audit(entry: Omit<AuditEntry, "_id" | "at">) {
  try {
    const { audit } = await collections();
    await audit.insertOne({ _id: new ObjectId(), at: new Date(), ...entry });
  } catch (e) {
    console.warn(`[audit] ${errMsg(e)}`);
  }
}

export async function loadConversation(id: ObjectId): Promise<Conversation | null> {
  const { conversations } = await collections();
  return conversations.findOne({ _id: id });
}

export async function createConversation(user: string, title: string, agentId?: ObjectId): Promise<Conversation> {
  const now = new Date();
  const c: Conversation = {
    _id: new ObjectId(),
    title: title.slice(0, 80),
    user,
    agentId,
    display: [],
    createdAt: now,
    updatedAt: now,
  };
  const { conversations } = await collections();
  await conversations.insertOne(c);
  return c;
}

export async function saveDisplay(id: ObjectId, display: DisplayMessage[], agentId?: ObjectId) {
  const { conversations } = await collections();
  await conversations.updateOne(
    { _id: id },
    { $set: { display, updatedAt: new Date(), ...(agentId ? { agentId } : {}) } },
  );
}

export async function listConversations(user: string, limit = 30) {
  const { conversations } = await collections();
  return conversations
    .find({ user }, { projection: { title: 1, updatedAt: 1, agentId: 1 } })
    .sort({ updatedAt: -1 })
    .limit(limit)
    .toArray();
}

export function actionView(a: PendingAction, secret?: string): PendingActionView {
  return {
    id: String(a._id),
    tool: a.tool,
    title: a.title,
    details: a.details,
    status: a.status,
    resultText: a.result,
    ...(secret ? { secret } : {}),
  };
}

export async function requestAction(
  conversationId: ObjectId,
  tool: WriteTool,
  input: Record<string, unknown>,
  ctx: ToolContext,
): Promise<PendingAction> {
  const preview = await tool.preview(ctx, input);
  const action: PendingAction = {
    _id: new ObjectId(),
    conversationId,
    tool: tool.name,
    input: preview.input,
    title: preview.title,
    details: preview.details,
    status: "pending",
    requestedBy: ctx.user,
    createdAt: new Date(),
  };
  const { actions } = await collections();
  await actions.insertOne(action);
  await audit({ user: ctx.user, conversationId, kind: "action-requested", name: tool.name, input: preview.input, ok: true });
  return action;
}

/** Confirm or cancel a pending action. Runs it on confirm. */
export async function decideAction(
  actionId: ObjectId,
  decision: "confirm" | "cancel",
  user: string,
): Promise<PendingActionView> {
  const { actions, conversations } = await collections();
  // Claim atomically so a double-click can't run it twice.
  const claimed = await actions.findOneAndUpdate(
    { _id: actionId, status: "pending" },
    { $set: { status: decision === "confirm" ? "confirmed" : "cancelled", decidedBy: user, decidedAt: new Date() } },
    { returnDocument: "after" },
  );
  if (!claimed) {
    const existing = await actions.findOne({ _id: actionId });
    if (!existing) throw new Error("Action not found");
    return actionView(existing);
  }

  let secret: string | undefined;
  let final = claimed;
  if (decision === "confirm") {
    const tool = TOOL_MAP.get(claimed.tool);
    const conv = await conversations.findOne({ _id: claimed.conversationId }, { projection: { agentId: 1 } });
    try {
      if (!tool || tool.kind !== "write") throw new Error(`Unknown action ${claimed.tool}`);
      const out = await tool.execute({ user, defaultAgentId: conv?.agentId }, claimed.input);
      secret = out.secret;
      final = { ...claimed, result: out.text };
      await actions.updateOne({ _id: actionId }, { $set: { result: out.text } });
      await audit({ user, conversationId: claimed.conversationId, kind: "action-confirmed", name: claimed.tool, input: claimed.input, ok: true });
    } catch (e) {
      final = { ...claimed, status: "failed", result: errMsg(e) };
      await actions.updateOne({ _id: actionId }, { $set: { status: "failed", result: errMsg(e) } });
      await audit({ user, conversationId: claimed.conversationId, kind: "action-confirmed", name: claimed.tool, ok: false, error: errMsg(e) });
    }
  } else {
    await audit({ user, conversationId: claimed.conversationId, kind: "action-cancelled", name: claimed.tool, ok: true });
  }

  // Reflect the outcome in the stored transcript (without the secret).
  // Read-modify-write keeps this portable across MongoDB-compatible stores; a failure
  // here must never hide the action's real outcome from the user.
  const view = actionView(final);
  try {
    const conv = await conversations.findOne({ _id: claimed.conversationId }, { projection: { display: 1 } });
    if (conv) {
      const display = conv.display.map((m) =>
        m.actions?.some((a) => a.id === view.id) ? { ...m, actions: m.actions.map((a) => (a.id === view.id ? view : a)) } : m,
      );
      await conversations.updateOne({ _id: claimed.conversationId }, { $set: { display, updatedAt: new Date() } });
    }
  } catch (e) {
    console.warn(`[assistant] could not update transcript for action ${view.id}: ${errMsg(e)}`);
  }
  return actionView(final, secret);
}
