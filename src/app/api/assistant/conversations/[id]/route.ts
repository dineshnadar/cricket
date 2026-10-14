import { actionView, loadConversation } from "@/lib/assistant/store";
import { requestUser } from "@/lib/assistant/user";
import { collections } from "@/lib/db";
import { fail, handle, idParam, ok } from "@/lib/http";

export const dynamic = "force-dynamic";
type Ctx = { params: Promise<{ id: string }> };

export const GET = handle(async (req: Request, { params }: Ctx) => {
  const id = await idParam(params);
  const c = id && (await loadConversation(id));
  if (!c || c.user !== requestUser(req)) return fail("Conversation not found", 404);
  // Action cards take their status from the actions collection (the source of truth).
  const { actions } = await collections();
  const live = new Map((await actions.find({ conversationId: c._id }).toArray()).map((a) => [String(a._id), actionView(a)]));
  const messages = c.display.map((m) => (m.actions?.length ? { ...m, actions: m.actions.map((a) => live.get(a.id) ?? a) } : m));
  return ok({ id: String(c._id), title: c.title, agentId: c.agentId ? String(c.agentId) : null, messages });
});

export const DELETE = handle(async (req: Request, { params }: Ctx) => {
  const id = await idParam(params);
  if (!id) return fail("Conversation not found", 404);
  const { conversations } = await collections();
  await conversations.deleteOne({ _id: id, user: requestUser(req) });
  return ok({ deleted: true });
});
