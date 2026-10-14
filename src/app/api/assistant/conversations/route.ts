import { listConversations } from "@/lib/assistant/store";
import { requestUser } from "@/lib/assistant/user";
import { handle, ok } from "@/lib/http";

export const dynamic = "force-dynamic";

export const GET = handle(async (req: Request) => {
  const list = await listConversations(requestUser(req));
  return ok(list.map((c) => ({ id: String(c._id), title: c.title, updatedAt: c.updatedAt, agentId: c.agentId ? String(c.agentId) : null })));
});
