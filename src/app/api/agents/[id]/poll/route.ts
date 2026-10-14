import { getAgent } from "@/lib/agents";
import { fail, handle, idParam, ok } from "@/lib/http";
import { pollAgent } from "@/lib/poller";

export const dynamic = "force-dynamic";
export const maxDuration = 120;

export const POST = handle(async (_req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const agent = id && (await getAgent(id));
  if (!agent) return fail("Agent not found", 404);
  return ok(await pollAgent(agent));
});
