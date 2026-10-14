import { getAgent } from "@/lib/agents";
import { fail, handle, idParam, ok } from "@/lib/http";
import { ask } from "@/lib/wisdom";

export const dynamic = "force-dynamic";
export const maxDuration = 120;

export const POST = handle(async (req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const agent = id && (await getAgent(id));
  if (!agent) return fail("Agent not found", 404);
  const body = (await req.json().catch(() => ({}))) as { question?: string };
  const q = body.question?.trim();
  if (!q) return fail("question is required");
  return ok({ answer: await ask(agent, q) });
});
