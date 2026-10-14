import { getAgent } from "@/lib/agents";
import { fail, handle, idParam, ok } from "@/lib/http";
import { refreshWisdom } from "@/lib/wisdom";

export const dynamic = "force-dynamic";
export const maxDuration = 300;

export const POST = handle(async (_req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const agent = id && (await getAgent(id));
  if (!agent) return fail("Agent not found", 404);
  const w = await refreshWisdom(agent);
  return ok({ generatedAt: w.generatedAt, repos: w.repos.length });
});
