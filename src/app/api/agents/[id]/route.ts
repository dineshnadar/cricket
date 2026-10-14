import { deleteAgent, getAgent, toPublic, updateAgent } from "@/lib/agents";
import { fail, handle, idParam, ok } from "@/lib/http";
import { agentInputSchema, formatZodError } from "@/lib/schemas";

export const dynamic = "force-dynamic";
type Ctx = { params: Promise<{ id: string }> };

export const GET = handle(async (_req: Request, { params }: Ctx) => {
  const id = await idParam(params);
  const agent = id && (await getAgent(id));
  return agent ? ok(toPublic(agent)) : fail("Agent not found", 404);
});

export const PUT = handle(async (req: Request, { params }: Ctx) => {
  const id = await idParam(params);
  if (!id) return fail("Agent not found", 404);
  const parsed = agentInputSchema.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const agent = await updateAgent(id, parsed.data);
  return agent ? ok(toPublic(agent)) : fail("Agent not found", 404);
});

export const DELETE = handle(async (_req: Request, { params }: Ctx) => {
  const id = await idParam(params);
  if (!id) return fail("Agent not found", 404);
  await deleteAgent(id);
  return ok({ deleted: true });
});
