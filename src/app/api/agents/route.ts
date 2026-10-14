import { createAgent, listAgents, toPublic } from "@/lib/agents";
import { fail, handle, ok } from "@/lib/http";
import { agentInputSchema, formatZodError } from "@/lib/schemas";

export const dynamic = "force-dynamic";

export const GET = handle(async () => ok((await listAgents()).map(toPublic)));

export const POST = handle(async (req: Request) => {
  const parsed = agentInputSchema.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const { agent, token } = await createAgent(parsed.data);
  // The token is only ever returned here and on rotation.
  return ok({ agent: toPublic(agent), token }, 201);
});
