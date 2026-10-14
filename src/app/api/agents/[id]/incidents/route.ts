import { z } from "zod";
import { getAgent } from "@/lib/agents";
import { fail, handle, idParam, ok } from "@/lib/http";
import { createManualIncident } from "@/lib/incidents";
import { formatZodError } from "@/lib/schemas";

export const dynamic = "force-dynamic";

const schema = z.object({
  number: z.string().trim().min(1),
  shortDescription: z.string().trim().min(1),
  description: z.string().default(""),
  openedAt: z.coerce.date().optional(),
});

/** Manually log an incident; the worker analyses it within a minute. */
export const POST = handle(async (req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const agent = id && (await getAgent(id));
  if (!agent) return fail("Agent not found", 404);
  const parsed = schema.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const inc = await createManualIncident(agent, parsed.data);
  return ok({ id: String(inc._id) }, 201);
});
