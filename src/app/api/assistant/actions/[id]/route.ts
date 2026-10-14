import { z } from "zod";
import { decideAction } from "@/lib/assistant/store";
import { requestUser } from "@/lib/assistant/user";
import { fail, handle, idParam, ok } from "@/lib/http";

export const dynamic = "force-dynamic";
export const maxDuration = 300;

const body = z.object({ decision: z.enum(["confirm", "cancel"]) });

export const POST = handle(async (req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  if (!id) return fail("Action not found", 404);
  const parsed = body.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail("decision must be confirm or cancel");
  return ok(await decideAction(id, parsed.data.decision, requestUser(req)));
});
