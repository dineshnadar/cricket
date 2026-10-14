import { z } from "zod";
import { requestUser } from "@/lib/assistant/user";
import { collections, toId } from "@/lib/db";
import { saveLabel } from "@/lib/evals";
import { fail, handle, ok } from "@/lib/http";
import { formatZodError } from "@/lib/schemas";

export const dynamic = "force-dynamic";

const body = z.discriminatedUnion("kind", [
  z.object({
    kind: z.literal("triage"),
    targetId: z.string(),
    expected: z.object({ relevant: z.boolean(), riskLevel: z.enum(["low", "medium", "high"]).optional() }),
    note: z.string().max(1000).optional(),
  }),
  z.object({
    kind: z.literal("incident"),
    targetId: z.string(),
    expected: z.object({ causeChangeId: z.string().regex(/^[a-f0-9]{24}$/, "must be a change id").nullable() }),
    note: z.string().max(1000).optional(),
  }),
]);

/** GET ?kind=triage&targetId=… → the current label, or null. */
export const GET = handle(async (req: Request) => {
  const u = new URL(req.url);
  const id = toId(u.searchParams.get("targetId") ?? "");
  const kind = u.searchParams.get("kind");
  if (!id || (kind !== "triage" && kind !== "incident")) return fail("kind and targetId are required");
  const { evalLabels } = await collections();
  return ok(await evalLabels.findOne({ kind, targetId: id }));
});

/** POST: record (or replace) a person's judgement of one AI decision. */
export const POST = handle(async (req: Request) => {
  const parsed = body.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const id = toId(parsed.data.targetId);
  if (!id) return fail("Invalid targetId");
  try {
    const label = await saveLabel({ ...parsed.data, targetId: id, user: requestUser(req) });
    return ok(label, 201);
  } catch (e) {
    return fail(e instanceof Error ? e.message : String(e), 404);
  }
});
