import { requeueChange } from "@/lib/changes";
import { fail, handle, idParam, ok } from "@/lib/http";

export const dynamic = "force-dynamic";

export const POST = handle(async (_req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  if (!id) return fail("Change not found", 404);
  return (await requeueChange(id)) ? ok({ queued: true }) : fail("Change not found or already processing", 409);
});
