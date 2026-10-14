import { rotateToken } from "@/lib/agents";
import { fail, handle, idParam, ok } from "@/lib/http";

export const dynamic = "force-dynamic";

export const POST = handle(async (_req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const token = id && (await rotateToken(id));
  return token ? ok({ token }) : fail("Agent not found", 404);
});
