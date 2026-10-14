import { fail, handle, ok } from "@/lib/http";
import { listBranches } from "@/lib/github";

export const dynamic = "force-dynamic";

export const GET = handle(async (req: Request) => {
  const repo = new URL(req.url).searchParams.get("repo") ?? "";
  if (!/^[\w.-]+\/[\w.-]+$/.test(repo)) return fail("repo must be owner/name");
  return ok(await listBranches(repo));
});
