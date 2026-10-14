import { handle, ok } from "@/lib/http";
import { searchRepos } from "@/lib/github";

export const dynamic = "force-dynamic";

export const GET = handle(async (req: Request) => {
  const q = new URL(req.url).searchParams.get("q") ?? "";
  return ok(await searchRepos(q));
});
