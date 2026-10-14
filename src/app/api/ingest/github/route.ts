import { recordChange } from "@/lib/changes";
import { collections, toId } from "@/lib/db";
import { fail, handle, ok } from "@/lib/http";
import { branchWatched, findRepoConfig } from "@/lib/paths";
import { formatZodError, ingestSchema } from "@/lib/schemas";
import { tokenMatches } from "@/lib/util";

export const dynamic = "force-dynamic";

/**
 * Called by the per-repo GitHub Action on every push.
 * Auth: Authorization: Bearer <agent ingest token>
 */
export const POST = handle(async (req: Request) => {
  const auth = req.headers.get("authorization") ?? "";
  const token = auth.replace(/^Bearer\s+/i, "").trim();
  if (!token) return fail("Missing bearer token", 401);

  const parsed = ingestSchema.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const p = parsed.data;

  const agentId = toId(p.agentId);
  const { agents } = await collections();
  const agent = agentId && (await agents.findOne({ _id: agentId }));
  if (!agent || !tokenMatches(token, agent.ingestTokenHash)) return fail("Invalid agent or token", 401);
  if (!agent.enabled) return ok({ accepted: false, reason: "agent disabled" }, 202);

  const repo = findRepoConfig(agent, p.repo);
  if (!repo) return ok({ accepted: false, reason: `${p.repo} is not watched by this agent` }, 202);
  if (repo.branches.length && !branchWatched(repo, p.branch)) {
    return ok({ accepted: false, reason: `branch ${p.branch} is not watched` }, 202);
  }

  const change = await recordChange({
    agent,
    repo: repo.fullName,
    branch: p.branch,
    before: p.before,
    after: p.after,
    pusher: p.pusher,
    compareUrl: p.compareUrl,
    source: "github-action",
    commits: p.commits,
    files: p.files,
  });
  if (!change) return ok({ accepted: false, reason: "duplicate" }, 200);
  return ok(
    { accepted: true, changeId: String(change._id), watchedFiles: change.matchedFiles.length },
    202,
  );
});
