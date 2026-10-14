import { z } from "zod";
import { audit } from "@/lib/assistant/store";
import { requestUser } from "@/lib/assistant/user";
import { fail, handle, ok } from "@/lib/http";
import { formatZodError } from "@/lib/schemas";
import { draftRepoNotes, improveModuleScope, suggestPaths } from "@/lib/setup-assist";
import { errMsg } from "@/lib/util";

export const dynamic = "force-dynamic";

const repoName = z.string().regex(/^[\w.-]+\/[\w.-]+$/, "repo must be owner/name");
const text = (max: number) => z.string().max(max).default("");

const body = z.discriminatedUnion("kind", [
  z.object({
    kind: z.literal("module"),
    rough: text(4000),
    moduleName: text(200),
    repos: z.array(z.object({ fullName: z.string(), notes: text(2000).optional() })).max(50).default([]),
  }),
  z.object({
    kind: z.literal("repoNotes"),
    repo: repoName,
    branches: z.array(z.string()).max(20).default([]),
    rough: text(4000),
    moduleDescription: text(4000),
  }),
  z.object({
    kind: z.literal("paths"),
    repo: repoName,
    branches: z.array(z.string()).max(20).default([]),
    notes: text(4000),
    moduleDescription: text(4000),
  }),
  // The form reports what the person did with a suggestion, for the audit trail.
  z.object({
    kind: z.literal("decision"),
    field: z.enum(["module", "repoNotes", "paths"]),
    repo: z.string().max(200).optional(),
    decision: z.enum(["used", "kept-mine"]),
  }),
]);

/** POST /api/agents/assist: AI suggestions for the agent form. Never saves anything itself. */
export const POST = handle(async (req: Request) => {
  const parsed = body.safeParse(await req.json().catch(() => null));
  if (!parsed.success) return fail(formatZodError(parsed.error));
  const b = parsed.data;
  const user = requestUser(req);

  if (b.kind === "decision") {
    await audit({ user, kind: "assist", name: `assist-${b.field}`, input: { repo: b.repo, decision: b.decision }, ok: true });
    return ok({ ok: true });
  }

  const target = b.kind === "module" ? b.moduleName || "module" : b.repo;
  try {
    const result =
      b.kind === "module"
        ? await improveModuleScope(b)
        : b.kind === "repoNotes"
          ? await draftRepoNotes(b)
          : await suggestPaths({ repo: b.repo, branches: b.branches, notes: b.notes, moduleDescription: b.moduleDescription });
    await audit({ user, kind: "assist", name: `assist-${b.kind}`, input: { target }, ok: true });
    return ok(result);
  } catch (e) {
    await audit({ user, kind: "assist", name: `assist-${b.kind}`, input: { target }, ok: false, error: errMsg(e) });
    return fail(errMsg(e), 502);
  }
});
