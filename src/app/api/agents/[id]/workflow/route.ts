import { getAgent } from "@/lib/agents";
import { env } from "@/lib/env";
import { fail, handle, idParam } from "@/lib/http";
import { findRepoConfig } from "@/lib/paths";
import { workflowYaml } from "@/lib/workflow-template";

export const dynamic = "force-dynamic";

/** GET /api/agents/:id/workflow?repo=owner/name -> workflow YAML for that repo */
export const GET = handle(async (req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const agent = id && (await getAgent(id));
  if (!agent) return fail("Agent not found", 404);
  const repoName = new URL(req.url).searchParams.get("repo") ?? "";
  const repo = findRepoConfig(agent, repoName);
  if (!repo) return fail(`Repo ${repoName} is not on this agent`, 404);
  return new Response(workflowYaml(agent, repo, env.appBaseUrl), {
    headers: {
      "Content-Type": "text/yaml; charset=utf-8",
      "Content-Disposition": `attachment; filename="wisdom-ops.yml"`,
    },
  });
});
