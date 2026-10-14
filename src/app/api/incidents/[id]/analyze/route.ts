import { collections } from "@/lib/db";
import { fail, handle, idParam, ok } from "@/lib/http";
import { analyzeIncident } from "@/lib/incidents";

export const dynamic = "force-dynamic";
export const maxDuration = 300;

export const POST = handle(async (_req: Request, { params }: { params: Promise<{ id: string }> }) => {
  const id = await idParam(params);
  const { incidents, agents } = await collections();
  const inc = id && (await incidents.findOne({ _id: id }));
  if (!inc) return fail("Incident not found", 404);
  const agent = await agents.findOne({ _id: inc.agentId });
  if (!agent) return fail("Agent not found", 404);
  const updated = await analyzeIncident({ ...inc, notified: true }, agent); // re-analysis doesn't re-notify
  return ok({ status: updated.status });
});
