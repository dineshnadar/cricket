import Link from "next/link";
import { notFound } from "next/navigation";
import { ChevronRight } from "lucide-react";
import AgentForm from "@/components/AgentForm";
import { getAgent } from "@/lib/agents";
import { toId } from "@/lib/db";
import type { AgentInput } from "@/lib/schemas";

export const dynamic = "force-dynamic";

export default async function EditAgentPage({ params }: { params: Promise<{ id: string }> }) {
  const { id } = await params;
  const oid = toId(id);
  const a = oid && (await getAgent(oid));
  if (!a) notFound();
  const initial: AgentInput = {
    name: a.name,
    module: a.module,
    description: a.description,
    repos: a.repos,
    mode: a.mode,
    pollingIntervalMinutes: a.pollingIntervalMinutes,
    emailRecipients: a.emailRecipients,
    teamsWebhookUrl: a.teamsWebhookUrl,
    relevanceThreshold: a.relevanceThreshold,
    pipelines: a.pipelines,
    notifyOnPipeline: a.notifyOnPipeline,
    servicenow: a.servicenow,
    splunk: a.splunk,
    enabled: a.enabled,
  };
  return (
    <>
      <div className="page-head">
        <div>
          <nav className="crumbs" aria-label="Breadcrumb">
            <Link href="/agents">Agents</Link>
            <ChevronRight size={14} aria-hidden />
            <Link href={`/agents/${id}`}>{a.name}</Link>
            <ChevronRight size={14} aria-hidden />
            <span>Edit</span>
          </nav>
          <h1>Edit {a.name}</h1>
        </div>
      </div>
      <AgentForm initial={initial} agentId={id} />
    </>
  );
}
