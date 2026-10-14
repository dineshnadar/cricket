import Link from "next/link";
import { notFound } from "next/navigation";
import { BrainCircuit, ChevronRight, CircleX, ExternalLink, FileSearch, FileText, ListChecks, MessageSquareText, Search, Target } from "lucide-react";
import { collections, toId } from "@/lib/db";
import { fmtDate, shortSha } from "@/lib/util";
import { ActionButton } from "@/components/client";
import { IncidentFeedback } from "@/components/Feedback";
import { ConfidenceBadge, Empty, IncidentBadge, Md, PanelHead, Table } from "@/components/ui";

export const dynamic = "force-dynamic";

export default async function IncidentPage({ params }: { params: Promise<{ id: string }> }) {
  const { id } = await params;
  const oid = toId(id);
  const { incidents, agents } = await collections();
  const inc = oid && (await incidents.findOne({ _id: oid }));
  if (!inc) notFound();
  const agent = await agents.findOne({ _id: inc.agentId }, { projection: { name: 1 } });
  const a = inc.analysis;

  return (
    <>
      <div className="page-head">
        <div>
          <nav className="crumbs" aria-label="Breadcrumb">
            <Link href={`/agents/${inc.agentId}?tab=incidents`}>{agent?.name ?? "Agent"}</Link>
            <ChevronRight size={14} aria-hidden />
            <span>Incident</span>
          </nav>
          <h1>
            <span className="mono">{inc.number}</span> {inc.shortDescription}
          </h1>
          <div className="sub">
            <IncidentBadge status={inc.status} />
            <span>opened {fmtDate(inc.openedAt)}</span>
            {inc.priority && (
              <>
                <span className="sep">·</span>
                <span className="mono">P{inc.priority}</span>
              </>
            )}
            {inc.url && (
              <a href={inc.url} target="_blank" rel="noreferrer" className="row" style={{ gap: 4 }}>
                Open in ServiceNow <ExternalLink size={13} aria-hidden />
              </a>
            )}
          </div>
        </div>
        <ActionButton
          url={`/api/incidents/${id}/analyze`}
          label={a ? "Re-analyse" : "Analyse now"}
          busyLabel="Analysing… (up to a minute)"
          icon="sparkles"
        />
      </div>

      {inc.error && (
        <div className="error" role="alert">
          <CircleX size={16} aria-hidden />
          <div>{inc.error}</div>
        </div>
      )}

      {a ? (
        <>
          <section className="panel">
            <PanelHead title="Analysis" icon={BrainCircuit} />
            <Md>{a.summary}</Md>
          </section>
          <section className="panel tight">
            <PanelHead title="Likely causes" icon={Target} count={a.likelyCauses.length} />
            {a.likelyCauses.length ? (
              <Table label="Likely causes">
                <thead>
                  <tr>
                    <th>Change</th>
                    <th>Why</th>
                    <th className="num">Confidence</th>
                  </tr>
                </thead>
                <tbody>
                  {a.likelyCauses.map((c, i) => (
                    <tr key={i}>
                      <td className="nowrap mono">
                        {c.changeId ? (
                          <Link href={`/changes/${c.changeId}`}>
                            {c.repo}@{shortSha(c.sha)}
                          </Link>
                        ) : (
                          (c.repo ?? "outside tracked changes")
                        )}
                      </td>
                      <td>{c.reason}</td>
                      <td className="num">
                        <ConfidenceBadge value={c.confidence} />
                      </td>
                    </tr>
                  ))}
                </tbody>
              </Table>
            ) : (
              <Empty icon={Target}>No recent change fits the symptoms.</Empty>
            )}
          </section>
          <section className="panel">
            <PanelHead title="Feedback" icon={MessageSquareText} />
            <IncidentFeedback
              incidentId={String(inc._id)}
              causes={a.likelyCauses
                .filter((c) => c.changeId)
                .map((c) => ({ changeId: c.changeId!, label: `${c.repo ?? "?"}@${shortSha(c.sha)}` }))}
            />
          </section>
          <div className="grid g2">
            <section className="panel">
              <PanelHead title="Evidence" icon={FileSearch} />
              <ul>{a.evidence.map((e, i) => <li key={i}>{e}</li>)}</ul>
            </section>
            <section className="panel">
              <PanelHead title="Suggested next steps" icon={ListChecks} />
              <ol style={{ margin: 0, paddingLeft: 20 }}>{a.suggestedActions.map((e, i) => <li key={i}>{e}</li>)}</ol>
            </section>
          </div>
        </>
      ) : (
        <section className="panel">
          <p className="hint" style={{ margin: 0 }}>Not analysed yet. The worker picks up new incidents within a minute.</p>
        </section>
      )}

      <section className="panel">
        <PanelHead title="Incident description" icon={FileText} />
        <pre style={{ fontSize: 13 }}>{inc.description || "—"}</pre>
      </section>

      <section className="panel">
        <PanelHead title="Splunk evidence" icon={Search} count={inc.splunkEvents.length} />
        {inc.splunkQuery && (
          <div className="diff" style={{ padding: "10px 0", marginBottom: 12 }}>
            <div>{inc.splunkQuery}</div>
          </div>
        )}
        {inc.splunkEvents.length ? (
          inc.splunkEvents.map((e, i) => (
            <details key={i}>
              <summary className="mono" style={{ fontSize: 13 }}>
                <ChevronRight size={14} className="chev" aria-hidden />
                <span className="icon-muted">{e.time}</span> <span className="icon-muted">{e.host}</span> {e.raw.slice(0, 120)}
              </summary>
              <div className="diff">
                <div>{e.raw}</div>
              </div>
            </details>
          ))
        ) : (
          <p className="hint" style={{ margin: 0 }}>No events (Splunk disabled for this agent, or nothing matched).</p>
        )}
      </section>
    </>
  );
}
