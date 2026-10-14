import Link from "next/link";
import { notFound } from "next/navigation";
import {
  Bell,
  BrainCircuit,
  ChevronRight,
  CircleCheck,
  CircleX,
  ExternalLink,
  Eye,
  FileCode2,
  FlaskConical,
  GitCommitHorizontal,
  Layers,
  ShieldAlert,
  Workflow,
} from "lucide-react";
import { collections, toId } from "@/lib/db";
import { fmtDate, shortSha } from "@/lib/util";
import { ActionButton } from "@/components/client";
import { TriageFeedback } from "@/components/Feedback";
import { Badge, ChangeStatusBadge, Diff, Empty, Md, PanelHead, RiskBadge, RunBadge, Table } from "@/components/ui";

export const dynamic = "force-dynamic";

export default async function ChangePage({ params }: { params: Promise<{ id: string }> }) {
  const { id } = await params;
  const oid = toId(id);
  const { changes, agents, runs } = await collections();
  const c = oid && (await changes.findOne({ _id: oid }));
  if (!c) notFound();
  const [agent, runList] = await Promise.all([
    agents.findOne({ _id: c.agentId }, { projection: { name: 1, module: 1 } }),
    runs.find({ changeId: c._id }).sort({ triggeredAt: -1 }).toArray(),
  ]);
  const matched = new Set(c.matchedFiles);
  const t = c.triage;
  const files = [...c.files].sort((a, b) => Number(matched.has(b.filename)) - Number(matched.has(a.filename)));

  return (
    <>
      <div className="page-head">
        <div>
          <nav className="crumbs" aria-label="Breadcrumb">
            <Link href={`/agents/${c.agentId}?tab=changes`}>{agent?.name ?? "Agent"}</Link>
            <ChevronRight size={14} aria-hidden />
            <span>Change</span>
          </nav>
          <h1>
            {c.repo}{" "}
            <span className="mono" style={{ fontSize: 15, fontWeight: 400, color: "var(--color-muted-foreground)" }}>
              {c.branch} · {shortSha(c.after)}
            </span>
          </h1>
          <div className="sub">
            <ChangeStatusBadge status={c.status} />
            <RiskBadge risk={t?.riskLevel} />
            <span>
              received {fmtDate(c.receivedAt)} via {c.source}
            </span>
            {c.compareUrl && (
              <a href={c.compareUrl} target="_blank" rel="noreferrer" className="row" style={{ gap: 4 }}>
                Compare on GitHub <ExternalLink size={13} aria-hidden />
              </a>
            )}
          </div>
        </div>
        <ActionButton url={`/api/changes/${id}/reprocess`} label="Re-run triage" busyLabel="Queuing…" icon="refresh" />
      </div>

      {c.error && (
        <div className="error" role="alert">
          <CircleX size={16} aria-hidden />
          <div>{c.error}</div>
        </div>
      )}

      <section className="panel">
        <PanelHead title="Wisdom AI triage" icon={BrainCircuit}>
          {t && (
            <span className="hint" style={{ margin: 0 }}>
              {t.relevant ? "relevant" : "not relevant"} · <span className="mono">{Math.round(t.confidence * 100)}%</span> ·{" "}
              {t.model}
            </span>
          )}
        </PanelHead>
        {t ? (
          <>
            <Md>{t.summary}</Md>
            <div className="grid g3" style={{ marginTop: 16 }}>
              <div>
                <h3 className="row" style={{ gap: 6 }}>
                  <Layers size={13} aria-hidden /> Impacted areas
                </h3>
                <div className="chips">
                  {t.impactedAreas.length ? t.impactedAreas.map((a) => <span className="chip" key={a}>{a}</span>) : "—"}
                </div>
              </div>
              <div>
                <h3 className="row" style={{ gap: 6 }}>
                  <ShieldAlert size={13} aria-hidden /> Why this risk
                </h3>
                <ul>{t.riskReasons.map((r, i) => <li key={i}>{r}</li>)}</ul>
              </div>
              <div>
                <h3 className="row" style={{ gap: 6 }}>
                  <FlaskConical size={13} aria-hidden /> Test focus
                </h3>
                <ul>{t.testFocus.map((r, i) => <li key={i}>{r}</li>)}</ul>
              </div>
            </div>
            <TriageFeedback changeId={String(c._id)} aiRelevant={t.relevant} aiRisk={t.riskLevel} />
          </>
        ) : (
          <p className="hint" style={{ margin: 0 }}>
            {c.status === "filtered"
              ? "No watched paths were touched, so Wisdom AI wasn't asked."
              : "Waiting for the worker to triage this change."}
          </p>
        )}
      </section>

      <div className="grid g2">
        <section className="panel tight">
          <PanelHead title="Pipelines" icon={Workflow} count={runList.length} />
          {runList.length ? (
            <Table>
              <tbody>
                {runList.map((r) => (
                  <tr key={String(r._id)}>
                    <td>
                      {r.pipelineName}
                      <div className="cell-sub">{r.tests ? `${r.tests.failed} failed of ${r.tests.total} tests` : r.type}</div>
                      {r.error && <div className="cell-sub" style={{ color: "var(--color-bad)" }}>{r.error.slice(0, 200)}</div>}
                    </td>
                    <td className="mono">
                      {r.buildUrl ? (
                        <a href={r.buildUrl} target="_blank" rel="noreferrer">
                          #{r.buildNumber}
                        </a>
                      ) : (
                        "—"
                      )}
                    </td>
                    <td className="num">
                      <RunBadge status={r.status} />
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          ) : (
            <Empty icon={Workflow}>None triggered.</Empty>
          )}
        </section>
        <section className="panel tight">
          <PanelHead title="Notifications" icon={Bell} count={c.notifications.length} />
          {c.notifications.length ? (
            <Table>
              <tbody>
                {c.notifications.map((n, i) => (
                  <tr key={i}>
                    <td>
                      <span style={{ textTransform: "capitalize" }}>{n.channel}</span>
                      {n.detail && <div className="cell-sub">{n.detail}</div>}
                    </td>
                    <td className="num">
                      <Badge tone={n.ok ? "good" : "bad"} icon={n.ok ? CircleCheck : CircleX}>
                        {n.ok ? "sent" : "failed"}
                      </Badge>
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          ) : (
            <Empty icon={Bell}>None sent.</Empty>
          )}
        </section>
      </div>

      <section className="panel tight">
        <PanelHead title="Commits" icon={GitCommitHorizontal} count={c.commits.length} />
        <Table>
          <tbody>
            {c.commits.map((cm) => (
              <tr key={cm.sha}>
                <td className="mono nowrap" style={{ width: 90 }}>
                  {cm.url ? (
                    <a href={cm.url} target="_blank" rel="noreferrer">
                      {shortSha(cm.sha)}
                    </a>
                  ) : (
                    shortSha(cm.sha)
                  )}
                </td>
                <td className="nowrap" style={{ width: 140 }}>
                  {cm.author}
                </td>
                <td>{cm.message.split("\n")[0]}</td>
              </tr>
            ))}
          </tbody>
        </Table>
      </section>

      <section className="panel">
        <PanelHead title="Files" icon={FileCode2}>
          <span className="hint mono" style={{ margin: 0 }}>
            {c.matchedFiles.length} watched / {c.files.length}
          </span>
        </PanelHead>
        {files.map((f) => (
          <details key={f.filename} open={matched.has(f.filename) && c.files.length <= 5}>
            <summary>
              <ChevronRight size={14} className="chev" aria-hidden />
              <span className="mono">{f.filename}</span>
              <span className="hint mono" style={{ margin: 0 }}>
                {f.status} <span style={{ color: "var(--color-good)" }}>+{f.additions}</span>{" "}
                <span style={{ color: "var(--color-bad)" }}>−{f.deletions}</span>
              </span>
              {matched.has(f.filename) && (
                <Badge tone="info" icon={Eye}>
                  watched
                </Badge>
              )}
            </summary>
            {f.patch ? <Diff patch={f.patch} /> : <p className="hint">No patch (binary or too large).</p>}
          </details>
        ))}
      </section>
    </>
  );
}
