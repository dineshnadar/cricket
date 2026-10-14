import Link from "next/link";
import { Activity, Bot, CircleCheck, CircleMinus, CircleX, GitCommitHorizontal, Plug, Plus, Siren, TriangleAlert, Workflow, Zap } from "lucide-react";
import { collections } from "@/lib/db";
import { integrationStatus } from "@/lib/env";
import { Badge, ChangeStatusBadge, Empty, IncidentBadge, PanelHead, RiskBadge, RunBadge, Table } from "@/components/ui";
import { ago, shortSha } from "@/lib/util";

export const dynamic = "force-dynamic";

export default async function Dashboard() {
  const { agents, changes, runs, incidents, meta } = await collections();
  const day = new Date(Date.now() - 86400_000);
  const [agentList, changes24, relevant24, failing, openInc, recent, recentRuns, recentInc, hb] = await Promise.all([
    agents.find({}, { projection: { name: 1, module: 1 } }).toArray(),
    changes.countDocuments({ receivedAt: { $gte: day } }),
    changes.countDocuments({ receivedAt: { $gte: day }, status: "actioned" }),
    runs.countDocuments({ triggeredAt: { $gte: day }, status: { $in: ["failure", "unstable", "error"] } }),
    incidents.countDocuments({ createdAt: { $gte: new Date(Date.now() - 7 * 86400_000) } }),
    changes.find({}).sort({ receivedAt: -1 }).limit(12).toArray(),
    runs.find({}).sort({ triggeredAt: -1 }).limit(8).toArray(),
    incidents.find({}).sort({ openedAt: -1 }).limit(6).toArray(),
    meta.findOne({ _id: "worker:heartbeat" }),
  ]);
  const names = new Map(agentList.map((a) => [String(a._id), a.name]));
  const integ = integrationStatus();
  const workerAt = hb ? new Date(hb.value as string) : null;
  const workerAlive = Boolean(workerAt && Date.now() - workerAt.getTime() < 120_000);

  const stats = [
    { k: "Agents", v: agentList.length, icon: Bot },
    { k: "Pushes · 24h", v: changes24, icon: GitCommitHorizontal },
    { k: "Relevant · 24h", v: relevant24, icon: Zap },
    { k: "Failed runs · 24h", v: failing, icon: CircleX, alert: failing > 0 },
    { k: "Incidents · 7d", v: openInc, icon: Siren, alert: openInc > 0 },
  ];

  return (
    <>
      <div className="page-head">
        <div>
          <h1>Dashboard</h1>
          <div className="sub">What changed across your modules, what it means, and what broke.</div>
        </div>
        <div className="row">
          <Badge tone={workerAlive ? "good" : "warn"} icon={workerAlive ? Activity : TriangleAlert}>
            worker {workerAlive ? "running" : workerAt ? `last seen ${ago(workerAt)}` : "not started"}
          </Badge>
          <Link className="btn primary" href="/agents/new">
            <Plus size={16} aria-hidden /> Create agent
          </Link>
        </div>
      </div>

      {!workerAlive && (
        <div className="notice" role="status">
          <TriangleAlert size={16} aria-hidden />
          <div>
            Changes are being queued but won&apos;t be triaged until the background worker runs: <code>npm run worker</code>
          </div>
        </div>
      )}

      <div className="stats">
        {stats.map(({ k, v, icon: Icon, alert }) => (
          <div className={`stat${alert ? " alert" : ""}`} key={k}>
            <div className="k">
              <Icon size={14} aria-hidden /> {k}
            </div>
            <div className="v">{v}</div>
          </div>
        ))}
      </div>

      <section className="panel tight">
        <PanelHead title="Latest changes" icon={GitCommitHorizontal} count={recent.length} />
        {recent.length ? (
          <Table label="Latest changes">
            <thead>
              <tr>
                <th>When</th>
                <th>Repo</th>
                <th>Commit</th>
                <th>Agent</th>
                <th>Risk</th>
                <th>Status</th>
              </tr>
            </thead>
            <tbody>
              {recent.map((c) => (
                <tr key={String(c._id)}>
                  <td className="nowrap">{ago(c.receivedAt)}</td>
                  <td className="nowrap">
                    <Link href={`/changes/${c._id}`}>{c.repo}</Link>
                    <div className="cell-sub mono">
                      {c.branch} · {shortSha(c.after)}
                    </div>
                  </td>
                  <td>
                    {c.commits[0]?.message.split("\n")[0].slice(0, 80) ?? "—"}
                    {c.commits.length > 1 && <span className="cell-sub"> +{c.commits.length - 1} more</span>}
                  </td>
                  <td className="nowrap">{names.get(String(c.agentId)) ?? "—"}</td>
                  <td>
                    <RiskBadge risk={c.triage?.riskLevel} />
                  </td>
                  <td>
                    <ChangeStatusBadge status={c.status} />
                  </td>
                </tr>
              ))}
            </tbody>
          </Table>
        ) : (
          <Empty icon={GitCommitHorizontal}>
            No changes yet. <Link href="/agents/new">Create an agent</Link> and install its workflow, or use poll mode.
          </Empty>
        )}
      </section>

      <div className="grid g2">
        <section className="panel tight">
          <PanelHead title="Pipeline runs" icon={Workflow} count={recentRuns.length} />
          {recentRuns.length ? (
            <Table>
              <tbody>
                {recentRuns.map((r) => (
                  <tr key={String(r._id)}>
                    <td>
                      <Link href={`/changes/${r.changeId}`}>{r.pipelineName}</Link>
                      <div className="cell-sub">
                        {ago(r.triggeredAt)}
                        {r.tests ? ` · ${r.tests.failed}/${r.tests.total} tests failed` : ""}
                      </div>
                    </td>
                    <td className="num">
                      <RunBadge status={r.status} />
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          ) : (
            <Empty icon={Workflow}>No pipeline runs yet.</Empty>
          )}
        </section>
        <section className="panel tight">
          <PanelHead title="Incidents" icon={Siren} count={recentInc.length} />
          {recentInc.length ? (
            <Table>
              <tbody>
                {recentInc.map((i) => (
                  <tr key={String(i._id)}>
                    <td>
                      <Link href={`/incidents/${i._id}`} className="mono">
                        {i.number}
                      </Link>{" "}
                      {i.shortDescription}
                      <div className="cell-sub">
                        {ago(i.openedAt)}
                        {i.analysis?.likelyCauses[0] ? ` · suspect ${i.analysis.likelyCauses[0].repo}` : ""}
                      </div>
                    </td>
                    <td className="num">
                      <IncidentBadge status={i.status} />
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          ) : (
            <Empty icon={Siren}>No incidents. Enable ServiceNow on an agent, or log one manually.</Empty>
          )}
        </section>
      </div>

      <section className="panel">
        <div className="spread">
          <h2 style={{ margin: 0, display: "flex", alignItems: "center", gap: 8 }}>
            <Plug size={16} aria-hidden className="icon-muted" /> Integrations
          </h2>
          <div className="chips">
            {Object.entries(integ)
              .filter(([k]) => k !== "dryRun")
              .map(([k, v]) => (
                <Badge key={k} tone={v ? "good" : "muted"} icon={v ? CircleCheck : CircleMinus}>
                  {k}
                </Badge>
              ))}
            {integ.dryRun && (
              <Badge tone="warn" icon={TriangleAlert}>
                dry run
              </Badge>
            )}
          </div>
        </div>
      </section>
    </>
  );
}
