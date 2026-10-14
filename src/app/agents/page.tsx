import Link from "next/link";
import { Bot, CircleCheck, CirclePause, Plus } from "lucide-react";
import { listAgents } from "@/lib/agents";
import { collections } from "@/lib/db";
import { Badge, Empty, PanelHead, Table } from "@/components/ui";
import { ago } from "@/lib/util";

export const dynamic = "force-dynamic";
export const metadata = { title: "Agents" };

const modeLabel = { action: "GitHub Action", poll: "Poller", both: "Action + poller" } as const;

export default async function AgentsPage() {
  const agents = await listAgents();
  const { changes } = await collections();
  const week = await changes
    .find({ receivedAt: { $gte: new Date(Date.now() - 7 * 86400_000) } }, { projection: { agentId: 1, receivedAt: 1 } })
    .sort({ receivedAt: -1 })
    .limit(5000)
    .toArray();
  const stats = new Map<string, { at: Date; n: number }>();
  for (const c of week) {
    const k = String(c.agentId);
    const s = stats.get(k);
    if (s) s.n++;
    else stats.set(k, { at: c.receivedAt, n: 1 });
  }

  return (
    <>
      <div className="page-head">
        <div>
          <h1>Agents</h1>
          <div className="sub">Each agent watches the repos behind one module.</div>
        </div>
        <Link className="btn primary" href="/agents/new">
          <Plus size={16} aria-hidden /> Create agent
        </Link>
      </div>
      <section className="panel tight">
        <PanelHead title="All agents" icon={Bot} count={agents.length} />
        {agents.length ? (
          <Table label="Agents">
            <thead>
              <tr>
                <th>Agent</th>
                <th>Detection</th>
                <th className="num">Repos</th>
                <th className="num">Pipelines</th>
                <th>Last 7 days</th>
                <th>State</th>
              </tr>
            </thead>
            <tbody>
              {agents.map((a) => {
                const s = stats.get(String(a._id));
                return (
                  <tr key={String(a._id)}>
                    <td>
                      <Link href={`/agents/${a._id}`}>
                        <strong>{a.name}</strong>
                      </Link>
                      <div className="cell-sub">{a.module}</div>
                    </td>
                    <td className="nowrap">{modeLabel[a.mode]}</td>
                    <td className="num mono">{a.repos.length}</td>
                    <td className="num mono">{a.pipelines.length}</td>
                    <td className="nowrap">
                      {s ? (
                        <>
                          <span className="mono">{s.n}</span> changes
                          <div className="cell-sub">last {ago(s.at)}</div>
                        </>
                      ) : (
                        <span className="icon-muted">no activity</span>
                      )}
                    </td>
                    <td>
                      {a.enabled ? (
                        <Badge tone="good" icon={CircleCheck}>
                          enabled
                        </Badge>
                      ) : (
                        <Badge tone="muted" icon={CirclePause}>
                          paused
                        </Badge>
                      )}
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </Table>
        ) : (
          <Empty icon={Bot}>
            No agents yet. <Link href="/agents/new">Create one</Link> for the discretion module to get started.
          </Empty>
        )}
      </section>
    </>
  );
}
