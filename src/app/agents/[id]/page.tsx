import Link from "next/link";
import { notFound } from "next/navigation";
import type { ObjectId } from "mongodb";
import {
  BookOpen,
  ChevronRight,
  CirclePause,
  FolderGit2,
  GitCommitHorizontal,
  KeyRound,
  LayoutGrid,
  MessageSquareText,
  Pencil,
  Settings2,
  Siren,
  Target,
  Workflow,
} from "lucide-react";
import { getAgent } from "@/lib/agents";
import { collections, toId } from "@/lib/db";
import { env } from "@/lib/env";
import { ago, plural, shortSha } from "@/lib/util";
import { latestWisdom } from "@/lib/wisdom";
import { workflowYaml } from "@/lib/workflow-template";
import { ActionButton, AskBox, CopyBox, ManualIncident, RotateToken } from "@/components/client";
import { Badge, ChangeStatusBadge, ConfidenceBadge, Empty, IncidentBadge, Md, PanelHead, RiskBadge, RunBadge, Table } from "@/components/ui";

export const dynamic = "force-dynamic";

const TABS = [
  { id: "overview", label: "Overview", icon: LayoutGrid },
  { id: "changes", label: "Changes", icon: GitCommitHorizontal },
  { id: "runs", label: "Runs", icon: Workflow },
  { id: "wisdom", label: "Wisdom", icon: BookOpen },
  { id: "incidents", label: "Incidents", icon: Siren },
  { id: "setup", label: "Setup", icon: Settings2 },
] as const;
type Tab = (typeof TABS)[number]["id"];

export default async function AgentPage({
  params,
  searchParams,
}: {
  params: Promise<{ id: string }>;
  searchParams: Promise<{ tab?: string; show?: string }>;
}) {
  const { id } = await params;
  const sp = await searchParams;
  const tab: Tab = (TABS.find((t) => t.id === sp.tab)?.id ?? "overview") as Tab;
  const oid = toId(id);
  const agent = oid && (await getAgent(oid));
  if (!agent) notFound();

  const detection =
    agent.mode === "action"
      ? "GitHub Action"
      : agent.mode === "poll"
        ? `polling every ${agent.pollingIntervalMinutes}m`
        : `Action + polling every ${agent.pollingIntervalMinutes}m`;

  return (
    <>
      <div className="page-head">
        <div>
          <nav className="crumbs" aria-label="Breadcrumb">
            <Link href="/agents">Agents</Link>
            <ChevronRight size={14} aria-hidden />
            <span>{agent.module}</span>
          </nav>
          <h1>{agent.name}</h1>
          <div className="sub">
            <span>{plural(agent.repos.length, "repo")}</span>
            <span className="sep">·</span>
            <span>{detection}</span>
            <span className="sep">·</span>
            <span>{plural(agent.pipelines.length, "pipeline")}</span>
            {!agent.enabled && (
              <Badge tone="muted" icon={CirclePause}>
                paused
              </Badge>
            )}
          </div>
        </div>
        <div className="row">
          {agent.mode !== "action" && <ActionButton url={`/api/agents/${id}/poll`} label="Poll now" busyLabel="Polling…" icon="refresh" />}
          <Link className="btn" href={`/agents/${id}/edit`}>
            <Pencil size={15} aria-hidden /> Edit
          </Link>
          <ActionButton
            url={`/api/agents/${id}`}
            method="DELETE"
            label="Delete"
            icon="trash"
            className="danger"
            confirm={`Delete ${agent.name} and all its history? This cannot be undone.`}
            redirectTo="/agents"
          />
        </div>
      </div>

      <nav className="tabs" aria-label="Agent sections">
        {TABS.map(({ id: t, label, icon: Icon }) => (
          <Link key={t} aria-current={t === tab ? "page" : undefined} href={`/agents/${id}?tab=${t}`}>
            <Icon size={15} aria-hidden />
            {label}
          </Link>
        ))}
      </nav>

      {tab === "overview" && (
        <>
          <section className="panel">
            <PanelHead title="Ask the module" icon={MessageSquareText} />
            <AskBox agentId={id} module={agent.module} />
          </section>
          <div className="grid g2">
            <section className="panel">
              <PanelHead title="Scope" icon={Target} />
              <p>{agent.description}</p>
              <dl className="kv">
                <dt>Threshold</dt>
                <dd>
                  <span className="mono">{Math.round(agent.relevanceThreshold * 100)}%</span> confidence
                </dd>
                <dt>Email</dt>
                <dd>{agent.emailRecipients.join(", ") || "—"}</dd>
                <dt>Teams</dt>
                <dd>{agent.teamsWebhookUrl ? "configured" : "—"}</dd>
                <dt>Pipelines</dt>
                <dd>{agent.pipelines.map((p) => p.name).join(", ") || "—"}</dd>
                <dt>ServiceNow</dt>
                <dd>{agent.servicenow.enabled ? <code>{agent.servicenow.query || "all incidents"}</code> : "off"}</dd>
                <dt>Splunk</dt>
                <dd>{agent.splunk.enabled ? <code>{agent.splunk.baseSearch}</code> : "off"}</dd>
              </dl>
            </section>
            <section className="panel tight">
              <PanelHead title="Repos" icon={FolderGit2} count={agent.repos.length} />
              <Table>
                <tbody>
                  {agent.repos.map((r) => (
                    <tr key={r.fullName}>
                      <td>
                        <strong className="mono">{r.fullName}</strong>
                        <div className="cell-sub">
                          {r.branches.join(", ") || "default branch"} · {r.paths.length ? `${r.paths.length} path rules` : "all files"}
                        </div>
                        {r.notes && <div className="cell-sub">{r.notes}</div>}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </Table>
            </section>
          </div>
        </>
      )}

      {tab === "changes" && <ChangesTab agentId={agent._id} showAll={sp.show === "all"} id={id} />}
      {tab === "runs" && <RunsTab agentId={agent._id} />}
      {tab === "wisdom" && <WisdomTab agentId={agent._id} id={id} />}
      {tab === "incidents" && <IncidentsTab agentId={agent._id} id={id} />}

      {tab === "setup" && (
        <>
          {agent.mode !== "poll" && (
            <section className="panel">
              <PanelHead title="Install the workflow in each repo" icon={Workflow} />
              <ol style={{ margin: "0 0 16px", paddingLeft: 20 }}>
                <li>
                  Add a secret <code>WISDOM_TOKEN</code> with the agent&apos;s ingest token (repo or org level).
                </li>
                <li>
                  If runners can&apos;t reach <code>{env.appBaseUrl}</code>, set variable <code>WISDOM_URL</code> to an address they can,
                  and <code>WISDOM_RUNNER</code> to a self-hosted runner label if needed.
                </li>
                <li>
                  Commit the file below as <code>.github/workflows/wisdom-ops.yml</code> on each watched branch.
                </li>
              </ol>
              {agent.repos.map((r) => (
                <div key={r.fullName} style={{ marginBottom: 16 }}>
                  <h3>{r.fullName}</h3>
                  <CopyBox text={workflowYaml(agent, r, env.appBaseUrl)} filename="wisdom-ops.yml" />
                </div>
              ))}
            </section>
          )}
          <section className="panel">
            <PanelHead title="Ingest token" icon={KeyRound} />
            <p>
              Current token ends in <code>…{agent.ingestTokenHint}</code>. Agent id <code>{id}</code>
            </p>
            <RotateToken agentId={id} />
          </section>
        </>
      )}
    </>
  );
}

function firstLine(md?: string) {
  return md?.split("\n").find((l) => l.trim())?.replace(/^[-*]\s*/, "").replace(/`/g, "");
}

async function ChangesTab({ agentId, showAll, id }: { agentId: ObjectId; showAll: boolean; id: string }) {
  const { changes } = await collections();
  const filter = showAll ? { agentId } : { agentId, status: { $ne: "filtered" as const } };
  const list = await changes.find(filter).sort({ receivedAt: -1 }).limit(100).toArray();
  return (
    <section className="panel tight">
      <PanelHead title="Changes" icon={GitCommitHorizontal} count={list.length}>
        <Link className="btn small ghost" href={`/agents/${id}?tab=changes${showAll ? "" : "&show=all"}`}>
          {showAll ? "Hide path-filtered" : "Include path-filtered"}
        </Link>
      </PanelHead>
      {list.length ? (
        <Table label="Changes">
          <thead>
            <tr>
              <th>When</th>
              <th>Repo</th>
              <th>Summary</th>
              <th>Risk</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {list.map((c) => (
              <tr key={String(c._id)}>
                <td className="nowrap">
                  {ago(c.receivedAt)}
                  <div className="cell-sub">{c.source}</div>
                </td>
                <td className="nowrap">
                  <Link href={`/changes/${c._id}`}>{c.repo}</Link>
                  <div className="cell-sub mono">
                    {c.branch} · {shortSha(c.after)}
                  </div>
                </td>
                <td style={{ maxWidth: 520 }}>{(firstLine(c.triage?.summary) ?? c.commits[0]?.message.split("\n")[0] ?? "—").slice(0, 180)}</td>
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
        <Empty icon={GitCommitHorizontal}>No changes recorded yet.</Empty>
      )}
    </section>
  );
}

async function RunsTab({ agentId }: { agentId: ObjectId }) {
  const { runs } = await collections();
  const list = await runs.find({ agentId }).sort({ triggeredAt: -1 }).limit(100).toArray();
  return (
    <section className="panel tight">
      <PanelHead title="Pipeline runs" icon={Workflow} count={list.length} />
      {list.length ? (
        <Table label="Pipeline runs">
          <thead>
            <tr>
              <th>Triggered</th>
              <th>Pipeline</th>
              <th>Build</th>
              <th className="num">Tests failed</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {list.map((r) => (
              <tr key={String(r._id)}>
                <td className="nowrap">{ago(r.triggeredAt)}</td>
                <td>
                  <Link href={`/changes/${r.changeId}`}>{r.pipelineName}</Link>
                  <div className="cell-sub">{r.type}</div>
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
                <td className="num mono">{r.tests ? `${r.tests.failed} / ${r.tests.total}` : "—"}</td>
                <td>
                  <RunBadge status={r.status} />
                  {r.error && <div className="cell-sub">{r.error.slice(0, 120)}</div>}
                </td>
              </tr>
            ))}
          </tbody>
        </Table>
      ) : (
        <Empty icon={Workflow}>No pipeline runs yet.</Empty>
      )}
    </section>
  );
}

async function WisdomTab({ agentId, id }: { agentId: ObjectId; id: string }) {
  const w = await latestWisdom(agentId);
  return (
    <>
      <section className="panel">
        <PanelHead title="Module wisdom" icon={BookOpen}>
          {w && <span className="hint" style={{ margin: 0 }}>Generated {ago(w.generatedAt)}</span>}
          <ActionButton
            url={`/api/agents/${id}/wisdom`}
            label={w ? "Regenerate" : "Generate now"}
            busyLabel="Reading repos… (1–2 min)"
            icon="sparkles"
          />
        </PanelHead>
        {w ? <Md>{w.moduleSummary}</Md> : <p className="hint">Not generated yet. The worker builds it daily, or generate it now.</p>}
      </section>
      {w?.repos.map((r) => (
        <section className="panel" key={r.repo}>
          <PanelHead title={r.repo} icon={FolderGit2} />
          <p>{r.purpose}</p>
          <div className="grid g2">
            <div>
              <h3>Key components</h3>
              <ul>{r.keyComponents.map((k, i) => <li key={i}>{k}</li>)}</ul>
              <h3 style={{ marginTop: 16 }}>Hotspots</h3>
              <ul>{r.hotspots.map((k, i) => <li key={i}>{k}</li>)}</ul>
            </div>
            <div>
              <h3>Recent activity</h3>
              <Md>{r.recentActivity}</Md>
              <h3 style={{ marginTop: 16 }}>Risks</h3>
              <ul>{r.risks.map((k, i) => <li key={i}>{k}</li>)}</ul>
              <h3 style={{ marginTop: 16 }}>Testing notes</h3>
              <Md>{r.testingNotes}</Md>
            </div>
          </div>
        </section>
      ))}
    </>
  );
}

async function IncidentsTab({ agentId, id }: { agentId: ObjectId; id: string }) {
  const { incidents } = await collections();
  const list = await incidents.find({ agentId }).sort({ openedAt: -1 }).limit(100).toArray();
  return (
    <section className="panel tight">
      <PanelHead title="Incidents" icon={Siren} count={list.length}>
        <ManualIncident agentId={id} />
      </PanelHead>
      {list.length ? (
        <Table label="Incidents">
          <thead>
            <tr>
              <th>Opened</th>
              <th>Incident</th>
              <th>Top suspect</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {list.map((i) => {
              const top = i.analysis?.likelyCauses[0];
              return (
                <tr key={String(i._id)}>
                  <td className="nowrap">{ago(i.openedAt)}</td>
                  <td>
                    <Link href={`/incidents/${i._id}`} className="mono">
                      {i.number}
                    </Link>
                    <div className="cell-sub">{i.shortDescription}</div>
                  </td>
                  <td>
                    {top ? (
                      <span className="row" style={{ gap: 6 }}>
                        <span className="mono">
                          {top.repo ?? "?"}@{shortSha(top.sha)}
                        </span>
                        <ConfidenceBadge value={top.confidence} />
                      </span>
                    ) : (
                      "—"
                    )}
                  </td>
                  <td>
                    <IncidentBadge status={i.status} />
                  </td>
                </tr>
              );
            })}
          </tbody>
        </Table>
      ) : (
        <Empty icon={Siren}>No incidents yet.</Empty>
      )}
    </section>
  );
}
