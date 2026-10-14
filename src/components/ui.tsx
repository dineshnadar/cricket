import Markdown from "react-markdown";
import {
  Ban,
  CircleCheck,
  CircleDashed,
  CircleMinus,
  CircleX,
  Clock,
  Filter,
  Inbox,
  LoaderCircle,
  ShieldAlert,
  ShieldCheck,
  ShieldQuestion,
  TriangleAlert,
  type LucideIcon,
} from "lucide-react";
import type { ChangeStatus, Incident, RiskLevel, RunStatus } from "@/lib/types";

type Tone = "good" | "warn" | "bad" | "info" | "muted";

/** Status pill: always icon + text, never colour alone. */
export function Badge({ tone, icon: Icon, spin, children }: { tone: Tone; icon?: LucideIcon; spin?: boolean; children: React.ReactNode }) {
  return (
    <span className={`badge b-${tone}`}>
      {Icon && <Icon size={13} strokeWidth={2.25} aria-hidden className={spin ? "spin" : undefined} />}
      {children}
    </span>
  );
}

const changeMeta: Record<ChangeStatus, [Tone, LucideIcon, string, boolean?]> = {
  received: ["info", Clock, "queued"],
  processing: ["info", LoaderCircle, "triaging", true],
  filtered: ["muted", Filter, "path-filtered"],
  "not-relevant": ["muted", CircleMinus, "not relevant"],
  actioned: ["good", CircleCheck, "actioned"],
  error: ["bad", CircleX, "error"],
};
export function ChangeStatusBadge({ status }: { status: ChangeStatus }) {
  const [tone, icon, label, spin] = changeMeta[status];
  return (
    <Badge tone={tone} icon={icon} spin={spin}>
      {label}
    </Badge>
  );
}

const riskMeta: Record<RiskLevel, [Tone, LucideIcon]> = {
  low: ["good", ShieldCheck],
  medium: ["warn", ShieldQuestion],
  high: ["bad", ShieldAlert],
};
export function RiskBadge({ risk }: { risk?: RiskLevel }) {
  if (!risk) return <span className="icon-muted" aria-label="no risk rating">—</span>;
  const [tone, icon] = riskMeta[risk];
  return (
    <Badge tone={tone} icon={icon}>
      {risk} risk
    </Badge>
  );
}

const runMeta: Record<RunStatus, [Tone, LucideIcon, boolean?]> = {
  queued: ["info", Clock],
  running: ["info", LoaderCircle, true],
  success: ["good", CircleCheck],
  unstable: ["warn", TriangleAlert],
  failure: ["bad", CircleX],
  aborted: ["muted", Ban],
  error: ["bad", CircleX],
};
export function RunBadge({ status }: { status: RunStatus }) {
  const [tone, icon, spin] = runMeta[status];
  return (
    <Badge tone={tone} icon={icon} spin={spin}>
      {status}
    </Badge>
  );
}

const incMeta: Record<Incident["status"], [Tone, LucideIcon, boolean?]> = {
  new: ["info", CircleDashed],
  analyzing: ["info", LoaderCircle, true],
  analyzed: ["good", CircleCheck],
  error: ["bad", CircleX],
};
export function IncidentBadge({ status }: { status: Incident["status"] }) {
  const [tone, icon, spin] = incMeta[status];
  return (
    <Badge tone={tone} icon={icon} spin={spin}>
      {status}
    </Badge>
  );
}

export function ConfidenceBadge({ value }: { value: number }) {
  const pct = Math.round(value * 100);
  const tone: Tone = value >= 0.7 ? "bad" : value >= 0.4 ? "warn" : "muted";
  return (
    <Badge tone={tone}>
      <span className="mono" style={{ fontSize: 12 }}>{pct}%</span>
    </Badge>
  );
}

export function PanelHead({
  title,
  icon: Icon,
  count,
  children,
}: {
  title: string;
  icon?: LucideIcon;
  count?: number;
  children?: React.ReactNode;
}) {
  return (
    <div className="panel-head">
      <h2>
        {Icon && <Icon size={16} aria-hidden className="icon-muted" />}
        {title}
        {count !== undefined && <span className="count">{count}</span>}
      </h2>
      {children && <div className="row">{children}</div>}
    </div>
  );
}

export function Md({ children }: { children?: string }) {
  if (!children) return null;
  return (
    <div className="md">
      <Markdown>{children}</Markdown>
    </div>
  );
}

export function Diff({ patch }: { patch: string }) {
  return (
    <div className="diff" role="region" aria-label="Diff">
      {patch.split("\n").map((l, i) => (
        <div key={i} className={l.startsWith("@@") ? "hunk" : l.startsWith("+") ? "add" : l.startsWith("-") ? "del" : undefined}>
          {l || " "}
        </div>
      ))}
    </div>
  );
}

export function Empty({ children, icon: Icon = Inbox }: { children: React.ReactNode; icon?: LucideIcon }) {
  return (
    <div className="empty">
      <Icon size={18} aria-hidden />
      <div>{children}</div>
    </div>
  );
}

export function Table({ children, label }: { children: React.ReactNode; label?: string }) {
  return (
    <div className="table-wrap" role="region" aria-label={label} tabIndex={label ? 0 : undefined}>
      <table>{children}</table>
    </div>
  );
}
