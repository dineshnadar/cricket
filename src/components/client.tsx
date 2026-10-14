"use client";

import { useRouter } from "next/navigation";
import { useState } from "react";
import Markdown from "react-markdown";
import {
  Check,
  CircleAlert,
  Copy,
  Download,
  KeyRound,
  LoaderCircle,
  Plus,
  RefreshCw,
  Send,
  Siren,
  Sparkles,
  Trash2,
  type LucideIcon,
} from "lucide-react";

const icons = {
  refresh: RefreshCw,
  trash: Trash2,
  sparkles: Sparkles,
  siren: Siren,
} satisfies Record<string, LucideIcon>;

/** POSTs (or DELETEs) to an endpoint, then refreshes or navigates. */
export function ActionButton(props: {
  url: string;
  label: string;
  busyLabel?: string;
  method?: "POST" | "DELETE";
  confirm?: string;
  redirectTo?: string;
  className?: string;
  icon?: keyof typeof icons;
}) {
  const router = useRouter();
  const [busy, setBusy] = useState(false);
  const [msg, setMsg] = useState("");
  const Icon = props.icon ? icons[props.icon] : null;
  return (
    <span className="row">
      <button
        className={`btn ${props.className ?? ""}`}
        disabled={busy}
        aria-busy={busy}
        onClick={async () => {
          if (props.confirm && !window.confirm(props.confirm)) return;
          setBusy(true);
          setMsg("");
          try {
            const res = await fetch(props.url, { method: props.method ?? "POST" });
            const j = await res.json().catch(() => ({}));
            if (!res.ok) setMsg(j.error ?? `Failed (${res.status})`);
            else if (props.redirectTo) router.push(props.redirectTo);
            else router.refresh();
          } catch (e) {
            setMsg(String(e));
          } finally {
            setBusy(false);
          }
        }}
      >
        {busy ? <LoaderCircle size={15} className="spin" aria-hidden /> : Icon && <Icon size={15} aria-hidden />}
        {busy ? (props.busyLabel ?? "Working…") : props.label}
      </button>
      {msg && (
        <span className="inline-error" role="alert">
          <CircleAlert size={14} aria-hidden />
          {msg}
        </span>
      )}
    </span>
  );
}

export function CopyBox({ text, filename }: { text: string; filename?: string }) {
  const [copied, setCopied] = useState(false);
  return (
    <div className="codebox">
      <div className="codebox-actions">
        {filename && (
          <a className="btn small" href={`data:text/yaml;charset=utf-8,${encodeURIComponent(text)}`} download={filename}>
            <Download size={14} aria-hidden /> Download
          </a>
        )}
        <button
          className="btn small"
          onClick={async () => {
            await navigator.clipboard.writeText(text);
            setCopied(true);
            setTimeout(() => setCopied(false), 1500);
          }}
        >
          {copied ? <Check size={14} aria-hidden /> : <Copy size={14} aria-hidden />}
          <span aria-live="polite">{copied ? "Copied" : "Copy"}</span>
        </button>
      </div>
      <pre>{text}</pre>
    </div>
  );
}

export function RotateToken({ agentId }: { agentId: string }) {
  const [token, setToken] = useState("");
  const [busy, setBusy] = useState(false);
  return token ? (
    <>
      <div className="notice">
        <KeyRound size={16} aria-hidden />
        <div>
          New token. Store it as the <code>WISDOM_TOKEN</code> secret now: it won&apos;t be shown again, and the old token has
          already stopped working.
        </div>
      </div>
      <CopyBox text={token} />
    </>
  ) : (
    <button
      className="btn"
      disabled={busy}
      onClick={async () => {
        if (!window.confirm("Rotate the ingest token? Every repo's WISDOM_TOKEN secret must be updated.")) return;
        setBusy(true);
        const res = await fetch(`/api/agents/${agentId}/token`, { method: "POST" });
        const j = await res.json();
        setBusy(false);
        if (res.ok) setToken(j.token);
      }}
    >
      {busy ? <LoaderCircle size={15} className="spin" aria-hidden /> : <KeyRound size={15} aria-hidden />}
      Rotate ingest token
    </button>
  );
}

/** Turn [change:<id>] / [incident:<id>] citations into links. */
function linkify(s: string) {
  return s
    .replace(/\[change:([a-f0-9]{24})\]/g, "[change](/changes/$1)")
    .replace(/\[incident:([a-f0-9]{24})\]/g, "[incident](/incidents/$1)");
}

const SUGGESTIONS = ["What changed this week?", "Which repo is riskiest right now?", "Why did the last regression fail?"];

export function AskBox({ agentId, module }: { agentId: string; module: string }) {
  const [q, setQ] = useState("");
  const [answer, setAnswer] = useState("");
  const [err, setErr] = useState("");
  const [busy, setBusy] = useState(false);

  const ask = async (question: string) => {
    if (!question.trim()) return;
    setBusy(true);
    setErr("");
    setAnswer("");
    try {
      const res = await fetch(`/api/agents/${agentId}/ask`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ question }),
      });
      const j = await res.json();
      if (res.ok) setAnswer(j.answer);
      else setErr(j.error ?? "Failed");
    } catch (e) {
      setErr(String(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <div>
      <form
        className="row"
        onSubmit={(e) => {
          e.preventDefault();
          void ask(q);
        }}
      >
        <label htmlFor="ask" className="sr-only">
          Ask about {module}
        </label>
        <input
          id="ask"
          style={{ flex: 1, minWidth: 240 }}
          placeholder={`Ask about ${module}…`}
          value={q}
          onChange={(e) => setQ(e.target.value)}
        />
        <button className="btn primary" disabled={busy || !q.trim()} aria-busy={busy}>
          {busy ? <LoaderCircle size={15} className="spin" aria-hidden /> : <Send size={15} aria-hidden />}
          {busy ? "Thinking…" : "Ask"}
        </button>
      </form>
      {!answer && !busy && (
        <div className="chips suggestions" style={{ marginTop: 10 }}>
          {SUGGESTIONS.map((s) => (
            <button
              key={s}
              type="button"
              className="btn small"
              onClick={() => {
                setQ(s);
                void ask(s);
              }}
            >
              {s}
            </button>
          ))}
        </div>
      )}
      {err && (
        <div className="error" role="alert" style={{ marginTop: 12, marginBottom: 0 }}>
          <CircleAlert size={16} aria-hidden />
          {err}
        </div>
      )}
      {answer && (
        <div className="card md" style={{ marginTop: 12, marginBottom: 0 }} aria-live="polite">
          <Markdown components={{ a: ({ href, children }) => <a href={href}>{children}</a> }}>{linkify(answer)}</Markdown>
        </div>
      )}
    </div>
  );
}

export function ManualIncident({ agentId }: { agentId: string }) {
  const router = useRouter();
  const [open, setOpen] = useState(false);
  const [f, setF] = useState({ number: "", shortDescription: "", description: "", openedAt: "" });
  const [err, setErr] = useState("");
  const [busy, setBusy] = useState(false);
  if (!open)
    return (
      <button className="btn" onClick={() => setOpen(true)}>
        <Plus size={15} aria-hidden /> Log incident
      </button>
    );
  return (
    <form
      className="card"
      style={{ width: "100%", marginBottom: 0 }}
      onSubmit={async (e) => {
        e.preventDefault();
        setBusy(true);
        const res = await fetch(`/api/agents/${agentId}/incidents`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ ...f, openedAt: f.openedAt ? new Date(f.openedAt).toISOString() : undefined }),
        });
        const j = await res.json();
        setBusy(false);
        if (!res.ok) return setErr(j.error ?? "Failed");
        setOpen(false);
        router.push(`/incidents/${j.id}`);
      }}
    >
      {err && (
        <div className="error" role="alert">
          <CircleAlert size={16} aria-hidden />
          {err}
        </div>
      )}
      <div className="grid g2">
        <div className="field">
          <label htmlFor="inc-num">Incident number</label>
          <input id="inc-num" required value={f.number} onChange={(e) => setF({ ...f, number: e.target.value })} placeholder="INC0012345" />
        </div>
        <div className="field">
          <label htmlFor="inc-at">Opened at</label>
          <input id="inc-at" type="datetime-local" value={f.openedAt} onChange={(e) => setF({ ...f, openedAt: e.target.value })} />
        </div>
      </div>
      <div className="field">
        <label htmlFor="inc-sd">Short description</label>
        <input id="inc-sd" required value={f.shortDescription} onChange={(e) => setF({ ...f, shortDescription: e.target.value })} />
      </div>
      <div className="field">
        <label htmlFor="inc-d">Error details</label>
        <textarea id="inc-d" value={f.description} onChange={(e) => setF({ ...f, description: e.target.value })} placeholder="Stack trace, error codes, affected endpoint…" />
        <div className="hint">Wisdom AI pulls log search terms from this, so paste exact error text.</div>
      </div>
      <div className="row">
        <button className="btn primary" disabled={busy}>
          {busy ? <LoaderCircle size={15} className="spin" aria-hidden /> : <Siren size={15} aria-hidden />}
          Create and analyse
        </button>
        <button type="button" className="btn ghost" onClick={() => setOpen(false)}>
          Cancel
        </button>
      </div>
    </form>
  );
}
