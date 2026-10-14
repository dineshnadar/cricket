"use client";

import { useState } from "react";
import { Check, CircleAlert, CircleHelp, LoaderCircle, Sparkles, X } from "lucide-react";
import { fieldQuality, type FieldKind } from "@/lib/field-quality";

export type AssistKind = "module" | "repoNotes" | "paths";

interface Result {
  text?: string;
  paths?: string[];
  questions?: string[];
  counts?: { glob: string; matches: number }[];
  dropped?: string[];
  matched?: number;
  total?: number;
  reasoning?: string;
}

function userHeader(): Record<string, string> {
  try {
    const u = localStorage.getItem("wo-user");
    return u ? { "x-wisdom-user": u } : {};
  } catch {
    return {};
  }
}

async function post(body: unknown) {
  const res = await fetch("/api/agents/assist", {
    method: "POST",
    headers: { "Content-Type": "application/json", ...userHeader() },
    body: JSON.stringify(body),
  });
  const j = await res.json().catch(() => ({}));
  if (!res.ok) throw new Error(j.error ?? `Failed (${res.status})`);
  return j as Result;
}

/** Small "how good is this?" label under a free-text field. */
export function QualityHint({ kind, text }: { kind: FieldKind; text: string }) {
  const q = fieldQuality(kind, text);
  const cls = q.level === "good" ? "b-good" : q.level === "ok" ? "b-info" : "b-warn";
  return (
    <div className="quality" aria-live="polite">
      <span className={`badge ${cls}`}>{q.label}</span>
      <span className="hint" style={{ marginTop: 0 }}>{q.tip}</span>
    </div>
  );
}

/**
 * An "✨ … with AI" button plus the suggestion panel. Nothing changes until the
 * person clicks "Use this". `request` builds the API body from the current form state.
 */
export function AiAssist({
  kind,
  label,
  request,
  current,
  onUse,
  repo,
  disabledReason,
}: {
  kind: AssistKind;
  label: string;
  request: () => Record<string, unknown>;
  current: string;
  onUse: (value: string) => void;
  repo?: string;
  disabledReason?: string;
}) {
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState("");
  const [res, setRes] = useState<Result | null>(null);

  const run = async () => {
    setBusy(true);
    setErr("");
    setRes(null);
    try {
      setRes(await post({ kind, ...request() }));
    } catch (e) {
      setErr(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  };
  const decide = (decision: "used" | "kept-mine") => {
    void post({ kind: "decision", field: kind, repo, decision }).catch(() => {});
    if (decision === "used" && res) onUse(kind === "paths" ? (res.paths ?? []).join("\n") : (res.text ?? ""));
    setRes(null);
  };

  const suggestion = kind === "paths" ? (res?.paths ?? []).join("\n") : (res?.text ?? "");

  return (
    <div className="ai-assist">
      <button type="button" className="btn small" onClick={run} disabled={busy || !!disabledReason} title={disabledReason}>
        {busy ? <LoaderCircle size={14} className="spin" aria-hidden /> : <Sparkles size={14} aria-hidden />}
        {busy ? "Working…" : label}
      </button>
      {disabledReason && <span className="hint" style={{ marginTop: 0 }}>{disabledReason}</span>}
      {err && (
        <div className="error" role="alert" style={{ marginTop: 8 }}>
          <CircleAlert size={16} aria-hidden />
          <div>{err}</div>
        </div>
      )}
      {res && (
        <div className="ai-suggestion" role="region" aria-label="AI suggestion">
          <div className="hint" style={{ marginTop: 0 }}>
            Suggestion. Check it before using; nothing is saved until you click <strong>Save</strong>.
          </div>
          <pre className={kind === "paths" ? "mono" : undefined}>{suggestion || "(nothing suggested)"}</pre>
          {kind === "paths" && res.counts && (
            <div className="hint">
              <ul className="ai-counts">
                {res.counts.map((c) => (
                  <li key={c.glob}>
                    <code>{c.glob}</code> {c.glob.startsWith("!") ? "ignores" : "matches"} {c.matches} {c.matches === 1 ? "file" : "files"}
                  </li>
                ))}
              </ul>
              Watches <strong>{res.matched}</strong> of {res.total} files.
              {res.dropped?.length ? ` Dropped (matched nothing): ${res.dropped.join(", ")}.` : ""}
              {res.reasoning ? ` ${res.reasoning}` : ""}
            </div>
          )}
          {!!res.questions?.length && (
            <ul className="ai-questions">
              {res.questions.map((q) => (
                <li key={q}>
                  <CircleHelp size={14} aria-hidden /> {q}
                </li>
              ))}
            </ul>
          )}
          <div className="row" style={{ marginTop: 10 }}>
            <button type="button" className="btn small primary" onClick={() => decide("used")} disabled={!suggestion}>
              <Check size={14} aria-hidden /> Use this
            </button>
            <button type="button" className="btn small" onClick={() => decide("kept-mine")}>
              <X size={14} aria-hidden /> {current.trim() ? "Keep mine" : "Discard"}
            </button>
          </div>
          {kind !== "paths" && (
            <div className="hint">After using it you can still edit the text, e.g. answer the questions above in it.</div>
          )}
        </div>
      )}
    </div>
  );
}
