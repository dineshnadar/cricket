"use client";

import { useEffect, useId, useState } from "react";
import { Check, CircleAlert, LoaderCircle, ThumbsDown, ThumbsUp } from "lucide-react";

/**
 * "Was this right?" buttons. Each answer becomes an eval label (collection eval_labels),
 * the ground truth `npm run eval` uses to measure Wisdom AI's accuracy.
 */

type Risk = "low" | "medium" | "high";

interface Label {
  expected: { relevant?: boolean; riskLevel?: Risk; causeChangeId?: string | null };
  user: string;
  at: string;
  note?: string;
}

function userHeader(): Record<string, string> {
  try {
    const u = localStorage.getItem("wo-user");
    return u ? { "x-wisdom-user": u } : {};
  } catch {
    return {};
  }
}

function useLabel(kind: "triage" | "incident", targetId: string) {
  const [label, setLabel] = useState<Label | null | undefined>(undefined);
  useEffect(() => {
    fetch(`/api/evals/labels?kind=${kind}&targetId=${targetId}`)
      .then((r) => (r.ok ? r.json() : null))
      .then(setLabel)
      .catch(() => setLabel(null));
  }, [kind, targetId]);
  const save = async (body: Record<string, unknown>) => {
    const res = await fetch("/api/evals/labels", {
      method: "POST",
      headers: { "Content-Type": "application/json", ...userHeader() },
      body: JSON.stringify({ kind, targetId, ...body }),
    });
    const j = await res.json().catch(() => ({}));
    if (!res.ok) throw new Error(j.error ?? `Failed (${res.status})`);
    setLabel(j);
  };
  return { label, save };
}

function Saved({ text, label, onChange, plain }: { text: string; label: Label; onChange: () => void; plain?: boolean }) {
  return (
    <div className={`feedback feedback-done${plain ? " plain" : ""}`} role="status">
      <Check size={15} aria-hidden />
      <span>
        {text} <span className="hint" style={{ margin: 0 }}>· {label.user} · {new Date(label.at).toLocaleDateString()}</span>
      </span>
      <button type="button" className="linklike" onClick={onChange}>
        Change
      </button>
    </div>
  );
}

const FOOT = "Your answer is used to measure Wisdom AI's accuracy (see docs/10-evals.md).";

export function TriageFeedback({ changeId, aiRelevant, aiRisk }: { changeId: string; aiRelevant: boolean; aiRisk: Risk }) {
  const uid = useId();
  const { label, save } = useLabel("triage", changeId);
  const [mode, setMode] = useState<"ask" | "wrong">("ask");
  const [relevant, setRelevant] = useState(!aiRelevant);
  const [risk, setRisk] = useState<Risk | "">("");
  const [note, setNote] = useState("");
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState("");
  const [editing, setEditing] = useState(false);

  const submit = async (body: Record<string, unknown>) => {
    setBusy(true);
    setErr("");
    try {
      await save(body);
      setEditing(false);
      setMode("ask");
    } catch (e) {
      setErr(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  };

  if (label === undefined) return null;
  if (label && !editing) {
    const e = label.expected;
    const right = e.relevant === aiRelevant && (!e.riskLevel || e.riskLevel === aiRisk);
    const text = right
      ? "Marked correct."
      : `Marked wrong: should be ${e.relevant ? "relevant" : "not relevant"}${e.riskLevel ? `, ${e.riskLevel} risk` : ""}.`;
    return <Saved text={text} label={label} onChange={() => setEditing(true)} />;
  }

  return (
    <div className="feedback" id="feedback">
      <div className="feedback-row">
        <strong>Was this triage right?</strong>
        <button type="button" className="btn small" disabled={busy} onClick={() => submit({ expected: { relevant: aiRelevant, riskLevel: aiRisk } })}>
          {busy && mode === "ask" ? <LoaderCircle size={14} className="spin" aria-hidden /> : <ThumbsUp size={14} aria-hidden />} Yes
        </button>
        <button type="button" className="btn small" aria-expanded={mode === "wrong"} onClick={() => setMode(mode === "wrong" ? "ask" : "wrong")}>
          <ThumbsDown size={14} aria-hidden /> No
        </button>
      </div>
      {mode === "wrong" && (
        <div className="feedback-form">
          <fieldset>
            <legend>This change should be</legend>
            <label className="check">
              <input type="radio" name={uid + "-rel"} checked={relevant} onChange={() => setRelevant(true)} /> Relevant to the module
            </label>
            <label className="check">
              <input type="radio" name={uid + "-rel"} checked={!relevant} onChange={() => setRelevant(false)} /> Not relevant
            </label>
          </fieldset>
          <div className="grid g2">
            <div className="field" style={{ marginBottom: 0 }}>
              <label htmlFor={uid + "-risk"}>Risk should be (optional)</label>
              <select id={uid + "-risk"} value={risk} onChange={(e) => setRisk(e.target.value as Risk | "")}>
                <option value="">Leave as is ({aiRisk})</option>
                <option value="low">Low</option>
                <option value="medium">Medium</option>
                <option value="high">High</option>
              </select>
            </div>
            <div className="field" style={{ marginBottom: 0 }}>
              <label htmlFor={uid + "-note"}>Why (optional)</label>
              <input id={uid + "-note"} value={note} onChange={(e) => setNote(e.target.value)} placeholder="e.g. touches the CIP vendor client" />
            </div>
          </div>
          <div className="row">
            <button type="button" className="btn small primary" disabled={busy} onClick={() => submit({ expected: { relevant, ...(risk ? { riskLevel: risk } : {}) }, note: note || undefined })}>
              {busy ? <LoaderCircle size={14} className="spin" aria-hidden /> : <Check size={14} aria-hidden />} Save
            </button>
          </div>
        </div>
      )}
      {err && (
        <p className="field-error">
          <CircleAlert size={14} aria-hidden /> {err}
        </p>
      )}
      <p className="hint">{FOOT}</p>
    </div>
  );
}

export function IncidentFeedback({
  incidentId,
  causes,
}: {
  incidentId: string;
  causes: { changeId: string; label: string }[];
}) {
  const uid = useId();
  const { label, save } = useLabel("incident", incidentId);
  const [choice, setChoice] = useState<string>(causes[0]?.changeId ?? "none");
  const [other, setOther] = useState("");
  const [note, setNote] = useState("");
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState("");
  const [editing, setEditing] = useState(false);

  if (label === undefined) return null;
  if (label && !editing) {
    const id = label.expected.causeChangeId;
    const known = causes.find((c) => c.changeId === id);
    const text = id ? `Actual cause recorded: ${known ? known.label : id}.` : "Recorded: not caused by a code change.";
    return <Saved text={text} label={label} onChange={() => setEditing(true)} plain />;
  }

  const submit = async () => {
    const causeChangeId = choice === "none" ? null : choice === "other" ? other.trim() : choice;
    if (choice === "other" && !/^[a-f0-9]{24}$/.test(causeChangeId ?? "")) {
      setErr("Paste the change id from the change page's address (24 characters).");
      return;
    }
    setBusy(true);
    setErr("");
    try {
      await save({ expected: { causeChangeId }, note: note || undefined });
      setEditing(false);
    } catch (e) {
      setErr(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="feedback plain" id="feedback">
      <fieldset>
        <legend>
          <strong>Once you know: what actually caused this?</strong>
        </legend>
        {causes.map((c, i) => (
          <label className="check" key={c.changeId}>
            <input type="radio" name={uid} checked={choice === c.changeId} onChange={() => setChoice(c.changeId)} /> Likely cause #{i + 1}: {c.label}
          </label>
        ))}
        <label className="check">
          <input type="radio" name={uid} checked={choice === "other"} onChange={() => setChoice("other")} /> A different change
        </label>
        {choice === "other" && (
          <div className="field" style={{ margin: "4px 0 8px 30px" }}>
            <label htmlFor={uid + "-other"}>Change id</label>
            <input id={uid + "-other"} className="mono" value={other} onChange={(e) => setOther(e.target.value)} placeholder="6acea6e46cf1a3ce6802c6e3" />
          </div>
        )}
        <label className="check">
          <input type="radio" name={uid} checked={choice === "none"} onChange={() => setChoice("none")} /> Not caused by a code change (config, data, infra, vendor…)
        </label>
      </fieldset>
      <div className="field" style={{ margin: "8px 0" }}>
        <label htmlFor={uid + "-note"}>Note (optional)</label>
        <input id={uid + "-note"} value={note} onChange={(e) => setNote(e.target.value)} placeholder="e.g. vendor outage, not our code" />
      </div>
      <button type="button" className="btn small primary" disabled={busy} onClick={submit}>
        {busy ? <LoaderCircle size={14} className="spin" aria-hidden /> : <Check size={14} aria-hidden />} Save
      </button>
      {err && (
        <p className="field-error">
          <CircleAlert size={14} aria-hidden /> {err}
        </p>
      )}
      <p className="hint">{FOOT}</p>
    </div>
  );
}
