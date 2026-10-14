"use client";

import Link from "next/link";
import { useRouter } from "next/navigation";
import { useCallback, useEffect, useId, useRef, useState } from "react";
import type { AgentInput } from "@/lib/schemas";
import { fieldId, firstInvalidStep, reviewWarnings, STEPS, stepForServerError, stepSummary, validateStep, type FieldError, type StepId } from "@/lib/agent-steps";
import type { PipelineConfig, WatchedRepo } from "@/lib/types";
import { AiAssist, QualityHint } from "./AiAssist";
import { CopyBox } from "./client";
import { ArrowLeft, ArrowRight, Check, ChevronRight, CircleAlert, LoaderCircle, Pencil, Plus, RotateCcw, Save, Trash2, TriangleAlert } from "lucide-react";

const lines = (s: string) =>
  s
    .split(/[\n,]/)
    .map((x) => x.trim())
    .filter(Boolean);

const blankPipeline = (): PipelineConfig => ({
  name: "",
  type: "jenkins",
  jobPath: "",
  params: {},
  runWhen: "always",
  onlyRepos: [],
});

export const defaultAgent: AgentInput = {
  name: "",
  module: "",
  description: "",
  repos: [],
  mode: "action",
  pollingIntervalMinutes: 10,
  emailRecipients: [],
  teamsWebhookUrl: "",
  relevanceThreshold: 0.6,
  pipelines: [],
  notifyOnPipeline: "failure",
  servicenow: { enabled: false, query: "", postWorkNotes: false },
  splunk: { enabled: false, baseSearch: "", windowBeforeMinutes: 120, windowAfterMinutes: 30, maxEvents: 50 },
  enabled: true,
};

/** Inline error under a field, linked with aria-describedby. */
function FieldErr({ errors, id }: { errors: FieldError[]; id: string }) {
  const e = errors.find((x) => x.field === id);
  return e ? (
    <p className="field-error" id={`${id}-err`}>
      <CircleAlert size={14} aria-hidden /> {e.message}
    </p>
  ) : null;
}
const errProps = (errors: FieldError[], id: string) =>
  errors.some((x) => x.field === id) ? { "aria-invalid": true as const, "aria-describedby": `${id}-err` } : {};

interface RepoHit {
  fullName: string;
  description: string | null;
  defaultBranch: string;
}

function RepoPicker({ onAdd, existing, errors }: { onAdd: (r: WatchedRepo) => void; existing: string[]; errors: FieldError[] }) {
  const uid = useId();
  const [q, setQ] = useState("");
  const [hits, setHits] = useState<RepoHit[]>([]);
  const [err, setErr] = useState("");
  const [manual, setManual] = useState("");

  useEffect(() => {
    if (q.trim().length < 2) {
      setHits([]);
      return;
    }
    const t = setTimeout(async () => {
      try {
        const res = await fetch(`/api/github/repos?q=${encodeURIComponent(q)}`);
        const j = await res.json();
        if (!res.ok) throw new Error(j.error);
        setHits(j);
        setErr("");
      } catch (e) {
        setErr(`Repo search unavailable (${e instanceof Error ? e.message : e}). Add repos by name below.`);
      }
    }, 300);
    return () => clearTimeout(t);
  }, [q]);

  const add = (fullName: string, defaultBranch?: string) => {
    if (existing.includes(fullName.toLowerCase())) return;
    onAdd({ fullName, branches: defaultBranch ? [defaultBranch] : [], paths: [], notes: "" });
    setQ("");
    setHits([]);
  };

  return (
    <div className="card" style={{ background: "var(--color-surface)" }}>
      <label htmlFor={fieldId.repos}>Add repos</label>
      <input id={fieldId.repos} type="search" placeholder="Search GitHub Enterprise repos…" value={q} onChange={(e) => setQ(e.target.value)} {...errProps(errors, fieldId.repos)} />
      <FieldErr errors={errors} id={fieldId.repos} />
      {err && (
        <div className="hint" role="status">
          {err}
        </div>
      )}
      {hits.length > 0 && (
        <div className="results">
          {hits.map((h) => (
            <button type="button" key={h.fullName} onClick={() => add(h.fullName, h.defaultBranch)}>
              <strong>{h.fullName}</strong>
              {existing.includes(h.fullName.toLowerCase()) && <span className="hint"> (added)</span>}
              {h.description && <div className="hint">{h.description}</div>}
            </button>
          ))}
        </div>
      )}
      <div className="row" style={{ marginTop: 8 }}>
        <input style={{ flex: 1 }} placeholder="or type owner/name" value={manual} onChange={(e) => setManual(e.target.value)} />
        <button
          type="button"
          className="btn"
          onClick={() => {
            if (/^[\w.-]+\/[\w.-]+$/.test(manual.trim())) {
              add(manual.trim());
              setManual("");
            }
          }}
        >
          <Plus size={15} aria-hidden /> Add
        </button>
      </div>
    </div>
  );
}

const NOTES_PLACEHOLDER = `What it does: POA service; validates and stores powers of attorney.
Critical code: PoaValidator, ExpiryService, /api/poa/*, poa-rules.yml
Watch out for: date and time-zone handling, validation rule changes
Owner: POA platform team

Rough is fine too, e.g. "poa stuff, expiry is fragile". Or leave it empty and click Draft from repo.`;

const PATHS_PLACEHOLDER = `src/main/java/**/poa/**
src/main/resources/application*.yml
!**/*.md

Not sure? Click Suggest paths.`;

function RepoCard({
  repo,
  moduleDescription,
  onChange,
  onRemove,
}: {
  repo: WatchedRepo;
  moduleDescription: string;
  onChange: (r: WatchedRepo) => void;
  onRemove: () => void;
}) {
  const uid = useId();
  const [branches, setBranches] = useState<string[]>([]);
  // Local text while typing; committed on blur or when an AI suggestion is used.
  const [pathsText, setPathsText] = useState(repo.paths.join("\n"));
  const [notes, setNotes] = useState(repo.notes);
  useEffect(() => {
    fetch(`/api/github/branches?repo=${encodeURIComponent(repo.fullName)}`)
      .then((r) => (r.ok ? r.json() : []))
      .then(setBranches)
      .catch(() => {});
  }, [repo.fullName]);
  return (
    <div className="card">
      <div className="spread" style={{ marginBottom: 10 }}>
        <strong className="mono">{repo.fullName}</strong>
        <button type="button" className="btn small danger" onClick={onRemove} aria-label={`Remove ${repo.fullName}`}>
          <Trash2 size={14} aria-hidden /> Remove
        </button>
      </div>
      <div className="grid g2">
        <div className="field">
          <label htmlFor={uid + "-2"}>Branches</label>
          <input id={uid + "-2"}
            list={`br-${repo.fullName}`}
            defaultValue={repo.branches.join(", ")}
            onBlur={(e) => onChange({ ...repo, branches: lines(e.target.value) })}
            placeholder="main, release/*"
          />
          <datalist id={`br-${repo.fullName}`}>
            {branches.map((b) => (
              <option key={b} value={b} />
            ))}
          </datalist>
          <div className="hint">Comma-separated, globs allowed. Empty = default branch.</div>
        </div>
        <div className="field">
          <label htmlFor={uid + "-3"}>Watched paths</label>
          <textarea id={uid + "-3"}
            value={pathsText}
            className="mono"
            style={{ minHeight: 110 }}
            onChange={(e) => setPathsText(e.target.value)}
            onBlur={(e) => onChange({ ...repo, paths: lines(e.target.value) })}
            placeholder={PATHS_PLACEHOLDER}
          />
          <div className="hint">One glob per line; prefix ! to exclude. Empty = every file.</div>
          <AiAssist
            kind="paths"
            label="Suggest paths"
            repo={repo.fullName}
            current={pathsText}
            request={() => ({ repo: repo.fullName, branches: repo.branches, notes, moduleDescription })}
            onUse={(v) => {
              setPathsText(v);
              onChange({ ...repo, paths: lines(v), notes });
            }}
          />
        </div>
      </div>
      <div className="field" style={{ marginBottom: 0 }}>
        <label htmlFor={uid + "-4"}>What matters in this repo</label>
        <textarea id={uid + "-4"}
          value={notes}
          style={{ minHeight: 150 }}
          onChange={(e) => setNotes(e.target.value)}
          onBlur={(e) => onChange({ ...repo, notes: e.target.value })}
          placeholder={NOTES_PLACEHOLDER}
        />
        <QualityHint kind="repoNotes" text={notes} />
        <AiAssist
          kind="repoNotes"
          label={notes.trim() ? "Improve with AI" : "Draft from repo"}
          repo={repo.fullName}
          current={notes}
          request={() => ({ repo: repo.fullName, branches: repo.branches, rough: notes, moduleDescription })}
          onUse={(v) => {
            setNotes(v);
            onChange({ ...repo, notes: v, paths: lines(pathsText) });
          }}
        />
        <div className="hint">Wisdom AI reads this when deciding relevance and summarising. It drafts from the repo&apos;s README, folders and recent commits.</div>
      </div>
    </div>
  );
}

function PipelineCard({
  p,
  index,
  errors,
  repos,
  onChange,
  onRemove,
}: {
  p: PipelineConfig;
  index: number;
  errors: FieldError[];
  repos: string[];
  onChange: (p: PipelineConfig) => void;
  onRemove: () => void;
}) {
  const uid = useId();
  const paramText = Object.entries(p.params)
    .map(([k, v]) => `${k}=${v}`)
    .join("\n");
  return (
    <div className="card">
      <div className="grid g3">
        <div className="field">
          <label htmlFor={fieldId.pipe(index, "name")}>Name</label>
          <input id={fieldId.pipe(index, "name")} {...errProps(errors, fieldId.pipe(index, "name"))} value={p.name} onChange={(e) => onChange({ ...p, name: e.target.value })} placeholder="POA regression" />
          <FieldErr errors={errors} id={fieldId.pipe(index, "name")} />
        </div>
        <div className="field">
          <label htmlFor={uid + "-6"}>Type</label>
          <select id={uid + "-6"} value={p.type} onChange={(e) => onChange({ ...p, type: e.target.value as PipelineConfig["type"] })}>
            <option value="jenkins">Jenkins</option>
            <option value="teamcity">TeamCity</option>
            <option value="github-actions">GitHub Actions (workflow_dispatch)</option>
          </select>
        </div>
        <div className="field">
          <label htmlFor={uid + "-7"}>Run when</label>
          <select id={uid + "-7"} value={p.runWhen} onChange={(e) => onChange({ ...p, runWhen: e.target.value as PipelineConfig["runWhen"] })}>
            <option value="always">Every relevant change</option>
            <option value="recommended">Only when Wisdom AI recommends it</option>
          </select>
        </div>
      </div>
      {p.type === "jenkins" ? (
        <div className="field">
          <label htmlFor={fieldId.pipe(index, "job")}>Jenkins job path</label>
          <input id={fieldId.pipe(index, "job")} {...errProps(errors, fieldId.pipe(index, "job"))}
            value={p.jobPath ?? ""}
            onChange={(e) => onChange({ ...p, jobPath: e.target.value })}
            placeholder="discretion/poa-regression  (or a full job URL)"
          />
          <FieldErr errors={errors} id={fieldId.pipe(index, "job")} />
        </div>
      ) : p.type === "teamcity" ? (
        <div className="grid g2">
          <div className="field">
            <label htmlFor={fieldId.pipe(index, "build")}>TeamCity build configuration id</label>
            <input id={fieldId.pipe(index, "build")} {...errProps(errors, fieldId.pipe(index, "build"))}
              className="mono"
              value={p.buildTypeId ?? ""}
              onChange={(e) => onChange({ ...p, buildTypeId: e.target.value })}
              placeholder="Discretion_PoaRegression"
            />
            <FieldErr errors={errors} id={fieldId.pipe(index, "build")} />
            <div className="hint">From the build configuration&apos;s URL or settings (buildTypeId).</div>
          </div>
          <div className="field">
            <label htmlFor={uid + "-tc2"}>Branch</label>
            <select id={uid + "-tc2"}
              value={p.ref === "$CHANGE_BRANCH" ? "$CHANGE_BRANCH" : p.ref ? "custom" : ""}
              onChange={(e) => onChange({ ...p, ref: e.target.value === "custom" ? "main" : e.target.value })}
            >
              <option value="">Build configuration default</option>
              <option value="$CHANGE_BRANCH">Same branch as the push</option>
              <option value="custom">A fixed branch…</option>
            </select>
            {p.ref && p.ref !== "$CHANGE_BRANCH" && (
              <input aria-label="Fixed branch name" style={{ marginTop: 8 }} value={p.ref} onChange={(e) => onChange({ ...p, ref: e.target.value })} placeholder="main" />
            )}
          </div>
          <div className="field">
            <label htmlFor={uid + "-tc3"}>Send parameters as</label>
            <select id={uid + "-tc3"}
              value={p.paramPrefix ?? "env."}
              onChange={(e) => onChange({ ...p, paramPrefix: e.target.value as PipelineConfig["paramPrefix"] })}
            >
              <option value="env.">Environment variables (env.NAME)</option>
              <option value="system.">System properties (system.NAME)</option>
              <option value="">Configuration parameters (NAME)</option>
            </select>
          </div>
        </div>
      ) : (
        <div className="grid g3">
          <div className="field">
            <label htmlFor={fieldId.pipe(index, "repo")}>Workflow repo</label>
            <input id={fieldId.pipe(index, "repo")} {...errProps(errors, fieldId.pipe(index, "repo"))} value={p.repo ?? ""} onChange={(e) => onChange({ ...p, repo: e.target.value })} placeholder="org/test-harness" />
            <FieldErr errors={errors} id={fieldId.pipe(index, "repo")} />
          </div>
          <div className="field">
            <label htmlFor={fieldId.pipe(index, "workflow")}>Workflow file</label>
            <input id={fieldId.pipe(index, "workflow")} {...errProps(errors, fieldId.pipe(index, "workflow"))} value={p.workflow ?? ""} onChange={(e) => onChange({ ...p, workflow: e.target.value })} placeholder="regression.yml" />
            <FieldErr errors={errors} id={fieldId.pipe(index, "workflow")} />
          </div>
          <div className="field">
            <label htmlFor={uid + "-11"}>Ref</label>
            <input id={uid + "-11"} value={p.ref ?? ""} onChange={(e) => onChange({ ...p, ref: e.target.value })} placeholder="main" />
          </div>
        </div>
      )}
      <div className="grid g2">
        <div className="field">
          <label htmlFor={uid + "-12"}>Parameters</label>
          <textarea id={uid + "-12"}
            className="mono"
            defaultValue={paramText}
            onBlur={(e) =>
              onChange({
                ...p,
                params: Object.fromEntries(
                  e.target.value
                    .split("\n")
                    .map((l) => l.trim())
                    .filter((l) => l.includes("="))
                    .map((l) => [l.slice(0, l.indexOf("=")).trim(), l.slice(l.indexOf("=") + 1).trim()]),
                ),
              })
            }
            placeholder={"SUITE=poa\nENV=qa"}
          />
          <div className="hint">
            KEY=value per line (no dots in names). WISDOM_REPO, WISDOM_SHA, WISDOM_CHANGE_ID etc. are added automatically
            {p.type === "teamcity" ? " as env.WISDOM_*." : "."}
          </div>
        </div>
        <div className="field">
          <label htmlFor={uid + "-13"}>Only for repos</label>
          <select id={uid + "-13"}
            multiple
            value={p.onlyRepos}
            onChange={(e) => onChange({ ...p, onlyRepos: Array.from(e.target.selectedOptions).map((o) => o.value) })}
            style={{ minHeight: 72 }}
          >
            {repos.map((r) => (
              <option key={r} value={r}>
                {r}
              </option>
            ))}
          </select>
          <div className="hint">None selected = any repo.</div>
        </div>
      </div>
      <button type="button" className="btn small danger" onClick={onRemove}>
        <Trash2 size={14} aria-hidden /> Remove pipeline
      </button>
    </div>
  );
}

const MODULE_PLACEHOLDER = `In scope: POA setup and validation; check deposit and processing; card issuance and limits; CIP identity verification for advisory accounts.
Out of scope: statements, marketing pages, reporting, UI copy-only changes.
Critical flows: POA expiry checks; CIP pass/fail decisions; card limit updates.

Rough is fine, e.g. "poa, checks, card limits, cip. not statements". Then click Improve with AI.`;

const DRAFT_KEY = "wo-agent-draft";

const storage = {
  get(): { agent: AgentInput; at: string; visited: StepId[] } | null {
    try {
      const raw = localStorage.getItem(DRAFT_KEY);
      return raw ? JSON.parse(raw) : null;
    } catch {
      return null;
    }
  },
  set(v: unknown) {
    try {
      localStorage.setItem(DRAFT_KEY, JSON.stringify(v));
    } catch {
      /* storage unavailable: drafts are a convenience only */
    }
  },
  clear() {
    try {
      localStorage.removeItem(DRAFT_KEY);
    } catch {
      /* ignore */
    }
  },
};

const isStep = (v: string | null): v is StepId => !!v && STEPS.some((s) => s.id === v);
const indexOf = (id: StepId) => STEPS.findIndex((s) => s.id === id);
const prefersReducedMotion = () => typeof window !== "undefined" && window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;

/** Is there anything filled in on an optional step? (decides "Skip" vs "Next") */
function stepHasData(step: StepId, a: AgentInput): boolean {
  if (step === "pipelines") return a.pipelines.length > 0;
  if (step === "notifications") return a.emailRecipients.length > 0 || !!a.teamsWebhookUrl.trim();
  if (step === "incidents") return a.servicenow.enabled || a.splunk.enabled;
  return true;
}

export default function AgentForm({ initial, agentId }: { initial?: AgentInput; agentId?: string }) {
  const editing = !!agentId;
  const router = useRouter();
  const [a, setA] = useState<AgentInput>(initial ?? defaultAgent);
  const aRef = useRef(a);
  aRef.current = a;
  const [step, setStep] = useState<StepId>("module");
  const [visited, setVisited] = useState<Set<StepId>>(() => new Set(editing ? STEPS.map((s) => s.id) : ["module"]));
  const [errors, setErrors] = useState<FieldError[]>([]);
  const [serverErr, setServerErr] = useState("");
  const [busy, setBusy] = useState(false);
  const [created, setCreated] = useState<{ id: string; token: string } | null>(null);
  const [draftAt, setDraftAt] = useState<string | null>(null);
  const [emailsText, setEmailsText] = useState((initial ?? defaultAgent).emailRecipients.join(", "));
  const headingRef = useRef<HTMLHeadingElement>(null);
  const summaryRef = useRef<HTMLDivElement>(null);
  const set = <K extends keyof AgentInput>(k: K, v: AgentInput[K]) => setA((prev) => ({ ...prev, [k]: v }));

  // ----- draft (create only): restore once, then autosave -----
  useEffect(() => {
    if (editing) return;
    const d = storage.get();
    if (d?.agent) {
      setA({ ...defaultAgent, ...d.agent });
      setVisited(new Set(d.visited?.length ? d.visited : ["module"]));
      setEmailsText((d.agent.emailRecipients ?? []).join(", "));
      setDraftAt(d.at);
    }
  }, [editing]);
  useEffect(() => {
    if (editing || created) return;
    const t = setTimeout(() => {
      if (JSON.stringify(a) !== JSON.stringify(defaultAgent)) storage.set({ agent: a, at: new Date().toISOString(), visited: [...visited] });
    }, 500);
    // Also save immediately when the tab is closed or reloaded, so the last keystrokes aren't lost.
    const flush = () => {
      if (JSON.stringify(a) !== JSON.stringify(defaultAgent)) storage.set({ agent: a, at: new Date().toISOString(), visited: [...visited] });
    };
    window.addEventListener("pagehide", flush);
    return () => {
      clearTimeout(t);
      window.removeEventListener("pagehide", flush);
    };
  }, [a, visited, editing, created]);

  // ----- step in the URL, so browser Back/Forward move between steps -----
  const go = useCallback((to: StepId, opts: { push?: boolean; errors?: FieldError[] } = {}) => {
    setStep(to);
    setErrors(opts.errors ?? []);
    setVisited((v) => new Set(v).add(to));
    if (opts.push !== false) {
      const u = new URL(window.location.href);
      u.searchParams.set("step", to);
      window.history.pushState({ step: to }, "", u);
    }
    requestAnimationFrame(() => {
      window.scrollTo({ top: 0, behavior: prefersReducedMotion() ? "auto" : "smooth" });
      if (opts.errors?.length) summaryRef.current?.focus();
      else headingRef.current?.focus({ preventScroll: true });
    });
  }, []);
  useEffect(() => {
    const fromUrl = () => new URL(window.location.href).searchParams.get("step");
    const s = fromUrl();
    if (isStep(s)) setStep(s);
    const onPop = () => {
      const p = fromUrl();
      setStep(isStep(p) ? p : "module");
      setErrors([]);
    };
    window.addEventListener("popstate", onPop);
    return () => window.removeEventListener("popstate", onPop);
  }, []);

  const startOver = () => {
    storage.clear();
    setA(defaultAgent);
    setEmailsText("");
    setVisited(new Set(["module"]));
    setDraftAt(null);
    go("module");
  };

  if (created) {
    return (
      <div className="panel">
        <h2>Agent created</h2>
        <p>
          This is the agent&apos;s ingest token. Add it as a secret named <code>WISDOM_TOKEN</code> in each watched repo (or once at
          org level). It is shown only once; you can rotate it later.
        </p>
        <CopyBox text={created.token} />
        <div className="row" style={{ marginTop: 16 }}>
          <Link className="btn primary" href={`/agents/${created.id}?tab=setup`}>
            Next: install the GitHub workflow <ArrowRight size={16} aria-hidden />
          </Link>
        </div>
      </div>
    );
  }

  const idx = indexOf(step);
  const def = STEPS[idx];
  const isReview = step === "review";

  /** Validate the current step; on failure show the error summary and inline errors. */
  const checkCurrent = (): boolean => {
    const errs = validateStep(step, aRef.current);
    if (!errs.length) return true;
    setErrors(errs);
    requestAnimationFrame(() => summaryRef.current?.focus());
    return false;
  };
  const next = () => {
    if (!checkCurrent()) return;
    go(STEPS[idx + 1].id);
  };
  const back = () => go(STEPS[idx - 1].id);
  const jump = (to: StepId) => {
    if (to === step) return;
    // Moving forward in create mode checks the current step first.
    if (!editing && indexOf(to) > idx && !checkCurrent()) return;
    go(to);
  };

  const submit = async () => {
    const bad = firstInvalidStep(aRef.current);
    if (bad) return go(bad, { errors: validateStep(bad, aRef.current) });
    setBusy(true);
    setServerErr("");
    const res = await fetch(agentId ? `/api/agents/${agentId}` : "/api/agents", {
      method: agentId ? "PUT" : "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(aRef.current),
    });
    const j = await res.json().catch(() => ({}));
    setBusy(false);
    if (!res.ok) {
      const msg: string = j.error ?? `Failed (${res.status})`;
      setServerErr(msg);
      go(stepForServerError(msg));
      return;
    }
    if (agentId) {
      router.push(`/agents/${agentId}`);
      router.refresh();
    } else {
      storage.clear();
      setCreated({ id: j.agent._id, token: j.token });
    }
  };

  const furthest = Math.max(...[...visited].map(indexOf));
  const status = (s: StepId): "current" | "done" | "error" | "todo" => {
    if (s === step) return "current";
    if (!visited.has(s)) return "todo";
    if (s !== "review" && validateStep(s, a).length) return "error";
    return "done";
  };

  return (
    <div className="wizard">
      <nav className="stepper" aria-label="Agent setup steps">
        <div className="stepper-progress" aria-hidden>
          <span style={{ width: `${((idx + 1) / STEPS.length) * 100}%` }} />
        </div>
        <ol>
          {STEPS.map((s, i) => {
            const st = status(s.id);
            const reachable = editing || i <= furthest + 1;
            return (
              <li key={s.id} className={`step ${st}`}>
                <button
                  type="button"
                  onClick={() => jump(s.id)}
                  disabled={!reachable}
                  aria-current={st === "current" ? "step" : undefined}
                  aria-label={`Step ${i + 1}: ${s.title}${s.optional ? " (optional)" : ""}${st === "done" ? ", complete" : st === "error" ? ", needs attention" : ""}`}
                >
                  <span className="step-dot" aria-hidden>
                    {st === "done" ? <Check size={13} /> : st === "error" ? <CircleAlert size={13} /> : i + 1}
                  </span>
                  <span className="step-text">
                    <span className="step-title">
                      {s.title}
                      {s.optional && <span className="step-opt">Optional</span>}
                    </span>
                    {s.id !== "review" && <span className="step-sum">{stepSummary(s.id, a)}</span>}
                  </span>
                </button>
              </li>
            );
          })}
        </ol>
      </nav>

      <div className="wizard-main">
        {draftAt && !editing && (
          <div className="notice info" role="status">
            <RotateCcw size={16} aria-hidden />
            <div style={{ flex: 1 }}>
              Draft restored from {new Date(draftAt).toLocaleString([], { dateStyle: "medium", timeStyle: "short" })}. It&apos;s saved in this
              browser as you type.
            </div>
            <button type="button" className="btn small ghost" onClick={startOver}>
              Start over
            </button>
          </div>
        )}

        {serverErr && (
          <div className="error" role="alert">
            <CircleAlert size={16} aria-hidden />
            <div>Couldn&apos;t save: {serverErr}</div>
          </div>
        )}

        {errors.length > 0 && (
          <div className="error error-summary" role="alert" tabIndex={-1} ref={summaryRef} aria-labelledby="err-title">
            <CircleAlert size={16} aria-hidden />
            <div>
              <strong id="err-title">Fix {errors.length === 1 ? "this" : `these ${errors.length} things`} to continue</strong>
              <ul>
                {errors.map((e) => (
                  <li key={e.field + e.message}>
                    <a
                      href={`#${e.field}`}
                      onClick={(ev) => {
                        ev.preventDefault();
                        document.getElementById(e.field)?.focus();
                      }}
                    >
                      {e.message}
                    </a>
                  </li>
                ))}
              </ul>
            </div>
          </div>
        )}

        <section className="panel wizard-step" aria-labelledby="step-heading">
          <div className="step-head">
            <div className="step-count">
              Step {idx + 1} of {STEPS.length}
              {def.optional && " · Optional"}
            </div>
            <h2 id="step-heading" ref={headingRef} tabIndex={-1}>
              {def.title}
            </h2>
            <p className="hint" style={{ marginTop: 2 }}>{def.hint}</p>
          </div>

          {step === "module" && (
            <>
              <div className="grid g2">
                <div className="field">
                  <label htmlFor={fieldId.name}>Agent name</label>
                  <input id={fieldId.name} value={a.name} onChange={(e) => set("name", e.target.value)} placeholder="Discretion watcher" {...errProps(errors, fieldId.name)} />
                  <FieldErr errors={errors} id={fieldId.name} />
                </div>
                <div className="field">
                  <label htmlFor={fieldId.module}>Module name</label>
                  <input id={fieldId.module} value={a.module} onChange={(e) => set("module", e.target.value)} placeholder="Discretion module" {...errProps(errors, fieldId.module)} />
                  <FieldErr errors={errors} id={fieldId.module} />
                </div>
              </div>
              <div className="field" style={{ marginBottom: 0 }}>
                <label htmlFor={fieldId.description}>What belongs to this module</label>
                <textarea
                  id={fieldId.description}
                  style={{ minHeight: 130 }}
                  value={a.description}
                  onChange={(e) => set("description", e.target.value)}
                  placeholder={MODULE_PLACEHOLDER}
                  {...errProps(errors, fieldId.description)}
                />
                <FieldErr errors={errors} id={fieldId.description} />
                <QualityHint kind="module" text={a.description} />
                <AiAssist
                  kind="module"
                  label="Improve with AI"
                  current={a.description}
                  disabledReason={a.description.trim() || a.module.trim() ? undefined : "Type a module name or a rough line first."}
                  request={() => ({ rough: a.description, moduleName: a.module, repos: a.repos.map((r) => ({ fullName: r.fullName, notes: r.notes })) })}
                  onUse={(v) => set("description", v)}
                />
                <div className="hint">Wisdom AI uses this to decide if a change is relevant. Be specific about sub-areas and what&apos;s out of scope.</div>
              </div>
            </>
          )}

          {step === "repos" && (
            <>
              <RepoPicker existing={a.repos.map((r) => r.fullName.toLowerCase())} onAdd={(r) => set("repos", [...a.repos, r])} errors={errors} />
              {a.repos.map((r, i) => (
                <RepoCard
                  key={r.fullName}
                  repo={r}
                  moduleDescription={a.description}
                  onChange={(nr) => set("repos", aRef.current.repos.map((x, j) => (j === i ? nr : x)))}
                  onRemove={() => set("repos", aRef.current.repos.filter((_, j) => j !== i))}
                />
              ))}
              <details className="advanced" open>
                <summary>
                  <ChevronRight size={16} className="chev" aria-hidden />
                  Detection settings
                  <span className="hint" style={{ margin: 0 }}>
                    {a.mode === "action" ? "GitHub Action" : a.mode === "poll" ? "Central poller" : "Action + poller"} · relevance{" "}
                    {Math.round(a.relevanceThreshold * 100)}%
                  </span>
                </summary>
                <div className="grid g2">
                  <div className="field">
                    <label htmlFor="f-mode">How changes are detected</label>
                    <select id="f-mode" value={a.mode} onChange={(e) => set("mode", e.target.value as AgentInput["mode"])}>
                      <option value="action">GitHub Action in each repo (instant)</option>
                      <option value="poll">Central poller (no repo changes needed)</option>
                      <option value="both">Both (poller as safety net)</option>
                    </select>
                    <div className="hint">Use Both for repos with release branches or bot merges.</div>
                  </div>
                  <div className="field">
                    <label htmlFor="f-poll">Polling interval (minutes)</label>
                    <input id="f-poll" type="number" min={1} value={a.pollingIntervalMinutes} disabled={a.mode === "action"} onChange={(e) => set("pollingIntervalMinutes", Number(e.target.value))} />
                  </div>
                </div>
                <div className="field" style={{ marginBottom: 0 }}>
                  <label htmlFor="f-threshold">Relevance threshold: {Math.round(a.relevanceThreshold * 100)}%</label>
                  <input id="f-threshold" type="range" min={0} max={1} step={0.05} value={a.relevanceThreshold} onChange={(e) => set("relevanceThreshold", Number(e.target.value))} />
                  <div className="hint">
                    For every push, Wisdom AI says whether it matters to this module and how sure it is. Pipelines and alerts only fire when it
                    says yes <em>and</em> is at least this sure. Everything else is still recorded on the Changes tab as &quot;not relevant&quot;.
                    Lower it if you&apos;re missing changes; raise it if you get too many alerts.
                  </div>
                </div>
              </details>
            </>
          )}

          {step === "pipelines" && (
            <>
              {!a.pipelines.length && (
                <p className="empty-step">
                  No pipelines yet. Without one, relevant changes are still summarised and announced; no tests run. You can add them later.
                </p>
              )}
              {a.pipelines.map((p, i) => (
                <PipelineCard
                  key={i}
                  p={p}
                  index={i}
                  errors={errors}
                  repos={a.repos.map((r) => r.fullName)}
                  onChange={(np) => set("pipelines", aRef.current.pipelines.map((x, j) => (j === i ? np : x)))}
                  onRemove={() => set("pipelines", aRef.current.pipelines.filter((_, j) => j !== i))}
                />
              ))}
              <button type="button" className="btn" onClick={() => set("pipelines", [...a.pipelines, blankPipeline()])}>
                <Plus size={15} aria-hidden /> Add pipeline
              </button>
            </>
          )}

          {step === "notifications" && (
            <>
              <div className="field">
                <label htmlFor={fieldId.emails}>Email recipients</label>
                <input
                  id={fieldId.emails}
                  value={emailsText}
                  onChange={(e) => {
                    setEmailsText(e.target.value);
                    set("emailRecipients", lines(e.target.value));
                  }}
                  placeholder="discretion-dev@company.com, lead@company.com"
                  {...errProps(errors, fieldId.emails)}
                />
                <FieldErr errors={errors} id={fieldId.emails} />
                <div className="hint">Comma-separated.</div>
              </div>
              <div className="field">
                <label htmlFor={fieldId.teams}>Teams webhook URL</label>
                <input id={fieldId.teams} value={a.teamsWebhookUrl} onChange={(e) => set("teamsWebhookUrl", e.target.value)} placeholder="https://…" {...errProps(errors, fieldId.teams)} />
                <FieldErr errors={errors} id={fieldId.teams} />
                <div className="hint">A Teams Workflows &quot;post to channel when a webhook request is received&quot; URL.</div>
              </div>
              <div className="field" style={{ marginBottom: 0 }}>
                <label htmlFor="f-notify">Pipeline result notifications</label>
                <select id="f-notify" value={a.notifyOnPipeline} onChange={(e) => set("notifyOnPipeline", e.target.value as AgentInput["notifyOnPipeline"])}>
                  <option value="failure">On failure only</option>
                  <option value="always">Always</option>
                  <option value="never">Never</option>
                </select>
              </div>
            </>
          )}

          {step === "incidents" && (
            <>
              <label className="check">
                <input type="checkbox" checked={a.servicenow.enabled} onChange={(e) => set("servicenow", { ...a.servicenow, enabled: e.target.checked })} />
                Pull incidents from ServiceNow
              </label>
              {a.servicenow.enabled && (
                <div className="sub-fields">
                  <div className="field">
                    <label htmlFor={fieldId.snQuery}>Encoded query</label>
                    <input
                      id={fieldId.snQuery}
                      value={a.servicenow.query}
                      onChange={(e) => set("servicenow", { ...a.servicenow, query: e.target.value })}
                      placeholder="assignment_group.name=Discretion Support^active=true"
                      {...errProps(errors, fieldId.snQuery)}
                    />
                    <FieldErr errors={errors} id={fieldId.snQuery} />
                    <div className="hint">In ServiceNow, filter the Incident list, right-click the filter breadcrumb and choose Copy query.</div>
                  </div>
                  <label className="check">
                    <input type="checkbox" checked={a.servicenow.postWorkNotes} onChange={(e) => set("servicenow", { ...a.servicenow, postWorkNotes: e.target.checked })} />
                    Post Wisdom AI&apos;s analysis back to the incident as a work note
                  </label>
                  <div className="hint">Start with this off; turn it on once the team trusts the analysis.</div>
                </div>
              )}
              <div style={{ height: 14 }} />
              <label className="check">
                <input type="checkbox" checked={a.splunk.enabled} onChange={(e) => set("splunk", { ...a.splunk, enabled: e.target.checked })} />
                Search Splunk for evidence when an incident arrives
              </label>
              {a.splunk.enabled && (
                <div className="sub-fields">
                  <div className="field">
                    <label htmlFor={fieldId.splunkSearch}>Base search</label>
                    <input
                      id={fieldId.splunkSearch}
                      value={a.splunk.baseSearch}
                      onChange={(e) => set("splunk", { ...a.splunk, baseSearch: e.target.value })}
                      placeholder="index=discretion sourcetype=app_logs (ERROR OR WARN)"
                      {...errProps(errors, fieldId.splunkSearch)}
                    />
                    <FieldErr errors={errors} id={fieldId.splunkSearch} />
                    <div className="hint">Where to look (index, sourcetype, app). Wisdom AI adds what to look for from each incident.</div>
                  </div>
                  <div className="grid g3">
                    <div className="field">
                      <label htmlFor="f-sp-before">Minutes before incident</label>
                      <input id="f-sp-before" type="number" value={a.splunk.windowBeforeMinutes} onChange={(e) => set("splunk", { ...a.splunk, windowBeforeMinutes: Number(e.target.value) })} />
                    </div>
                    <div className="field">
                      <label htmlFor="f-sp-after">Minutes after</label>
                      <input id="f-sp-after" type="number" value={a.splunk.windowAfterMinutes} onChange={(e) => set("splunk", { ...a.splunk, windowAfterMinutes: Number(e.target.value) })} />
                    </div>
                    <div className="field">
                      <label htmlFor="f-sp-max">Max events</label>
                      <input id="f-sp-max" type="number" value={a.splunk.maxEvents} onChange={(e) => set("splunk", { ...a.splunk, maxEvents: Number(e.target.value) })} />
                    </div>
                  </div>
                </div>
              )}
            </>
          )}

          {isReview && <Review a={a} onEdit={go} onToggleEnabled={(v) => set("enabled", v)} />}
        </section>

        <div className="wizard-footer">
          <div className="wizard-footer-inner">
            <span className="step-count">
              {idx + 1} / {STEPS.length} · {def.title}
            </span>
            <div className="row" style={{ marginLeft: "auto" }}>
              {idx > 0 && (
                <button type="button" className="btn" onClick={back}>
                  <ArrowLeft size={16} aria-hidden /> Back
                </button>
              )}
              {!isReview && (
                <button type="button" className={editing ? "btn" : "btn primary"} onClick={next}>
                  {def.optional && !stepHasData(step, a) ? "Skip" : "Next"} <ArrowRight size={16} aria-hidden />
                </button>
              )}
              {(isReview || editing) && (
                <button type="button" className="btn primary" disabled={busy} onClick={submit}>
                  {busy ? <LoaderCircle size={16} className="spin" aria-hidden /> : <Save size={16} aria-hidden />}
                  {busy ? "Saving…" : editing ? "Save changes" : "Create agent"}
                </button>
              )}
              {editing && (
                <Link className="btn ghost" href={`/agents/${agentId}`}>
                  Cancel
                </Link>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

/** Last step: everything on one screen, with an Edit link per section and non-blocking warnings. */
function Review({ a, onEdit, onToggleEnabled }: { a: AgentInput; onEdit: (s: StepId) => void; onToggleEnabled: (v: boolean) => void }) {
  const warnings = reviewWarnings(a);
  const Section = ({ id, title, children }: { id: StepId; title: string; children: React.ReactNode }) => (
    <div className="review-section">
      <div className="spread">
        <h3>{title}</h3>
        <button type="button" className="btn small ghost" onClick={() => onEdit(id)} aria-label={`Edit ${title}`}>
          <Pencil size={14} aria-hidden /> Edit
        </button>
      </div>
      <dl>{children}</dl>
    </div>
  );
  const Row = ({ k, children }: { k: string; children: React.ReactNode }) => (
    <>
      <dt>{k}</dt>
      <dd>{children}</dd>
    </>
  );
  const pipeTarget = (p: PipelineConfig) =>
    p.type === "jenkins" ? `Jenkins · ${p.jobPath}` : p.type === "teamcity" ? `TeamCity · ${p.buildTypeId}` : `GitHub Actions · ${p.repo}/${p.workflow}`;

  return (
    <>
      {warnings.length > 0 && (
        <div className="notice" role="status">
          <TriangleAlert size={16} aria-hidden />
          <div>
            <strong>Worth a look before you create it</strong> (you can still go ahead)
            <ul>
              {warnings.map((w) => (
                <li key={w.message}>
                  {w.message}{" "}
                  <button type="button" className="linklike" onClick={() => onEdit(w.step)}>
                    Fix
                  </button>
                </li>
              ))}
            </ul>
          </div>
        </div>
      )}

      <Section id="module" title="Module">
        <Row k="Agent">{a.name}</Row>
        <Row k="Module">{a.module}</Row>
        <Row k="Scope">
          <span className="pre">{a.description}</span>
          <QualityHint kind="module" text={a.description} />
        </Row>
      </Section>

      <Section id="repos" title="Repos">
        {a.repos.map((r) => (
          <Row key={r.fullName} k={r.fullName}>
            {r.branches.length ? r.branches.join(", ") : "default branch"} ·{" "}
            {r.paths.length ? `${r.paths.length} path rule${r.paths.length === 1 ? "" : "s"}` : "every file"} ·{" "}
            {r.notes.trim() ? "notes added" : "no notes"}
          </Row>
        ))}
        <Row k="Detection">
          {a.mode === "action" ? "GitHub Action" : a.mode === "poll" ? `Poller every ${a.pollingIntervalMinutes} min` : `Action + poller every ${a.pollingIntervalMinutes} min`} ·
          relevance {Math.round(a.relevanceThreshold * 100)}%
        </Row>
      </Section>

      <Section id="pipelines" title="Test pipelines">
        {a.pipelines.length ? (
          a.pipelines.map((p, i) => (
            <Row key={i} k={p.name || `Pipeline ${i + 1}`}>
              {pipeTarget(p)} · {p.runWhen === "always" ? "every relevant change" : "when Wisdom AI recommends"}
              {p.onlyRepos.length ? ` · only ${p.onlyRepos.join(", ")}` : ""}
            </Row>
          ))
        ) : (
          <Row k="Pipelines">None</Row>
        )}
      </Section>

      <Section id="notifications" title="Notifications">
        <Row k="Email">{a.emailRecipients.join(", ") || "None"}</Row>
        <Row k="Teams">{a.teamsWebhookUrl.trim() ? "Webhook set" : "None"}</Row>
        <Row k="Pipeline results">{a.notifyOnPipeline === "failure" ? "On failure only" : a.notifyOnPipeline === "always" ? "Always" : "Never"}</Row>
      </Section>

      <Section id="incidents" title="Incidents">
        <Row k="ServiceNow">
          {a.servicenow.enabled ? (
            <>
              <code>{a.servicenow.query}</code>
              {a.servicenow.postWorkNotes ? " · posts work notes" : ""}
            </>
          ) : (
            "Off"
          )}
        </Row>
        <Row k="Splunk">
          {a.splunk.enabled ? (
            <>
              <code>{a.splunk.baseSearch}</code> · {a.splunk.windowBeforeMinutes} min before, {a.splunk.windowAfterMinutes} after, max {a.splunk.maxEvents}
            </>
          ) : (
            "Off"
          )}
        </Row>
      </Section>

      <label className="check" style={{ marginTop: 8 }}>
        <input type="checkbox" checked={a.enabled} onChange={(e) => onToggleEnabled(e.target.checked)} />
        Enabled: start watching as soon as it&apos;s saved
      </label>
    </>
  );
}
