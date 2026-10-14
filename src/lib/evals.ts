/**
 * Evals: measure how good Wisdom AI's decisions are against labels people gave.
 *
 *  - Labels come from the "Was this right?" buttons on change and incident pages (collection eval_labels).
 *  - `npm run eval` scores either the answers stored at the time (--stored, free) or re-runs the
 *    current prompt/model on the same items (default) and compares.
 *
 * The metric functions are pure so they can be unit tested.
 */
import type { ObjectId } from "mongodb";
import { collections } from "./db";
import { computeIncidentAnalysis, INCIDENT_PROMPT_VERSION } from "./incidents";
import { triageChange, TRIAGE_PROMPT_VERSION } from "./triage";
import type { Agent, EvalLabel, RiskLevel } from "./types";
import { errMsg } from "./util";

// ---------- metrics (pure) ----------

export interface TriageRow {
  changeId: string;
  repo: string;
  sha: string;
  expectedRelevant: boolean;
  expectedRisk?: RiskLevel;
  relevant: boolean;
  confidence: number;
  riskLevel?: RiskLevel;
  threshold: number;
  error?: string;
}

/** What the system actually did with the answer: relevant AND confident enough to act. */
export const acted = (r: Pick<TriageRow, "relevant" | "confidence" | "threshold">) => r.relevant && r.confidence >= r.threshold;

const ratio = (a: number, b: number) => (b ? a / b : null);

export interface TriageMetrics {
  n: number;
  errors: number;
  tp: number;
  fp: number;
  fn: number;
  tn: number;
  accuracy: number | null;
  /** Of the changes we acted on, how many really mattered. */
  precision: number | null;
  /** Of the changes that really mattered, how many we acted on. Missed = false negatives. */
  recall: number | null;
  f1: number | null;
  riskAgreement: number | null;
  riskLabelled: number;
  avgConfidenceCorrect: number | null;
  avgConfidenceWrong: number | null;
  falseNegatives: TriageRow[];
  falsePositives: TriageRow[];
}

export function triageMetrics(rows: TriageRow[]): TriageMetrics {
  const ok = rows.filter((r) => !r.error);
  let tp = 0, fp = 0, fn = 0, tn = 0;
  const falseNegatives: TriageRow[] = [];
  const falsePositives: TriageRow[] = [];
  const confRight: number[] = [];
  const confWrong: number[] = [];
  for (const r of ok) {
    const p = acted(r);
    if (p && r.expectedRelevant) tp++;
    else if (p && !r.expectedRelevant) (fp++, falsePositives.push(r));
    else if (!p && r.expectedRelevant) (fn++, falseNegatives.push(r));
    else tn++;
    (p === r.expectedRelevant ? confRight : confWrong).push(r.confidence);
  }
  const precision = ratio(tp, tp + fp);
  const recall = ratio(tp, tp + fn);
  const f1 = precision !== null && recall !== null && precision + recall > 0 ? (2 * precision * recall) / (precision + recall) : null;
  const risky = ok.filter((r) => r.expectedRisk && r.riskLevel);
  const avg = (xs: number[]) => (xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : null);
  return {
    n: ok.length,
    errors: rows.length - ok.length,
    tp, fp, fn, tn,
    accuracy: ratio(tp + tn, ok.length),
    precision,
    recall,
    f1,
    riskAgreement: ratio(risky.filter((r) => r.expectedRisk === r.riskLevel).length, risky.length),
    riskLabelled: risky.length,
    avgConfidenceCorrect: avg(confRight),
    avgConfidenceWrong: avg(confWrong),
    falseNegatives,
    falsePositives,
  };
}

export interface IncidentRow {
  incidentId: string;
  number: string;
  /** null = not caused by a recorded code change */
  expectedCause: string | null;
  /** change ids of likely causes, most likely first, with confidences */
  causes: { changeId?: string; confidence: number }[];
  error?: string;
}

/** Below this, a top cause counts as "no clear cause". */
export const NO_CAUSE_CONFIDENCE = 0.4;

export interface IncidentMetrics {
  n: number;
  errors: number;
  /** Incidents with a known causing change */
  withCause: number;
  hitAt1: number | null;
  hitAt3: number | null;
  /** Incidents labelled "not a code change": did the AI avoid confidently blaming a change? */
  noCause: number;
  noCauseCorrect: number | null;
  misses: IncidentRow[];
}

export function incidentMetrics(rows: IncidentRow[]): IncidentMetrics {
  const ok = rows.filter((r) => !r.error);
  const withCause = ok.filter((r) => r.expectedCause);
  const noCause = ok.filter((r) => !r.expectedCause);
  const ids = (r: IncidentRow) => r.causes.filter((c) => c.changeId).map((c) => c.changeId!);
  const at1 = withCause.filter((r) => ids(r)[0] === r.expectedCause);
  const at3 = withCause.filter((r) => ids(r).slice(0, 3).includes(r.expectedCause!));
  const noCauseOk = noCause.filter((r) => {
    const top = r.causes.find((c) => c.changeId);
    return !top || top.confidence < NO_CAUSE_CONFIDENCE;
  });
  const misses = [
    ...withCause.filter((r) => !ids(r).slice(0, 3).includes(r.expectedCause!)),
    ...noCause.filter((r) => !noCauseOk.includes(r)),
  ];
  return {
    n: ok.length,
    errors: rows.length - ok.length,
    withCause: withCause.length,
    hitAt1: ratio(at1.length, withCause.length),
    hitAt3: ratio(at3.length, withCause.length),
    noCause: noCause.length,
    noCauseCorrect: ratio(noCauseOk.length, noCause.length),
    misses,
  };
}

// ---------- labels ----------

export async function saveLabel(input: {
  kind: EvalLabel["kind"];
  targetId: ObjectId;
  expected: EvalLabel["expected"];
  note?: string;
  user: string;
}): Promise<EvalLabel> {
  const { changes, incidents, evalLabels } = await collections();
  let agentId: ObjectId;
  let actual: EvalLabel["actual"];
  if (input.kind === "triage") {
    const c = await changes.findOne({ _id: input.targetId });
    if (!c?.triage) throw new Error("Change not found or not triaged yet");
    agentId = c.agentId;
    actual = { relevant: c.triage.relevant, confidence: c.triage.confidence, riskLevel: c.triage.riskLevel, model: c.triage.model, promptVersion: c.triage.promptVersion };
  } else {
    const i = await incidents.findOne({ _id: input.targetId });
    if (!i?.analysis) throw new Error("Incident not found or not analysed yet");
    agentId = i.agentId;
    const top = i.analysis.likelyCauses.find((c) => c.changeId);
    actual = { topCauseChangeId: top?.changeId ?? null, model: i.analysis.model, promptVersion: i.analysis.promptVersion };
  }
  const doc = { kind: input.kind, targetId: input.targetId, agentId, expected: input.expected, actual, note: input.note, user: input.user, at: new Date() };
  const r = await evalLabels.findOneAndUpdate(
    { kind: input.kind, targetId: input.targetId },
    { $set: doc },
    { upsert: true, returnDocument: "after" },
  );
  return r as EvalLabel;
}

export async function labelCounts(agentId: ObjectId) {
  const { evalLabels } = await collections();
  const [triage, incident] = await Promise.all([
    evalLabels.countDocuments({ agentId, kind: "triage" }),
    evalLabels.countDocuments({ agentId, kind: "incident" }),
  ]);
  return { triage, incident };
}

// ---------- runners ----------

export interface RunOptions {
  agentId?: ObjectId;
  limit?: number;
  /** true = score the answers stored at the time (no LLM calls). false = re-run the current prompt. */
  stored?: boolean;
  onProgress?: (done: number, total: number) => void;
}

async function loadAgents(): Promise<Map<string, Agent>> {
  const { agents } = await collections();
  return new Map((await agents.find({}).toArray()).map((a) => [String(a._id), a]));
}

export async function runTriageEval(o: RunOptions = {}): Promise<{ rows: TriageRow[]; metrics: TriageMetrics; promptVersion: string }> {
  const { evalLabels, changes } = await collections();
  const labels = await evalLabels
    .find({ kind: "triage", ...(o.agentId ? { agentId: o.agentId } : {}) })
    .sort({ at: -1 })
    .limit(o.limit ?? 1000)
    .toArray();
  const agents = await loadAgents();
  const rows: TriageRow[] = [];
  let done = 0;
  for (const l of labels) {
    const c = await changes.findOne({ _id: l.targetId });
    const agent = agents.get(String(l.agentId));
    const base = {
      changeId: String(l.targetId),
      repo: c?.repo ?? "?",
      sha: c?.after?.slice(0, 7) ?? "?",
      expectedRelevant: !!l.expected.relevant,
      expectedRisk: l.expected.riskLevel,
      threshold: agent?.relevanceThreshold ?? 0.6,
    };
    try {
      if (!c || !agent) throw new Error("change or agent no longer exists");
      const t = o.stored ? c.triage : await triageChange(agent, c);
      if (!t) throw new Error("no stored triage");
      rows.push({ ...base, relevant: t.relevant, confidence: t.confidence, riskLevel: t.riskLevel });
    } catch (e) {
      rows.push({ ...base, relevant: false, confidence: 0, error: errMsg(e) });
    }
    o.onProgress?.(++done, labels.length);
  }
  return { rows, metrics: triageMetrics(rows), promptVersion: o.stored ? "stored" : TRIAGE_PROMPT_VERSION };
}

export async function runIncidentEval(o: RunOptions = {}): Promise<{ rows: IncidentRow[]; metrics: IncidentMetrics; promptVersion: string }> {
  const { evalLabels, incidents } = await collections();
  const labels = await evalLabels
    .find({ kind: "incident", ...(o.agentId ? { agentId: o.agentId } : {}) })
    .sort({ at: -1 })
    .limit(o.limit ?? 1000)
    .toArray();
  const agents = await loadAgents();
  const rows: IncidentRow[] = [];
  let done = 0;
  for (const l of labels) {
    const inc = await incidents.findOne({ _id: l.targetId });
    const agent = agents.get(String(l.agentId));
    const base = { incidentId: String(l.targetId), number: inc?.number ?? "?", expectedCause: l.expected.causeChangeId ?? null };
    try {
      if (!inc || !agent) throw new Error("incident or agent no longer exists");
      // Re-runs reuse the stored Splunk evidence so results are repeatable.
      const a = o.stored ? inc.analysis : (await computeIncidentAnalysis(inc, agent, { reuseEvidence: true })).analysis;
      if (!a) throw new Error("no stored analysis");
      rows.push({ ...base, causes: a.likelyCauses.map((c) => ({ changeId: c.changeId, confidence: c.confidence })) });
    } catch (e) {
      rows.push({ ...base, causes: [], error: errMsg(e) });
    }
    o.onProgress?.(++done, labels.length);
  }
  return { rows, metrics: incidentMetrics(rows), promptVersion: o.stored ? "stored" : INCIDENT_PROMPT_VERSION };
}

// ---------- report ----------

const pct = (x: number | null) => (x === null ? "n/a" : `${Math.round(x * 100)}%`);

export function triageReport(m: TriageMetrics): string {
  const lines = [
    `| Metric | Value | Meaning |`,
    `|---|---|---|`,
    `| Labelled changes | ${m.n}${m.errors ? ` (+${m.errors} errors)` : ""} | |`,
    `| **Recall** | **${pct(m.recall)}** | Of the changes that really mattered, how many Wisdom AI acted on. Misses = risky changes nobody was told about |`,
    `| Precision | ${pct(m.precision)} | Of the changes it acted on, how many really mattered. Low = noisy alerts |`,
    `| F1 | ${pct(m.f1)} | Balance of the two |`,
    `| Accuracy | ${pct(m.accuracy)} | |`,
    `| Confusion | TP ${m.tp} · FP ${m.fp} · FN ${m.fn} · TN ${m.tn} | |`,
    `| Risk level agreement | ${pct(m.riskAgreement)} (${m.riskLabelled} labelled) | |`,
    `| Avg confidence when right / wrong | ${pct(m.avgConfidenceCorrect)} / ${pct(m.avgConfidenceWrong)} | Wrong answers should have lower confidence |`,
  ];
  if (m.falseNegatives.length) {
    lines.push("", "**Missed (false negatives):**");
    for (const r of m.falseNegatives) lines.push(`- ${r.repo}@${r.sha}: relevant=${r.relevant}, confidence ${pct(r.confidence)} (threshold ${pct(r.threshold)}) · /changes/${r.changeId}`);
  }
  if (m.falsePositives.length) {
    lines.push("", "**Acted on but not relevant (false positives):**");
    for (const r of m.falsePositives) lines.push(`- ${r.repo}@${r.sha}: confidence ${pct(r.confidence)} · /changes/${r.changeId}`);
  }
  return lines.join("\n");
}

export function incidentReport(m: IncidentMetrics): string {
  const lines = [
    `| Metric | Value | Meaning |`,
    `|---|---|---|`,
    `| Labelled incidents | ${m.n}${m.errors ? ` (+${m.errors} errors)` : ""} | |`,
    `| **Right cause ranked #1** | **${pct(m.hitAt1)}** (${m.withCause} with a known cause) | |`,
    `| Right cause in top 3 | ${pct(m.hitAt3)} | |`,
    `| "Not a code change" handled correctly | ${pct(m.noCauseCorrect)} (${m.noCause} labelled) | Didn't confidently blame a change (top confidence < ${pct(NO_CAUSE_CONFIDENCE)}) |`,
  ];
  if (m.misses.length) {
    lines.push("", "**Misses:**");
    for (const r of m.misses) {
      const got = r.causes.find((c) => c.changeId);
      lines.push(`- ${r.number}: expected ${r.expectedCause ?? "no code change"}, got ${got ? `${got.changeId} (${pct(got.confidence)})` : "no cause"} · /incidents/${r.incidentId}`);
    }
  }
  return lines.join("\n");
}
