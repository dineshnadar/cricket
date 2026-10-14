import type { ObjectId } from "mongodb";

export type WatchMode = "action" | "poll" | "both";
export type RiskLevel = "low" | "medium" | "high";

export interface WatchedRepo {
  /** "owner/name" */
  fullName: string;
  /** Branches to watch. Empty = default branch only. */
  branches: string[];
  /** Glob patterns (picomatch). Empty = every file counts. */
  paths: string[];
  /** Free text: what matters in this repo for the module, owners, conventions. */
  notes: string;
}

export type PipelineType = "jenkins" | "github-actions" | "teamcity";
export type PipelineRunWhen = "always" | "recommended";

export interface PipelineConfig {
  name: string;
  type: PipelineType;
  /** Jenkins: "folder/job" or a full job URL. */
  jobPath?: string;
  /** TeamCity: build configuration id, e.g. "Discretion_PoaRegression". */
  buildTypeId?: string;
  /** TeamCity: prefix added to every param name ("env." = environment variable, "system." = system property, "" = configuration parameter). */
  paramPrefix?: "env." | "system." | "";
  /** GitHub Actions: repo that owns the workflow ("owner/name"), workflow file, and ref. */
  repo?: string;
  workflow?: string;
  ref?: string;
  /** Static parameters; change context is added automatically. */
  params: Record<string, string>;
  /**
   * always      = trigger on every relevant change
   * recommended = trigger only when Claude recommends it for the change
   */
  runWhen: PipelineRunWhen;
  /** Restrict to changes from these repos ("owner/name"). Empty = any repo. */
  onlyRepos: string[];
}

export interface ServiceNowConfig {
  enabled: boolean;
  /** Encoded query, e.g. "assignment_group.name=Discretion Support^active=true" */
  query: string;
  /** Write Claude's analysis back to the incident as a work note. */
  postWorkNotes: boolean;
}

export interface SplunkConfig {
  enabled: boolean;
  /** Base search, e.g. "index=discretion sourcetype=app_logs (ERROR OR Exception)". Keywords are ANDed on. */
  baseSearch: string;
  windowBeforeMinutes: number;
  windowAfterMinutes: number;
  maxEvents: number;
}

export interface Agent {
  _id: ObjectId;
  name: string;
  module: string;
  /** What belongs to the module: Claude uses this to decide relevance. */
  description: string;
  repos: WatchedRepo[];
  mode: WatchMode;
  pollingIntervalMinutes: number;
  emailRecipients: string[];
  teamsWebhookUrl: string;
  /** 0..1 — minimum Claude confidence to treat a change as relevant. */
  relevanceThreshold: number;
  pipelines: PipelineConfig[];
  notifyOnPipeline: "failure" | "always" | "never";
  servicenow: ServiceNowConfig;
  splunk: SplunkConfig;
  enabled: boolean;
  ingestTokenHash: string;
  ingestTokenHint: string;
  createdAt: Date;
  updatedAt: Date;
}

export interface ChangeCommit {
  sha: string;
  message: string;
  author: string;
  email?: string;
  timestamp?: string;
  url?: string;
}

export interface ChangeFile {
  filename: string;
  status: string;
  additions: number;
  deletions: number;
  patch?: string;
}

export interface Triage {
  relevant: boolean;
  confidence: number;
  summary: string;
  impactedAreas: string[];
  riskLevel: RiskLevel;
  riskReasons: string[];
  testFocus: string[];
  recommendedPipelines: string[];
  model: string;
  /** Which version of the triage prompt produced this (see TRIAGE_PROMPT_VERSION). */
  promptVersion?: string;
  at: Date;
}

export type ChangeStatus =
  | "received"
  | "processing"
  | "filtered" // no watched path touched
  | "not-relevant" // Claude judged outside the module
  | "actioned" // relevant: pipelines triggered / notifications sent
  | "error";

export interface Change {
  _id: ObjectId;
  agentId: ObjectId;
  repo: string;
  branch: string;
  before: string;
  after: string;
  pusher?: string;
  compareUrl?: string;
  source: "github-action" | "poll" | "manual";
  commits: ChangeCommit[];
  files: ChangeFile[];
  matchedFiles: string[];
  status: ChangeStatus;
  attempts: number;
  nextAttemptAt: Date;
  error?: string;
  triage?: Triage;
  pipelineRunIds: ObjectId[];
  notifications: NotificationRecord[];
  receivedAt: Date;
  updatedAt: Date;
}

export interface NotificationRecord {
  channel: "email" | "teams";
  ok: boolean;
  detail?: string;
  at: Date;
}

export type RunStatus = "queued" | "running" | "success" | "failure" | "unstable" | "aborted" | "error";

export interface PipelineRun {
  _id: ObjectId;
  agentId: ObjectId;
  changeId: ObjectId;
  pipelineName: string;
  type: PipelineType;
  status: RunStatus;
  /** Jenkins queue item URL */
  queueUrl?: string;
  buildNumber?: number;
  buildUrl?: string;
  /** GitHub Actions */
  ghRepo?: string;
  ghWorkflow?: string;
  ghRef?: string;
  ghRunId?: number;
  /** TeamCity */
  tcBuildId?: number;
  params: Record<string, string>;
  tests?: { total: number; failed: number; skipped: number };
  error?: string;
  triggeredAt: Date;
  finishedAt?: Date;
  lastCheckedAt?: Date;
  notified?: boolean;
}

export interface PollState {
  _id: ObjectId;
  agentId: ObjectId;
  repo: string;
  branch: string;
  lastSha: string;
  lastPolledAt: Date;
}

export interface RepoWisdom {
  repo: string;
  purpose: string;
  keyComponents: string[];
  recentActivity: string;
  hotspots: string[];
  risks: string[];
  testingNotes: string;
  headSha?: string;
}

export interface Wisdom {
  _id: ObjectId;
  agentId: ObjectId;
  moduleSummary: string;
  repos: RepoWisdom[];
  generatedAt: Date;
  model: string;
}

export interface LikelyCause {
  changeId?: string;
  repo?: string;
  sha?: string;
  reason: string;
  confidence: number;
}

export interface IncidentAnalysis {
  summary: string;
  likelyCauses: LikelyCause[];
  evidence: string[];
  suggestedActions: string[];
  searchTerms: string[];
  model: string;
  promptVersion?: string;
  at: Date;
}

export interface SplunkEvent {
  time?: string;
  host?: string;
  source?: string;
  raw: string;
}

export interface Incident {
  _id: ObjectId;
  agentId: ObjectId;
  sysId: string;
  number: string;
  shortDescription: string;
  description: string;
  priority?: string;
  state?: string;
  openedAt: Date;
  url?: string;
  status: "new" | "analyzing" | "analyzed" | "error";
  error?: string;
  splunkQuery?: string;
  splunkEvents: SplunkEvent[];
  analysis?: IncidentAnalysis;
  notified?: boolean;
  workNotePosted?: boolean;
  createdAt: Date;
  updatedAt: Date;
}

export interface Meta {
  _id: string;
  value: unknown;
  updatedAt: Date;
}

// ---------- Wisdom Assistant ----------

export interface ToolActivity {
  id: string;
  name: string;
  label: string;
  summary?: string;
  ok?: boolean;
}

export interface PendingActionView {
  id: string;
  tool: string;
  title: string;
  details: string[];
  status: "pending" | "confirmed" | "cancelled" | "failed";
  resultText?: string;
  /** Shown once (e.g. a new ingest token); never persisted after first display. */
  secret?: string;
}

export interface DisplayMessage {
  role: "user" | "assistant";
  text: string;
  tools?: ToolActivity[];
  actions?: PendingActionView[];
  at: Date;
}

export interface Conversation {
  _id: ObjectId;
  title: string;
  user: string;
  agentId?: ObjectId;
  /** The transcript. Each new turn is sent to Claude as plain text turns (tool traffic is not replayed). */
  display: DisplayMessage[];
  createdAt: Date;
  updatedAt: Date;
}

export interface PendingAction {
  _id: ObjectId;
  conversationId: ObjectId;
  tool: string;
  input: Record<string, unknown>;
  title: string;
  details: string[];
  status: "pending" | "confirmed" | "cancelled" | "failed";
  requestedBy: string;
  decidedBy?: string;
  result?: string;
  createdAt: Date;
  decidedAt?: Date;
}

export interface AuditEntry {
  _id: ObjectId;
  at: Date;
  user: string;
  conversationId?: ObjectId;
  kind: "question" | "tool" | "action-requested" | "action-confirmed" | "action-cancelled" | "assist";
  name: string;
  input?: unknown;
  ok: boolean;
  resultChars?: number;
  error?: string;
}

// ---------- evals ----------

/**
 * A person's judgement of one AI decision. One label per (kind, target):
 * the latest label wins. Used by `npm run eval` as ground truth.
 */
export interface EvalLabel {
  _id: ObjectId;
  kind: "triage" | "incident";
  agentId: ObjectId;
  /** The change id (triage) or incident id (incident). */
  targetId: ObjectId;
  expected: {
    /** triage: should this change count as relevant to the module? */
    relevant?: boolean;
    /** triage (optional): the risk level the reviewer would give */
    riskLevel?: RiskLevel;
    /** incident: the change that actually caused it; null = not caused by a code change we recorded */
    causeChangeId?: string | null;
  };
  /** What the AI said at the time, for the record. */
  actual: {
    relevant?: boolean;
    confidence?: number;
    riskLevel?: RiskLevel;
    topCauseChangeId?: string | null;
    model?: string;
    promptVersion?: string;
  };
  note?: string;
  user: string;
  at: Date;
}
