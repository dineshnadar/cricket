/**
 * Steps of the Create / Edit agent wizard: what each step requires, a one-line
 * summary for the step list, and non-blocking warnings for the review step.
 * Pure functions, no React, so they can be unit tested and used on any page.
 */
import type { AgentInput } from "./schemas";
import { fieldQuality } from "./field-quality";

export type StepId = "module" | "repos" | "pipelines" | "notifications" | "incidents" | "review";

export interface StepDef {
  id: StepId;
  title: string;
  hint: string;
  optional: boolean;
}

export const STEPS: StepDef[] = [
  { id: "module", title: "Module", hint: "Name it and say what belongs to it", optional: false },
  { id: "repos", title: "Repos", hint: "Which repos, branches and files to watch", optional: false },
  { id: "pipelines", title: "Test pipelines", hint: "Builds to run on relevant changes", optional: true },
  { id: "notifications", title: "Notifications", hint: "Who hears about changes and failures", optional: true },
  { id: "incidents", title: "Incidents", hint: "ServiceNow and Splunk", optional: true },
  { id: "review", title: "Review", hint: "Check everything, then create", optional: false },
];

export interface FieldError {
  /** id of the input to focus/link to */
  field: string;
  message: string;
}

const EMAIL = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

/** Field ids shared by the form and the error summary. */
export const fieldId = {
  name: "f-name",
  module: "f-module",
  description: "f-description",
  repos: "f-repo-search",
  pipe: (i: number, part: string) => `f-pipe-${i}-${part}`,
  emails: "f-emails",
  teams: "f-teams",
  snQuery: "f-sn-query",
  splunkSearch: "f-splunk-search",
};

export function validateStep(step: StepId, a: AgentInput): FieldError[] {
  const e: FieldError[] = [];
  switch (step) {
    case "module":
      if (!a.name.trim()) e.push({ field: fieldId.name, message: "Give the agent a name." });
      if (!a.module.trim()) e.push({ field: fieldId.module, message: "Enter the module name." });
      if (a.description.trim().length < 10)
        e.push({ field: fieldId.description, message: "Say what belongs to this module (at least a rough line). Improve with AI can tidy it up." });
      break;
    case "repos":
      if (!a.repos.length) e.push({ field: fieldId.repos, message: "Add at least one repo." });
      break;
    case "pipelines":
      a.pipelines.forEach((p, i) => {
        const n = `Pipeline ${i + 1}`;
        if (!p.name.trim()) e.push({ field: fieldId.pipe(i, "name"), message: `${n}: give it a name.` });
        if (p.type === "jenkins" && !p.jobPath?.trim()) e.push({ field: fieldId.pipe(i, "job"), message: `${n}: enter the Jenkins job path.` });
        if (p.type === "teamcity" && !p.buildTypeId?.trim())
          e.push({ field: fieldId.pipe(i, "build"), message: `${n}: enter the TeamCity build configuration id.` });
        if (p.type === "github-actions") {
          if (!p.repo?.trim()) e.push({ field: fieldId.pipe(i, "repo"), message: `${n}: enter the workflow repo.` });
          if (!p.workflow?.trim()) e.push({ field: fieldId.pipe(i, "workflow"), message: `${n}: enter the workflow file.` });
        }
      });
      break;
    case "notifications": {
      const bad = a.emailRecipients.filter((x) => !EMAIL.test(x));
      if (bad.length) e.push({ field: fieldId.emails, message: `Not a valid email: ${bad.join(", ")}` });
      if (a.teamsWebhookUrl.trim() && !/^https:\/\//i.test(a.teamsWebhookUrl.trim()))
        e.push({ field: fieldId.teams, message: "The Teams webhook URL must start with https://" });
      break;
    }
    case "incidents":
      if (a.servicenow.enabled && !a.servicenow.query.trim())
        e.push({ field: fieldId.snQuery, message: "Enter the ServiceNow encoded query, or turn ServiceNow off." });
      if (a.splunk.enabled && !a.splunk.baseSearch.trim())
        e.push({ field: fieldId.splunkSearch, message: "Enter the Splunk base search, or turn Splunk off." });
      break;
  }
  return e;
}

/** Every step before "review", in order: the first one with errors, if any. */
export function firstInvalidStep(a: AgentInput): StepId | null {
  for (const s of STEPS) if (s.id !== "review" && validateStep(s.id, a).length) return s.id;
  return null;
}

const plural = (n: number, one: string, many = one + "s") => `${n} ${n === 1 ? one : many}`;

/** Short status line shown under each step in the step list. */
export function stepSummary(step: StepId, a: AgentInput): string {
  switch (step) {
    case "module":
      return a.module.trim() || "Not started";
    case "repos":
      return a.repos.length ? plural(a.repos.length, "repo") : "None yet";
    case "pipelines":
      return a.pipelines.length ? plural(a.pipelines.length, "pipeline") : "None (optional)";
    case "notifications": {
      const parts = [a.emailRecipients.length ? plural(a.emailRecipients.length, "email") : "", a.teamsWebhookUrl.trim() ? "Teams" : ""].filter(Boolean);
      return parts.length ? parts.join(" + ") : "None (optional)";
    }
    case "incidents": {
      const parts = [a.servicenow.enabled ? "ServiceNow" : "", a.splunk.enabled ? "Splunk" : ""].filter(Boolean);
      return parts.length ? parts.join(" + ") : "Off (optional)";
    }
    case "review":
      return "";
  }
}

export interface Warning {
  step: StepId;
  message: string;
}

/** Things that are allowed but probably not what the user wants. Shown on the review step. */
export function reviewWarnings(a: AgentInput): Warning[] {
  const w: Warning[] = [];
  const q = fieldQuality("module", a.description);
  if (q.level === "vague" || q.level === "empty") w.push({ step: "module", message: `Module scope: ${q.tip}` });
  const thin = a.repos.filter((r) => fieldQuality("repoNotes", r.notes).level === "empty");
  if (thin.length) w.push({ step: "repos", message: `No notes for ${thin.map((r) => r.fullName).join(", ")}. Draft from repo takes a few seconds.` });
  const allFiles = a.repos.filter((r) => !r.paths.length);
  if (allFiles.length) w.push({ step: "repos", message: `${allFiles.map((r) => r.fullName).join(", ")} ${allFiles.length === 1 ? "watches" : "watch"} every file. Suggest paths can narrow it.` });
  if (!a.pipelines.length) w.push({ step: "pipelines", message: "No test pipelines: relevant changes are summarised and announced, but no tests run." });
  if (!a.emailRecipients.length && !a.teamsWebhookUrl.trim()) w.push({ step: "notifications", message: "Nobody is notified. Add an email address or a Teams webhook." });
  return w;
}

/** Map a server validation message ("repos.0.fullName: …") to the step that owns the field. */
export function stepForServerError(message: string): StepId {
  const head = message.trim().split(/[.:\s]/)[0].toLowerCase();
  const byField: Record<string, StepId> = {
    name: "module", module: "module", description: "module",
    repos: "repos", mode: "repos", pollingintervalminutes: "repos", relevancethreshold: "repos",
    pipelines: "pipelines",
    emailrecipients: "notifications", teamswebhookurl: "notifications", notifyonpipeline: "notifications",
    servicenow: "incidents", splunk: "incidents",
  };
  if (byField[head]) return byField[head];
  // Messages without a field path, e.g. "These repos don't exist…"
  const m = message.toLowerCase();
  if (/pipeline|jenkins|teamcity|workflow/.test(m)) return "pipelines";
  if (/\brepos?\b|branch/.test(m)) return "repos";
  if (/email|teams/.test(m)) return "notifications";
  if (/servicenow|splunk/.test(m)) return "incidents";
  return "review";
}
