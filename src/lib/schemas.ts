import { z } from "zod";

const repoName = z
  .string()
  .trim()
  .regex(/^[\w.-]+\/[\w.-]+$/, "Use owner/name");

export const watchedRepoSchema = z.object({
  fullName: repoName,
  branches: z.array(z.string().trim().min(1)).default([]),
  paths: z.array(z.string().trim().min(1)).default([]),
  notes: z.string().default(""),
});

export const pipelineSchema = z
  .object({
    name: z.string().trim().min(1),
    type: z.enum(["jenkins", "github-actions", "teamcity"]),
    jobPath: z.string().trim().optional(),
    buildTypeId: z.string().trim().optional(),
    paramPrefix: z.enum(["env.", "system.", ""]).optional(),
    repo: z.string().trim().optional(),
    workflow: z.string().trim().optional(),
    ref: z.string().trim().optional(),
    params: z
      .record(
        z.string().regex(/^[A-Za-z_][A-Za-z0-9_-]*$/, "Parameter names use letters, digits, _ or - (no dots; set the TeamCity prefix separately)"),
        z.string(),
      )
      .default({}),
    runWhen: z.enum(["always", "recommended"]).default("always"),
    onlyRepos: z.array(repoName).default([]),
  })
  .superRefine((p, ctx) => {
    if (p.type === "jenkins" && !p.jobPath) {
      ctx.addIssue({ code: "custom", path: ["jobPath"], message: "Jenkins job path is required" });
    }
    if (p.type === "teamcity" && !p.buildTypeId) {
      ctx.addIssue({ code: "custom", path: ["buildTypeId"], message: "TeamCity build configuration id is required" });
    }
    if (p.type === "github-actions") {
      if (!p.repo || !/^[\w.-]+\/[\w.-]+$/.test(p.repo))
        ctx.addIssue({ code: "custom", path: ["repo"], message: "Workflow repo (owner/name) is required" });
      if (!p.workflow) ctx.addIssue({ code: "custom", path: ["workflow"], message: "Workflow file is required" });
    }
  });

export const agentInputSchema = z.object({
  name: z.string().trim().min(1, "Name is required"),
  module: z.string().trim().min(1, "Module is required"),
  description: z.string().trim().min(10, "Describe what belongs to the module (Wisdom AI uses this)"),
  repos: z.array(watchedRepoSchema).min(1, "Add at least one repo"),
  mode: z.enum(["action", "poll", "both"]).default("action"),
  pollingIntervalMinutes: z.coerce.number().int().min(1).max(1440).default(10),
  emailRecipients: z.array(z.string().trim().email()).default([]),
  teamsWebhookUrl: z.string().trim().url().or(z.literal("")).default(""),
  relevanceThreshold: z.coerce.number().min(0).max(1).default(0.6),
  pipelines: z.array(pipelineSchema).default([]),
  notifyOnPipeline: z.enum(["failure", "always", "never"]).default("failure"),
  servicenow: z
    .object({
      enabled: z.boolean().default(false),
      query: z.string().default(""),
      postWorkNotes: z.boolean().default(false),
    })
    .default({ enabled: false, query: "", postWorkNotes: false }),
  splunk: z
    .object({
      enabled: z.boolean().default(false),
      baseSearch: z.string().default(""),
      windowBeforeMinutes: z.coerce.number().int().min(1).max(1440).default(120),
      windowAfterMinutes: z.coerce.number().int().min(0).max(1440).default(30),
      maxEvents: z.coerce.number().int().min(1).max(500).default(50),
    })
    .default({ enabled: false, baseSearch: "", windowBeforeMinutes: 120, windowAfterMinutes: 30, maxEvents: 50 }),
  enabled: z.boolean().default(true),
});

export type AgentInput = z.infer<typeof agentInputSchema>;

export const ingestSchema = z.object({
  agentId: z.string().min(1),
  repo: repoName,
  branch: z.string().min(1),
  before: z.string().default(""),
  after: z.string().min(7),
  pusher: z.string().optional(),
  compareUrl: z.string().optional(),
  commits: z
    .array(
      z.object({
        sha: z.string(),
        message: z.string().default(""),
        author: z.string().default(""),
        email: z.string().optional(),
        timestamp: z.string().optional(),
        url: z.string().optional(),
      }),
    )
    .default([]),
  files: z
    .array(
      z.object({
        filename: z.string(),
        status: z.string().default("modified"),
        additions: z.number().default(0),
        deletions: z.number().default(0),
        patch: z.string().optional(),
      }),
    )
    .max(3000)
    .default([]),
});

export type IngestPayload = z.infer<typeof ingestSchema>;

export function formatZodError(e: z.ZodError): string {
  return e.issues.map((i) => `${i.path.join(".") || "body"}: ${i.message}`).join("; ");
}
