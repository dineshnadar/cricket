import { describe, expect, it } from "vitest";
import { ObjectId } from "mongodb";
import { jobUrl } from "@/lib/jenkins";
import { branchWatched, findRepoConfig, matchWatchedFiles } from "@/lib/paths";
import { selectPipelines, changeParams } from "@/lib/pipelines";
import { agentInputSchema, ingestSchema } from "@/lib/schemas";
import { buildSearch, parseExport } from "@/lib/splunk";
import { buildTriagePrompt } from "@/lib/triage";
import { hashToken, newToken, tokenMatches } from "@/lib/util";
import { ingestScript, workflowYaml } from "@/lib/workflow-template";
import { mdToHtml, teamsCard } from "@/lib/notify";
import type { Agent, Change, WatchedRepo } from "@/lib/types";

const repo: WatchedRepo = {
  fullName: "acme/discretion-poa",
  branches: ["main", "release/*"],
  paths: ["src/**/poa/**", "src/main/resources/application*.yml", "!**/*.md"],
  notes: "Owns POA validation",
};

const agent: Agent = {
  _id: new ObjectId(),
  name: "Discretion watcher",
  module: "Discretion",
  description: "POA, check, card and CIP verification flows.",
  repos: [repo],
  mode: "action",
  pollingIntervalMinutes: 10,
  emailRecipients: ["a@b.com"],
  teamsWebhookUrl: "",
  relevanceThreshold: 0.6,
  pipelines: [
    { name: "POA regression", type: "jenkins", jobPath: "discretion/poa", params: { SUITE: "poa" }, runWhen: "always", onlyRepos: [] },
    { name: "Card e2e", type: "jenkins", jobPath: "discretion/card", params: {}, runWhen: "recommended", onlyRepos: [] },
    { name: "Other repo only", type: "jenkins", jobPath: "x", params: {}, runWhen: "always", onlyRepos: ["acme/other"] },
  ],
  notifyOnPipeline: "failure",
  servicenow: { enabled: false, query: "", postWorkNotes: false },
  splunk: { enabled: false, baseSearch: "", windowBeforeMinutes: 60, windowAfterMinutes: 10, maxEvents: 20 },
  enabled: true,
  ingestTokenHash: "",
  ingestTokenHint: "",
  createdAt: new Date(),
  updatedAt: new Date(),
};

const change = (over: Partial<Change> = {}): Change => ({
  _id: new ObjectId(),
  agentId: agent._id,
  repo: "acme/discretion-poa",
  branch: "main",
  before: "a".repeat(40),
  after: "b".repeat(40),
  source: "github-action",
  commits: [{ sha: "b".repeat(40), message: "Tighten POA expiry check", author: "dev1" }],
  files: [{ filename: "src/x/poa/Validator.java", status: "modified", additions: 3, deletions: 1, patch: "@@\n-old\n+new" }],
  matchedFiles: ["src/x/poa/Validator.java"],
  status: "received",
  attempts: 0,
  nextAttemptAt: new Date(),
  pipelineRunIds: [],
  notifications: [],
  receivedAt: new Date(),
  updatedAt: new Date(),
  ...over,
});

describe("paths", () => {
  it("matches include globs and honours excludes", () => {
    expect(
      matchWatchedFiles(repo, [
        "src/main/java/poa/A.java",
        "src/main/resources/application-prod.yml",
        "src/main/java/card/B.java",
        "src/poa/README.md",
      ]),
    ).toEqual(["src/main/java/poa/A.java", "src/main/resources/application-prod.yml"]);
  });
  it("treats empty paths as everything", () => {
    expect(matchWatchedFiles({ ...repo, paths: [] }, ["a", "b/c"])).toEqual(["a", "b/c"]);
  });
  it("matches branch globs", () => {
    expect(branchWatched(repo, "release/2026.10")).toBe(true);
    expect(branchWatched(repo, "feature/x")).toBe(false);
  });
  it("finds repo config case-insensitively", () => {
    expect(findRepoConfig(agent, "ACME/Discretion-POA")).toBe(repo);
  });
});

describe("jenkins", () => {
  it("builds nested job URLs", () => {
    expect(jobUrl("discretion/poa-regression", "https://jenkins.local/")).toBe(
      "https://jenkins.local/job/discretion/job/poa-regression",
    );
    expect(jobUrl("job/a/job/b", "https://j")).toBe("https://j/job/a/job/b");
    expect(jobUrl("https://other/job/x/", "https://j")).toBe("https://other/job/x");
  });
});

describe("pipeline selection", () => {
  it("runs 'always' pipelines, recommended ones only when recommended, and respects onlyRepos", () => {
    const c = change({
      triage: {
        relevant: true, confidence: 0.9, summary: "", impactedAreas: [], riskLevel: "high", riskReasons: [],
        testFocus: [], recommendedPipelines: ["Card e2e"], model: "m", at: new Date(),
      },
    });
    expect(selectPipelines(agent, c).map((p) => p.name)).toEqual(["POA regression", "Card e2e"]);
    expect(selectPipelines(agent, change()).map((p) => p.name)).toEqual(["POA regression"]);
  });
  it("adds change context params", () => {
    const c = change();
    const p = changeParams(agent, c, agent.pipelines[0]);
    expect(p).toMatchObject({ SUITE: "poa", WISDOM_REPO: c.repo, WISDOM_SHA: c.after, WISDOM_CHANGE_ID: String(c._id) });
  });
});

describe("schemas", () => {
  it("validates a minimal agent and applies defaults", () => {
    const r = agentInputSchema.parse({
      name: "x",
      module: "Discretion",
      description: "POA, check, card, CIP",
      repos: [{ fullName: "acme/a" }],
    });
    expect(r.mode).toBe("action");
    expect(r.repos[0].branches).toEqual([]);
    expect(r.splunk.maxEvents).toBe(50);
  });
  it("rejects jenkins pipelines without a job path", () => {
    const r = agentInputSchema.safeParse({
      name: "x", module: "m", description: "long enough desc", repos: [{ fullName: "a/b" }],
      pipelines: [{ name: "p", type: "jenkins" }],
    });
    expect(r.success).toBe(false);
  });
  it("accepts the payload shape the workflow sends", () => {
    const r = ingestSchema.safeParse({
      agentId: String(agent._id), repo: "acme/a", branch: "main", before: "", after: "c".repeat(40),
      commits: [{ sha: "c".repeat(40), message: "m", author: "u", email: undefined }],
      files: [{ filename: "f", status: "added", additions: 1, deletions: 0 }],
    });
    expect(r.success).toBe(true);
  });
});

describe("tokens", () => {
  it("hashes and verifies", () => {
    const t = newToken();
    expect(tokenMatches(t, hashToken(t))).toBe(true);
    expect(tokenMatches(t + "x", hashToken(t))).toBe(false);
  });
});

describe("workflow template", () => {
  it("emits branches, paths, the agent id, and GitHub expressions intact", () => {
    const y = workflowYaml(agent, repo, "https://wisdom.local");
    expect(y).toContain("- 'release/*'");
    expect(y).toContain("- '!**/*.md'");
    expect(y).toContain(`WISDOM_AGENT_ID: '${agent._id}'`);
    expect(y).toContain("${{ secrets.WISDOM_TOKEN }}");
    expect(y).toContain("${{ vars.WISDOM_URL || 'https://wisdom.local' }}");
  });
  it("embeds a syntactically valid async script", () => {
    // github-script wraps the script in an async function body
    const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
    expect(() => new AsyncFunction("github", "context", "core", ingestScript)).not.toThrow();
  });
});

describe("splunk", () => {
  it("builds a search with quoted terms and a head limit", () => {
    expect(buildSearch("index=disc ERROR", ['CipVerificationException', 'say "hi"'], 25)).toBe(
      'search index=disc ERROR ("CipVerificationException" OR "say \\"hi\\"") | head 25',
    );
  });
  it("parses export NDJSON", () => {
    const ev = parseExport('{"preview":false,"result":{"_raw":"boom","_time":"t","host":"h"}}\n{"lastrow":true}\n');
    expect(ev).toEqual([{ raw: "boom", time: "t", host: "h", source: undefined }]);
  });
});

describe("triage prompt", () => {
  it("includes module scope, repo notes, pipelines and the watched diff", () => {
    const p = buildTriagePrompt(agent, change());
    expect(p).toContain("POA, check, card and CIP verification flows.");
    expect(p).toContain("Owns POA validation");
    expect(p).toContain("- Card e2e (jenkins)");
    expect(p).toContain("src/x/poa/Validator.java (modified, +3/-1) [watched]");
    expect(p).toContain("+new");
  });
});

describe("notify", () => {
  it("renders bullets and escapes html", () => {
    expect(mdToHtml("- **a** <b>\n- `c`")).toBe("<ul>\n<li><strong>a</strong> &lt;b&gt;</li>\n<li><code>c</code></li>\n</ul>");
  });
  it("builds an adaptive card", () => {
    const card = teamsCard({
      subject: "s", title: "t", facts: [["k", "v"]], body: "b", link: { label: "l", url: "u" }, severity: "attention",
    });
    expect(card.attachments[0].content.body[0]).toMatchObject({ text: "t", color: "Attention" });
  });
});

describe("teamcity", () => {
  it("sends WISDOM_* context as env params and passes prefixed params through", async () => {
    const { toProperties, testsOf } = await import("@/lib/teamcity");
    expect(toProperties({ WISDOM_SHA: "abc", SUITE: "poa" })).toEqual([
      { name: "env.WISDOM_SHA", value: "abc" },
      { name: "env.SUITE", value: "poa" },
    ]);
    expect(toProperties({ WISDOM_SHA: "abc", SUITE: "poa" }, "system.")[1]).toEqual({ name: "system.SUITE", value: "poa" });
    expect(toProperties({ SUITE: "poa" }, "")[0]).toEqual({ name: "SUITE", value: "poa" });
    expect(testsOf({ id: 1, state: "finished", testOccurrences: { count: 10, failed: 2, ignored: 1, muted: 1 } })).toEqual({ total: 10, failed: 2, skipped: 2 });
    expect(testsOf({ id: 1, state: "queued" })).toBeUndefined();
  });
  it("requires a build configuration id", () => {
    const base = { name: "x", module: "m", description: "long enough desc", repos: [{ fullName: "a/b" }] };
    expect(agentInputSchema.safeParse({ ...base, pipelines: [{ name: "p", type: "teamcity" }] }).success).toBe(false);
    expect(agentInputSchema.safeParse({ ...base, pipelines: [{ name: "p", type: "teamcity", buildTypeId: "Proj_Build" }] }).success).toBe(true);
    // dotted param names are rejected (Mongo-unsafe); the prefix is a separate setting
    expect(agentInputSchema.safeParse({ ...base, pipelines: [{ name: "p", type: "teamcity", buildTypeId: "B", params: { "env.X": "1" } }] }).success).toBe(false);
  });
});
