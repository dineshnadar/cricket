import { describe, expect, it } from "vitest";
import { defaultAgent } from "@/components/AgentForm";
import { firstInvalidStep, reviewWarnings, stepForServerError, stepSummary, validateStep } from "@/lib/agent-steps";
import type { AgentInput } from "@/lib/schemas";

const good: AgentInput = {
  ...defaultAgent,
  name: "Discretion watcher",
  module: "Discretion",
  description: "In scope: POA setup and validation; check deposit; card limits; CIP checks. Out of scope: statements, marketing pages. Critical flows: POA expiry.",
  repos: [{ fullName: "acme/discretion-poa", branches: ["main"], paths: ["src/**/poa/**"], notes: "What it does: POA. Critical code: PoaValidator. Owner: POA team" }],
  emailRecipients: ["team@acme.com"],
};

describe("wizard step rules", () => {
  it("requires name, module and a scope on step 1", () => {
    const e = validateStep("module", defaultAgent).map((x) => x.field);
    expect(e).toEqual(["f-name", "f-module", "f-description"]);
    expect(validateStep("module", good)).toEqual([]);
  });
  it("requires at least one repo", () => {
    expect(validateStep("repos", { ...good, repos: [] })[0].field).toBe("f-repo-search");
  });
  it("checks each pipeline by type", () => {
    const a = { ...good, pipelines: [{ name: "", type: "teamcity" as const, params: {}, runWhen: "always" as const, onlyRepos: [] }] };
    expect(validateStep("pipelines", a).map((x) => x.field)).toEqual(["f-pipe-0-name", "f-pipe-0-build"]);
  });
  it("checks emails, Teams URL and enabled integrations", () => {
    expect(validateStep("notifications", { ...good, emailRecipients: ["nope"], teamsWebhookUrl: "http://x" })).toHaveLength(2);
    expect(validateStep("incidents", { ...good, servicenow: { ...good.servicenow, enabled: true } })[0].field).toBe("f-sn-query");
  });
  it("finds the first step that needs fixing", () => {
    expect(firstInvalidStep(good)).toBeNull();
    expect(firstInvalidStep({ ...good, repos: [] })).toBe("repos");
  });
  it("summarises steps for the step list", () => {
    expect(stepSummary("repos", good)).toBe("1 repo");
    expect(stepSummary("pipelines", good)).toBe("None (optional)");
    expect(stepSummary("notifications", { ...good, teamsWebhookUrl: "https://t" })).toBe("1 email + Teams");
  });
  it("warns, without blocking, about things people usually forget", () => {
    const w = reviewWarnings({ ...good, emailRecipients: [], repos: [{ ...good.repos[0], notes: "", paths: [] }] }).map((x) => x.step);
    expect(w).toEqual(["repos", "repos", "pipelines", "notifications"]);
  });
  it("sends server errors to the step that owns the field", () => {
    expect(stepForServerError("repos.0.fullName: Invalid")).toBe("repos");
    expect(stepForServerError("pipelines.1.jobPath: Required")).toBe("pipelines");
    expect(stepForServerError("description: Describe what belongs to the module")).toBe("module");
  });
});
