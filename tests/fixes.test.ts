import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";
import { collectIds, dropUncitedLinks } from "@/lib/assistant/citations";

const A = "a".repeat(24);
const B = "b".repeat(24);
const C = "c".repeat(24);

describe("citation check", () => {
  it("keeps links to records Claude was shown", () => {
    const known = collectIds([`{"_id":"${A}"}`]);
    const r = dropUncitedLinks(`Caused by the UTC change [change:${A}].`, known);
    expect(r.dropped).toEqual([]);
    expect(r.text).toBe(`Caused by the UTC change [change:${A}].`);
  });

  it("removes links to ids no tool returned and says so", () => {
    const known = collectIds([`run for change ${A}`]);
    const r = dropUncitedLinks(`See [change:${A}] and [incident:${B}], also [agent:${C}].`, known);
    expect(r.dropped).toEqual([`incident:${B}`, `agent:${C}`]);
    expect(r.text).toContain(`[change:${A}]`);
    expect(r.text).not.toContain(B);
    expect(r.text).not.toContain(C);
    expect(r.text).toMatch(/Removed 2 links/);
  });
});

describe("failure alert", () => {
  beforeAll(() => {
    process.env.APP_BASE_URL = "http://wisdom.local";
  });

  it("names the change author", async () => {
    const { runMessage } = await import("@/lib/notify");
    const change = {
      _id: A,
      repo: "acme/discretion-poa",
      branch: "main",
      after: "f".repeat(40),
      pusher: "pusher1",
      commits: [{ author: "Priya Shah" }, { author: "Priya Shah" }, { author: "Sam Lee" }],
      triage: { summary: "Moved isExpired to UTC" },
    };
    const run = { pipelineName: "poa-regression", status: "failure", tests: { failed: 2, total: 43 }, changeId: A };
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const m = runMessage({ module: "discretion" } as any, run as any, change as any);
    expect(m.facts).toContainEqual(["Author", "Priya Shah, Sam Lee"]);
    expect(m.severity).toBe("attention");
  });
});

describe("repo check for agent setup", () => {
  afterEach(() => vi.unstubAllGlobals());

  it("reports repos that don't exist and fails loudly when GitHub is unreachable", async () => {
    process.env.GITHUB_TOKEN = "t";
    process.env.GITHUB_API_URL = "http://gh.local";
    const { findMissingRepos } = await import("@/lib/github");
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string) =>
        url.includes("nope")
          ? new Response('{"message":"Not Found"}', { status: 404, statusText: "Not Found" })
          : new Response(JSON.stringify({ full_name: "acme/poa", default_branch: "main" }), { status: 200 }),
      ),
    );
    expect(await findMissingRepos(["acme/poa", "acme/nope"])).toEqual(["acme/nope"]);

    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response("boom", { status: 502, statusText: "Bad Gateway" })),
    );
    await expect(findMissingRepos(["acme/poa"])).rejects.toThrow(/Couldn't check acme\/poa/);
  });
});
