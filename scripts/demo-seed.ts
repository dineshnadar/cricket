/**
 * Seeds a demo agent and two pushes through the real API, so you can watch the
 * whole flow against the mock services.  Needs `npm run mock` and `npm run dev` running.
 *   npm run demo:seed
 */
import { loadEnvFiles } from "../src/lib/load-env";
loadEnvFiles();

const APP = (process.env.APP_BASE_URL || "http://localhost:3000").replace(/\/$/, "");
const MOCK = process.env.MOCK_URL || "http://127.0.0.1:4000";
const NAME = "Discretion watcher (demo)";

async function api(path: string, init: RequestInit = {}) {
  const res = await fetch(APP + path, { ...init, headers: { "Content-Type": "application/json", ...(init.headers ?? {}) } });
  const text = await res.text();
  if (!res.ok) throw new Error(`${init.method ?? "GET"} ${path} -> ${res.status} ${text}`);
  return text ? JSON.parse(text) : null;
}

async function main() {
  try {
    await fetch(APP + "/api/health");
  } catch {
    throw new Error(`Can't reach the app at ${APP}. Start it with: npm run dev`);
  }
  try {
    await fetch(MOCK + "/teams", { method: "POST", body: JSON.stringify({ attachments: [{ content: { body: [{ text: "seed check" }] } }] }) });
  } catch {
    throw new Error(`Can't reach the mock services at ${MOCK}. Start them with: npm run mock`);
  }

  const existing = ((await api("/api/agents")) as { _id: string; name: string }[]).find((a) => a.name === NAME);
  let agentId: string;
  let token: string;
  if (existing) {
    agentId = existing._id;
    token = (await api(`/api/agents/${agentId}/token`, { method: "POST" })).token;
    console.log(`Reusing agent ${NAME} (${agentId}); rotated its token.`);
  } else {
    const r = await api("/api/agents", {
      method: "POST",
      body: JSON.stringify({
        name: NAME,
        module: "Discretion",
        description:
          "Discretion covers POA (power of attorney) setup and validation, check processing, card limits and CIP verification for advisory accounts. Changes to these flows, their APIs, rules config or tests are in scope; UI copy is not.",
        repos: [{ fullName: "acme/discretion-poa", branches: ["main"], paths: ["src/**/poa/**"], notes: "Owns POA validation and /poa/validate." }],
        mode: "action",
        teamsWebhookUrl: `${MOCK}/teams`,
        pipelines: [
          { name: "POA regression", type: "jenkins", jobPath: "discretion/poa", params: { SUITE: "poa" }, runWhen: "always" },
          { name: "CIP smoke", type: "teamcity", buildTypeId: "Discretion_CipSmoke", ref: "$CHANGE_BRANCH", params: { SUITE: "cip-smoke" }, paramPrefix: "env.", runWhen: "always" },
        ],
        notifyOnPipeline: "failure",
        servicenow: { enabled: true, query: "assignment_group.name=Discretion Support", postWorkNotes: true },
        splunk: { enabled: true, baseSearch: "index=discretion ERROR", windowBeforeMinutes: 120, windowAfterMinutes: 30, maxEvents: 20 },
      }),
    });
    agentId = r.agent._id;
    token = r.token;
    console.log(`Created agent ${NAME} (${agentId}).`);
  }

  const stamp = Date.now().toString(16).padStart(12, "0");
  const push = (after: string, files: object[], message: string) =>
    api("/api/ingest/github", {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
      body: JSON.stringify({
        agentId,
        repo: "acme/discretion-poa",
        branch: "main",
        before: "a".repeat(40),
        after,
        commits: [{ sha: after, message, author: "dev1" }],
        files,
      }),
    });

  const relevant = await push(
    ("b" + stamp).padEnd(40, "b"),
    [
      {
        filename: "src/main/java/com/acme/poa/PoaValidator.java",
        status: "modified",
        additions: 3,
        deletions: 1,
        // Deliberately contains realistic client data so you can see it masked before Claude:
        patch:
          "@@ -41,7 +41,9 @@ public boolean isExpired(Poa poa) {\n-    return poa.getExpiry().isBefore(LocalDate.now());\n+    return poa.getExpiry().isBefore(LocalDate.now(ZoneOffset.UTC));\n+    // repro: client ssn 123-45-6789, card 4111 1111 1111 1111, acct 000123456789\n",
      },
      { filename: "README.md", status: "modified", additions: 1, deletions: 0 },
    ],
    "Use UTC for POA expiry (reported by jane.client@gmail.com)",
  );
  console.log("Push 1 (touches POA code):", relevant);
  const filtered = await push(("d" + stamp).padEnd(40, "d"), [{ filename: "docs/onboarding.md", status: "modified", additions: 4, deletions: 0 }], "Docs tweak");
  console.log("Push 2 (docs only, should be path-filtered):", filtered);

  console.log(`
Next:
  1. Make sure the worker is running (npm run worker). Within ~10s it triages push 1,
     triggers the fake Jenkins job (fails 2 of 43 tests) and the fake TeamCity build (passes),
     and pulls a fake ServiceNow incident.
  2. Open ${APP} to see the change, the failed run and incident INC0099.
  3. Open ${APP}/chat, pick "Discretion" in Scope and ask (the mock Claude answers these):
       - Why did the last regression run fail?
       - Re-run poa regression for that change      (then click Confirm)
       - Show the client details on INC0099          (shows masked SSN/account/email)
`);
}

main().catch((e) => {
  console.error(e instanceof Error ? e.message : e);
  process.exit(1);
});
