// Wisdom Ops mock services for local testing: fakes Claude (incl. streaming chat), Jenkins, GitHub Enterprise,
// ServiceNow, Splunk and a Teams webhook on one port. Start with `npm run mock`. Not for production.
const http = require("http");
const log = (...a) => console.log("[mock]", ...a);
const PORT = Number(process.env.MOCK_PORT || 4000);
const B = `http://127.0.0.1:${PORT}`;

function toolResult(name, prompt) {
  switch (name) {
    case "record_triage":
      return {
        relevant: true, confidence: 0.92,
        summary: "- Tightens `PoaValidator.isExpired` to use UTC.\n- Affects POA expiry checks on account maintenance.",
        impactedAreas: ["POA"], riskLevel: "high", riskReasons: ["Date handling change on a validation path"],
        testFocus: ["POA expiring today in non-UTC timezones"], recommendedPipelines: ["POA regression", "Nonexistent"],
      };
    case "record_terms":
      return { terms: ["PoaExpiredException", "/poa/validate"] };
    case "record_incident_analysis": {
      const m = prompt.match(/<change id="([a-f0-9]{24})"/);
      return {
        summary: "POA validations started failing after the UTC expiry change.",
        likelyCauses: [
          { changeId: m ? m[1] : "000000000000000000000000", reason: "Changes isExpired; logs show PoaExpiredException", confidence: 0.8 },
          { changeId: "ffffffffffffffffffffffff", reason: "hallucinated id", confidence: 0.3 },
        ],
        evidence: ["PoaExpiredException at /poa/validate"], suggestedActions: ["Revert abc1234"],
      };
    }
    case "record_module_scope": {
      const rough = (prompt.match(/<rough>\n([\s\S]*?)\n<\/rough>/) || [])[1] || "";
      return {
        inScope: ["POA setup and validation", "check deposit and processing", "card issuance and limits", "CIP identity verification for advisory accounts"],
        outOfScope: /statement/i.test(rough) ? ["client statements", "marketing pages"] : ["marketing pages", "reporting"],
        criticalFlows: ["POA expiry checks", "CIP pass/fail decisions"],
        questions: ["Do card disputes belong to this module?", "Are shared libraries (e.g. common-auth) in scope?"],
      };
    }
    case "record_repo_notes":
      return {
        whatItDoes: "POA service: validates and stores powers of attorney for advisory accounts.",
        criticalCode: ["PoaValidator", "ExpiryService", "/api/poa/*", "poa-rules.yml"],
        watchOutFor: ["date and time-zone handling", "validation rule changes"],
        owner: /owner|team/i.test((prompt.match(/<rough>\n([\s\S]*?)\n<\/rough>/) || [])[1] || "") ? "POA platform team" : "",
        questions: ["Is the renewal flow (RenewalController) still used?"],
      };
    case "record_watched_paths":
      return {
        include: ["src/main/java/**/poa/**", "src/main/resources/poa-rules.yml", "src/test/java/**/poa/**", "src/main/java/**/legacy/**"],
        exclude: ["**/*.md", "docs/**"],
        reasoning: "POA source, its rules config and tests; docs excluded.",
      };
    case "record_repo_wisdom":
      return {
        purpose: "Owns power-of-attorney setup and validation for advisory accounts, including the /poa/validate endpoint used by the advisor workstation.",
        keyComponents: ["PoaValidator: expiry, scope and signer checks", "PoaController: /poa/validate and /poa/renew endpoints", "PoaRepository: POA documents and audit trail"],
        recentActivity: "- Expiry check moved to UTC (high risk, see latest change)\n- Renewal flow refactor in progress",
        hotspots: ["src/main/java/com/acme/poa/PoaValidator.java", "date and time-zone handling"],
        risks: ["Time-zone boundaries around expiry dates", "No test covers POAs expiring on the current day"],
        testingNotes: "Run POA regression on every change; add a boundary test for expiry-day validation in ET and PT.",
      };
  }
  return {};
}


// ---- streaming chat mock (Wisdom Assistant) ----
function sse(res, blocks, stopReason, model) {
  res.writeHead(200, { "Content-Type": "text/event-stream", "Cache-Control": "no-cache" });
  const send = (event, data) => res.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`);
  send("message_start", { type: "message_start", message: { id: "msg_s", type: "message", role: "assistant", model, content: [], stop_reason: null, stop_sequence: null, usage: { input_tokens: 10, output_tokens: 1 } } });
  blocks.forEach((b, i) => {
    if (b.type === "text") {
      send("content_block_start", { type: "content_block_start", index: i, content_block: { type: "text", text: "" } });
      for (const chunk of b.text.match(/.{1,24}/gs) || []) send("content_block_delta", { type: "content_block_delta", index: i, delta: { type: "text_delta", text: chunk } });
    } else {
      send("content_block_start", { type: "content_block_start", index: i, content_block: { type: "tool_use", id: b.id, name: b.name, input: {} } });
      send("content_block_delta", { type: "content_block_delta", index: i, delta: { type: "input_json_delta", partial_json: JSON.stringify(b.input) } });
    }
    send("content_block_stop", { type: "content_block_stop", index: i });
  });
  send("message_delta", { type: "message_delta", delta: { stop_reason: stopReason, stop_sequence: null }, usage: { output_tokens: 20 } });
  send("message_stop", { type: "message_stop" });
  res.end();
}
function chatReply(j) {
  const msgs = j.messages;
  let q = ""; let step = 0; const results = [];
  for (let i = msgs.length - 1; i >= 0; i--) {
    const m = msgs[i];
    if (m.role === "user" && typeof m.content === "string") { q = m.content; break; }
    if (m.role === "user" && Array.isArray(m.content)) { step++; results.unshift(...m.content.filter(c => c.type === "tool_result").map(c => c.content)); }
  }
  const hist = msgs.map(m => typeof m.content === "string" ? m.content : "").join("\n");
  const tu = (name, input) => ({ type: "tool_use", id: "toolu_" + name + step, name, input });

  // ---- setup a new agent (multi-turn) ----
  const SETUP_ASK = "To set up the agent, tell me";
  const createdId = (hist.match(/Created agent .*?\(id ([a-f0-9]{24})\)/) || [])[1];
  if (/setup prs|set-?up pull|pull requests?|open (the )?prs/i.test(q) && createdId) {
    if (step === 0) return [[{ type: "text", text: "I'll open a pull request in each repo that adds the Wisdom Ops workflow." }, tu("open_setup_prs", { agent: createdId })], "tool_use"];
    return [[{ type: "text", text: "Click **Confirm** to open them. Each repo also needs the `WISDOM_TOKEN` secret." }], "end_turn"];
  }
  if (hist.includes(SETUP_ASK) && !/Create agent "/.test(hist) && !/set ?up|new agent|new module/i.test(q)) {
    const rm = q.match(/([\w.-]+)\/([\w.-]*\w)/); // repo names don't end with punctuation
    const repoMatch = rm ? [rm[0], rm[1], rm[2]] : null;
    const moduleName = ((q.match(/module(?: name)?\s*(?:is|:|=)?\s*([A-Za-z][\w -]{1,30}?)(?=[,.;\n]|$)/i) || [])[1] || "New module").trim();
    if (step === 0) return [[{ type: "text", text: "Let me find that repo in GitHub Enterprise." }, tu("search_github_repos", { query: repoMatch ? repoMatch[2] : moduleName })], "tool_use"];
    if (step === 1) {
      const found = (results.join("").match(/"repo": "([^"]+)"/) || [])[1];
      const repo = repoMatch ? repoMatch[0] : found || "acme/discretion-poa";
      const pipelines = [];
      const jm = q.match(/jenkins[^\w]*(?:job)?\s*[:=]?\s*([\w-]+\/[\w\/-]+)/i);
      if (/jenkins/i.test(q)) pipelines.push({ name: "Regression", type: "jenkins", jobPath: jm ? jm[1] : "discretion/poa", runWhen: "always" });
      const tm = q.match(/teamcity[^\w]*(?:build)?\s*[:=]?\s*([A-Za-z][\w]+_[\w]+)/i);
      if (/teamcity/i.test(q)) pipelines.push({ name: "Smoke", type: "teamcity", buildTypeId: tm ? tm[1] : "Discretion_CipSmoke", ref: "$CHANGE_BRANCH", runWhen: "always" });
      const emails = q.match(/[\w.+-]+@[\w.-]+\.[a-z]{2,}/gi) || [];
      const description = q.length >= 20 ? q : `${moduleName} module: all code, config and tests in ${repo}.`;
      return [[
        { type: "text", text: `Found **${repo}**. Here's the agent I'd create:` },
        tu("create_agent", { config: { name: `${moduleName} watcher`, module: moduleName, description, repos: [{ fullName: repo, branches: ["main"] }], pipelines, emailRecipients: emails } }),
      ], "tool_use"];
    }
    return [[{ type: "text", text: "Review the card and click **Confirm**. After that, ask me to **open the setup PRs** and I'll add the workflow to the repo." }], "end_turn"];
  }
  if (/set ?up|new agent|new module|onboard/i.test(q)) {
    return [[{ type: "text", text: `Happy to. ${SETUP_ASK} (one message is fine):\n\n1. **Module name** and what belongs to it (and what's out of scope)\n2. **Repos** to watch (owner/name)\n3. **Test pipelines**: Jenkins job path and/or TeamCity build configuration id\n4. **Who to notify**: email addresses and/or a Teams channel webhook\n\nExample: *Module: Discretion. Covers POA, check, card and CIP verification. Repo acme/discretion-poa. Jenkins job discretion/poa, TeamCity Discretion_CipSmoke. Notify team@acme.com*` }], "end_turn"];
  }
  // Demo of the citation check: cites a change it never looked up; the server removes the link.
  if (/made.?up|hallucinat|without looking/i.test(q)) {
    return [[{ type: "text", text: "From memory, the last POA outage came from the timezone refactor [change:0123456789abcdef01234567]." }], "end_turn"];
  }
  if (/fail/i.test(q)) {
    if (step === 0) return [[{ type: "text", text: "Let me check the recent failed runs." }, tu("list_runs", { status: "failure", since_days: 7 })], "tool_use"];
    if (step === 1) { const cid = (results.join("").match(/"change_id": "([a-f0-9]{24})"/) || [])[1]; return [[tu("get_change", { change_id: cid })], "tool_use"]; }
    const cid = (results.join("").match(/"change_id": "([a-f0-9]{24})"/) || [])[1];
    return [[{ type: "text", text: `**poa-regression failed: 2 of 43 tests.** It ran for the push that moved \`PoaValidator.isExpired\` to UTC [change:${cid}].\n\n- The risk was flagged at push time: date handling on a validation path.\n- Test focus was POAs expiring today in non-UTC time zones.\n\nNext: check the failing tests in the build, or I can re-run the pipeline.` }], "end_turn"];
  }
  if (/re-?run|trigger/i.test(q)) {
    const cid = (hist.match(/\[change:([a-f0-9]{24})\]/) || [])[1];
    if (step === 0) return [[{ type: "text", text: "I'll queue poa-regression for that change." }, tu("trigger_pipeline", { change_id: cid, pipeline: "POA regression" })], "tool_use"];
    return [[{ type: "text", text: "Click **Confirm** on the card to start it. Nothing runs until you do." }], "end_turn"];
  }
  if (/ssn|client/i.test(q)) {
    if (step === 0) return [[tu("get_incident", { incident: "INC0099" })], "tool_use"];
    return [[{ type: "text", text: "Here's what the tool returned (masked): " + results.join("").slice(0, 200) }], "end_turn"];
  }
  return [[{ type: "text", text: "I couldn't find anything on that in the recorded changes. Try naming a repo or date." }], "end_turn"];
}

const routes = [];
const on = (method, re, fn) => routes.push({ method, re, fn });

// Guard: shout if anything that looks like unmasked client data reaches "Claude".
const LEAKS = [[/\b\d{3}-\d{2}-\d{4}\b/, "SSN"], [/\b4111[ -]?1111[ -]?1111[ -]?1111\b/, "card"], [/[\w.]+@gmail\.com/i, "client email"], [/\b000123456789\b/, "account"]];
on("POST", /^\/v1\/messages$/, (req, body, res) => {
  for (const [re, what] of LEAKS) if (re.test(body)) log(`!!! UNMASKED ${what} RECEIVED BY CLAUDE !!!`);
  const j = JSON.parse(body);
  if (j.stream) { const [blocks, stop] = chatReply(j); log("claude stream", stop, blocks.map(b => b.name || "text").join(",")); sse(res, blocks, stop, j.model); return null; }
  const prompt = j.messages[0].content;
  const content = j.tool_choice
    ? [{ type: "tool_use", id: "toolu_1", name: j.tool_choice.name, input: toolResult(j.tool_choice.name, prompt) }]
    : [{ type: "text", text: /module overview/i.test(prompt)
        ? "### How it fits together\nPOA, check, card and CIP verification live in separate services. **poa-service** validates power-of-attorney documents for every advisory account change.\n\n### Where the risk is now\n- **POA expiry handling**: the move to UTC changes behaviour for documents expiring today.\n- **Test gap**: no regression test covers the expiry-day boundary.\n\n### Watch this week\n- Failures on /poa/validate after 8 PM ET\n- poa-regression results on the release branch"
        : "## Overview\nThe POA repo changed recently; see [change:" + ((prompt.match(/\[change:([a-f0-9]{24})\]/) || [])[1] || "") + "]." }];
  log("claude", j.tool_choice?.name ?? "text", j.model);
  return [200, { id: "msg_1", type: "message", role: "assistant", model: j.model, content, stop_reason: j.tool_choice ? "tool_use" : "end_turn", stop_sequence: null, usage: { input_tokens: 1, output_tokens: 1 } }];
});
// Jenkins
on("GET", /^\/crumbIssuer/, () => [404, {}]);
on("POST", /^\/job\/discretion\/job\/poa\/buildWithParameters$/, (req, body) => {
  log("jenkins trigger", body);
  return [201, "", { Location: B + "/queue/item/1/" }];
});
on("GET", /^\/queue\/item\/1\/api\/json$/, () => [200, { executable: { number: 7, url: B + "/job/discretion/job/poa/7/" } }]);
on("GET", /^\/job\/discretion\/job\/poa\/7\/api\/json/, () => [200, { building: false, result: "FAILURE", duration: 1000 }]);
on("GET", /^\/job\/discretion\/job\/poa\/7\/testReport\/api\/json/, () => [200, { failCount: 2, passCount: 40, skipCount: 1 }]);
// TeamCity: every queued build "finishes" SUCCESS with 118 passed / 0 failed / 2 ignored tests
let tcId = 9000;
const tcBuilds = new Map();
on("POST", /^\/app\/rest\/buildQueue$/, (req, body) => {
  const j = JSON.parse(body);
  const id = ++tcId;
  tcBuilds.set(id, j);
  log("teamcity trigger", j.buildType.id, j.branchName || "(default branch)", j.properties.property.map((p) => p.name).join(","));
  return [200, { id, state: "queued", webUrl: `${B}/tc/viewLog.html?buildId=${id}` }];
});
on("GET", /^\/app\/rest\/builds\/id:(\d+)/, (req) => {
  const id = Number(req.url.match(/id:(\d+)/)[1]);
  return [200, { id, number: String(id - 8900), state: "finished", status: "SUCCESS", statusText: "Tests passed: 118, ignored: 2", webUrl: `${B}/tc/viewLog.html?buildId=${id}`, testOccurrences: { count: 120, passed: 118, failed: 0, ignored: 2 } }];
});
on("GET", /^\/downloadBuildLog\.html/, () => [200, "[12:00:01] Step 1/2: Gradle test\n[12:03:44] Tests passed: 118, ignored: 2\n[12:03:45] Build finished"]);
// Teams
on("POST", /^\/teams$/, (req, body) => {
  const card = JSON.parse(body).attachments[0].content;
  const facts = card.body.flatMap((b) => b.facts ?? []).map((f) => `${f.title}=${f.value}`).join(" | ");
  log("teams", card.body[0].text, facts ? `[${facts}]` : "");
  return [200, "1"];
});
// GitHub
on("GET", /^\/gh\/repos\/acme\/discretion-poa$/, () => [200, { full_name: "acme/discretion-poa", description: "POA", default_branch: "main", private: true, pushed_at: null, html_url: "x" }]);
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/readme$/, () => [200, "# POA service\n\nValidates and stores powers of attorney for advisory accounts. Exposes /api/poa/validate and /api/poa/renew."]);
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/commits\?/, () => [200, [{ sha: "c".repeat(40), html_url: "u", commit: { message: "UTC fix", author: { name: "dev1", email: "e", date: "2026-10-13T00:00:00Z" } }, author: { login: "dev1" } }]]);
const POA_TREE = [
  "README.md", "docs/architecture.md", "pom.xml",
  "src/main/java/com/acme/poa/PoaValidator.java", "src/main/java/com/acme/poa/ExpiryService.java",
  "src/main/java/com/acme/poa/PoaController.java", "src/main/java/com/acme/poa/RenewalController.java",
  "src/main/java/com/acme/common/HttpClient.java", "src/main/resources/poa-rules.yml",
  "src/main/resources/application.yml", "src/test/java/com/acme/poa/PoaValidatorTest.java",
  "src/x/poa/PoaValidator.java",
];
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/git\/trees\//, () => [200, { tree: POA_TREE.map((path) => ({ path, type: "blob" })) }]);
// Any repo whose name contains "missing" doesn't exist (to demo the setup check).
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]*missing[\w.-]*$/, () => [404, { message: "Not Found" }]);
on("GET", /^\/gh\/repos\/([\w.-]+)\/([\w.-]+)$/, (req) => { const [, o, r] = req.url.match(/^\/gh\/repos\/([\w.-]+)\/([\w.-]+)$/); return [200, { full_name: `${o}/${r}`, description: "", default_branch: "main", private: true, pushed_at: null, html_url: "x" }]; });
on("GET", /^\/gh\/search\/repositories/, () => [200, { items: [{ full_name: "acme/discretion-poa", description: "POA", default_branch: "main", private: true, pushed_at: null, html_url: "x" }] }]);
on("GET", /^\/gh\/repos\/acme\/discretion-poa\/branches/, () => [200, [{ name: "main" }, { name: "release/1" }]]);
// GitHub: endpoints used to open the setup PR
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/pulls\?/, () => [200, []]);
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/git\/ref\/heads\//, () => [200, { object: { sha: "c".repeat(40) } }]);
on("POST", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/git\/refs$/, () => [201, { ref: "refs/heads/wisdom-ops/setup" }]);
on("GET", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/contents\/\.github/, () => [404, { message: "Not Found" }]);
on("PUT", /^\/gh\/repos\/[\w.-]+\/[\w.-]+\/contents\//, (req) => { log("github put workflow", req.url.split("?")[0]); return [201, { content: {} }]; });
let prNum = 40;
on("POST", /^\/gh\/repos\/([\w.-]+)\/([\w.-]+)\/pulls$/, (req) => { prNum++; log("github open PR #" + prNum, req.url); return [201, { number: prNum, html_url: `${B}/gh-ui/pull/${prNum}` }]; });
// ServiceNow
on("GET", /^\/api\/now\/table\/incident\?/, (req) => {
  log("servicenow query", decodeURIComponent(req.url));
  return [200, { result: [{ sys_id: "abc123", number: "INC0099", short_description: "POA validation failing", description: "Users get PoaExpiredException on /poa/validate", priority: "2", state: "1", opened_at: new Date(Date.now() + 60000).toISOString().replace("T", " ").slice(0, 19) }] }];
});
on("PATCH", /^\/api\/now\/table\/incident\/abc123$/, (req, body) => { log("worknote", JSON.parse(body).work_notes.split("\n")[0]); return [200, { result: {} }]; });
// Splunk
on("POST", /^\/services\/search\/jobs\/export$/, (req, body) => {
  log("splunk", new URLSearchParams(body).get("search"));
  return [200, '{"preview":false,"result":{"_raw":"ERROR PoaExpiredException at /poa/validate","_time":"2026-10-13T17:00:00","host":"app1"}}\n'];
});

http.createServer((req, res) => {
  let body = "";
  req.on("data", (c) => (body += c));
  req.on("end", () => {
    const r = routes.find((x) => x.method === req.method && x.re.test(req.url));
    if (!r) { log("404", req.method, req.url); res.writeHead(404); return res.end("{}"); }
    const out = r.fn(req, body, res);
    if (out === null) return;
    const [status, payload, headers = {}] = out;
    res.writeHead(status, { "Content-Type": typeof payload === "string" ? "text/plain" : "application/json", ...headers });
    res.end(typeof payload === "string" ? payload : JSON.stringify(payload));
  });
}).listen(PORT, () => log(`mock services on ${B}`));
