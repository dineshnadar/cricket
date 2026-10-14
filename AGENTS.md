# AGENTS.md: project memory for coding agents

Read this first. It's the shared context for any AI coding tool working on this repo (Codex, Claude Code, Copilot, Cursor…). It captures what the project is, how it's built, the decisions already made and why, the rules you must not break, and what's next. `CLAUDE.md` points here.

---

## 1. What this is

**Wisdom Ops** is an internal web app plus a background worker. It watches the GitHub Enterprise repos behind a business module, initially the **discretion module** (POA, check, card and CIP verification) of a financial-services platform, built by many developers.

On every push or merged PR to a watched branch:
1. **Path filter:** changes outside the watched files stop here, with no AI call.
2. **Wisdom AI triage** (Claude under the hood): is the change relevant to the module, how confident, a summary, risk level and reasons, what to test, which pipelines to run.
3. **Pipelines run:** Jenkins, TeamCity or GitHub Actions.
4. **Notify:** email (SMTP) and Microsoft Teams.
5. **Track** each build to its result. Failure alerts name the change and its author.

Also:
- **Incidents:** ServiceNow (or manual **Log incident**) → Splunk search (optional) → Wisdom AI correlates the incident with the last 14 days of relevant changes and failed runs → likely cause, confidence, next steps.
- **Module wisdom:** a daily AI summary per repo and per module.
- **Wisdom Assistant** (`/chat`): a chat that answers from recorded data using tools, and can take actions (re-run a pipeline, create an agent, open setup PRs) only after a person clicks **Confirm**.

Users install nothing. That was a hard requirement: the team won't run npm or tools locally.

## 2. Status (as of 2026-10-14)

- All phases built and tested against **mock services**. Not yet deployed to the work environment; the code is about to be moved there.
- **Phase-1 reality at work:** no ServiceNow API and no Splunk yet. Incidents come in via **Log incident** (and later via XLSX import, see `docs/09-incident-dashboard-plan.md`).
- 74 tests pass (`npm test`), typecheck clean, `next build` OK.
- **Evals exist** (`npm run eval`, `docs/10-evals.md`): the team labels triage and incident results with *Was this right?* buttons; the script scores stored answers or re-runs the current prompt. No real labels yet; they get collected during the pilot.
- Docker was **removed on purpose**. Don't re-add a Dockerfile or compose file unless asked.

## 3. Stack

- **Next.js 15** (App Router) + React 19 + TypeScript (strict).
- **MongoDB** via the official driver v6. No ORM. Local dev used FerretDB, which is MongoDB-compatible (see gotchas).
- **zod 3** for all input validation.
- **@anthropic-ai/sdk**. **Only `src/lib/llm.ts` may call it** (see rules).
- nodemailer (SMTP), picomatch (globs), lucide-react (icons), react-markdown, @fontsource Fira Sans / Fira Code.
- **tsx** runs the worker and scripts; **vitest** for tests.
- No CSS framework: one hand-written `src/app/globals.css` with design tokens (dark default plus light theme). Design rules are in `docs/design-system.md` and `design-system/wisdom-ops/MASTER.md`.

## 4. Commands

```bash
npm run dev          # web app on :3000
npm run worker       # background worker (must run for triage, pipelines, incidents, wisdom)
npm test             # vitest
npm run typecheck    # tsc --noEmit
npm run build        # next build
npm run mock         # fake Claude/GitHub/Jenkins/TeamCity/ServiceNow/Splunk/Teams on :4000
npm run demo:seed    # demo agent + pushes (with the mock)
npm run eval -- --stored   # AI accuracy vs team labels; drop --stored to re-run the current prompt (real model, costs tokens); --min-recall/--min-hit3 gate
```

Local demo with no credentials: `cp .env.mock .env.local`, start MongoDB, then `mock`, `dev`, `worker`, `demo:seed`. See `docs/06-local-testing.md`. The worker and scripts load `.env.local`, then `.env`, via `src/lib/load-env.ts`; Next loads them itself. **Real environment variables override env files.**

## 5. Layout

```
src/
  app/                    Next.js pages + API routes
    agents/ new/ [id]/ [id]/edit/   agent list, create wizard, agent page (tabs: overview, changes, runs, wisdom, incidents, setup), edit
    changes/[id]  incidents/[id]  chat/
    api/
      ingest/github       POST from the per-repo GitHub Action (Bearer agent token)
      agents/             CRUD; [id]/{token,workflow,poll,wisdom,ask,incidents}; assist (AI form helpers)
      assistant/          chat (NDJSON stream), actions (confirm/cancel), conversations
      evals/labels        GET/POST "Was this right?" labels
      changes/[id] incidents/[id] github/{repos,branches} health
  components/             AgentForm (6-step wizard), AiAssist, Feedback (eval labels), Chat, Sidebar, client.tsx (small client widgets), ui.tsx
  lib/
    llm.ts                THE ONLY Claude gateway: masks every prompt (structured / freeText / chatStream)
    redact.ts             masking rules (SSN/ITIN, EIN, card+Luhn, account/routing, long numbers, DOB, phone, emails, secrets, custom)
    triage.ts             change → relevance/risk/tests (forced tool call + zod); TRIAGE_PROMPT_VERSION
    evals.ts              eval labels, metrics (pure), triage/incident eval runners, markdown report
    changes.ts            change queue: path filter → triage → threshold → pipelines → notify
    pipelines.ts jenkins.ts teamcity.ts github.ts   CI triggers + run tracking; GitHub API
    incidents.ts servicenow.ts splunk.ts           incident sync + analysis (computeIncidentAnalysis = no side effects; INCIDENT_PROMPT_VERSION)
    wisdom.ts             daily repo/module summaries
    notify.ts             email + Teams cards (changeMessage, runMessage, incidentMessage)
    poller.ts             central poller (agents in poll/both mode)
    setup-assist.ts       AI helpers for the agent form (module scope, repo notes, watched paths)
    field-quality.ts      instant "Empty / Too vague / OK / Good" hints (no AI, browser-safe)
    agent-steps.ts        wizard steps: validation, summaries, review warnings, server-error → step
    workflow-template.ts  generates .github/workflows/wisdom-ops.yml per repo
    assistant/            loop.ts (tool loop, max 8 rounds), tools.ts (read + write tools), store.ts (audit, pending actions), citations.ts, user.ts
    db.ts env.ts schemas.ts types.ts paths.ts http.ts util.ts load-env.ts agents.ts
  worker/index.ts         ticks: changes queue, pipeline refresh, poller (60s), ServiceNow poll + incident analysis, wisdom (15 min check)
mock/server.js            all fake services, incl. scripted Claude responses and a leak guard
scripts/demo-seed.ts  scripts/eval.ts (npm run eval)
tests/                    vitest
docs/                     01..09 + design-system + Confluence page + images
```

**MongoDB collections:**
- `agents`
- `changes` (unique: agentId + repo + after sha)
- `pipeline_runs`
- `poll_state`
- `wisdom`
- `incidents` (unique: agentId + sysId; manual ones use `manual-<id>`)
- `conversations`, `assistant_actions`, `audit`
- `eval_labels` (unique: kind + targetId; latest label wins)

Indexes are created in code by `ensureIndexes` in `src/lib/db.ts`. The app account at work **can** create indexes, so no DBA step is needed.

## 6. Rules you must not break

1. **Every LLM call goes through `src/lib/llm.ts`.** It masks the system prompt, user messages and tool results. `tests/llm-gateway.test.ts` fails the build if any other file calls `messages.create` / `messages.stream` or does `new Anthropic`. Use `structured()` (forced tool call + zod validator), `freeText()` or `chatStream()`, each with a `purpose` string.
2. **Mask client data before the LLM.** Hard requirement: SSNs and similar must never reach the model. Don't add a path around the gateway. Internal email domains stay readable via `REDACT_ALLOW_EMAIL_DOMAINS`; firm-specific ids go in `REDACT_EXTRA_PATTERNS` (an invalid JSON value throws on purpose).
3. **On screen, the AI is called "Wisdom AI"**, never "Claude". The docs and prompts may say Claude, since engineers and approvers need to know the model.
4. **The LLM never acts on its own.** Assistant write tools only create a pending action; it runs only on **Confirm**, claimed atomically. Never auto-execute.
5. **Bump the prompt version** (`TRIAGE_PROMPT_VERSION` / `INCIDENT_PROMPT_VERSION`) whenever you change a triage or incident prompt or schema, and run `npm run eval` before and after; don't ship a drop in triage recall.
6. **Citations:** Assistant answers may only link `[change:id]`, `[incident:id]` or `[agent:id]` that a tool actually returned. `citations.ts` removes the rest. Incident analysis drops causes that cite unknown change ids.
7. **Never invent owners, team names, job names or ids** in AI output. Ask, or leave `?`.
8. **No new dependencies without a reason.** The work environment installs through an internal npm mirror.
9. **Keep it runnable with the mock.** Any new external call needs a mock route in `mock/server.js` and a mock response for any new Claude tool name (`toolResult()` in the mock).
10. **Accessibility is part of done.** Labels tied to inputs, inline errors with `aria-describedby`, visible focus, 44px touch targets on touch devices, reduced motion respected, status never shown by colour alone.

## 7. Decisions already made (and why)

| Decision | Why |
|---|---|
| Claude as LLM; SMTP relay for email; Teams via workflow webhook | User choice / firm setup |
| Change detection: **per-repo GitHub Action** (default) + central **poller**; "Both" for safety | No admin rights to add GitHub webhooks; any dev can merge the setup PR. A native **org webhook** mode is designed but **not built** (needs GHE admin + inbound network) |
| Per-agent ingest token, only a SHA-256 hash stored, shown once, rotatable | Security; one org-level secret can serve all repos of an agent |
| Pipelines: Jenkins, **TeamCity** (`paramPrefix` env./system./plain; param names can't contain dots because of MongoDB), GitHub Actions (workflow_dispatch) | Team uses all three |
| **No embeddings / vector DB** (v1) | One module's data is small and structured; direct queries are exact and never stale. Add only if similar-incident search misses matches |
| Assistant = tool-using chat with confirm cards; **no SSO yet** (display name only) | Team needs a chat UI with nothing to install; SSO comes later |
| Relevance threshold (default 60%) gates pipelines and alerts; below it, the change is still recorded as `not-relevant` | Avoid alert noise without losing history |
| Agent form = **6-step wizard** with draft autosave (localStorage), step in URL (`?step=`), review step with warnings, AI helpers on the free-text fields | Team won't write detailed config text |
| Docker removed; deploy as Node app (VM or K8s with an approved base image later) | Simplicity; bank base images differ |
| Live animated "flow" page: **rejected** as overkill | User decision |
| Evals from **team labels in the app** (Yes/No on changes, actual cause on incidents), not a hand-built spreadsheet; scored on what the system *acted on* (relevant AND ≥ threshold) | Cheapest way to get ground truth during the pilot; measures operational misses |
| Old incidents: **same `incidents` collection** with `source` + `classification` (not a separate historical collection) | The daily export contains both old and current incidents |

## 8. Gotchas

- **FerretDB** (local) doesn't support `$max` in updates or `arrayFilters`; the code computes in JS or uses read-modify-write. Keep it that way so local dev keeps working.
- The Mongo connect promise is reset on failure (`db.ts`); otherwise one failed connect sticks forever.
- **`APP_BASE_URL` is written into generated workflow files.** If it's `localhost`, every Action run fails; the `WISDOM_URL` GitHub variable overrides it.
- GitHub Actions only run the workflow file **present on the pushed branch**: old `release/*` branches miss it, so use "Both" mode. Bot merges made with `GITHUB_TOKEN` don't trigger workflows.
- GitHub-hosted runners can't reach internal URLs; at the bank, set `WISDOM_RUNNER` to a self-hosted runner label.
- Incident analysis only considers changes with `triage.relevant = true`, received in the 14 days before **Opened at**. The failed-runs query has no upper bound (known, minor).
- In uncontrolled inputs (`defaultValue` + `onBlur`), a button's label won't update while typing; prefer controlled inputs in the wizard.
- A sandbox or CI environment may already set `ANTHROPIC_BASE_URL` / `ANTHROPIC_API_KEY`, and those **override** `.env.local`. Unset them when testing with the mock.
- `details:first-of-type` in globals.css removes the top border from the first `<details>`; use `details.advanced` (or similar) for standalone collapsibles.

## 9. What's next (prioritised)

1. **Move the code to the work environment:**
   - clean repo, first commit, tag `v0.1`;
   - check with Synechron and the client about code ownership and AI-assisted code policy;
   - licence scan;
   - regenerate `package-lock.json` against the internal npm mirror;
   - pin Node 20/22;
   - approved Claude endpoint via `ANTHROPIC_BASE_URL`;
   - service accounts.
2. **Pilot:** `DRY_RUN=true` on one or two repos for a week. The module lead labels each day's changes (*Was this triage right?*, about 50 total), then runs `npm run eval -- --stored` and tunes the module scope and threshold from the misses.
3. **Incident import + classification + dashboard:** see `docs/09-incident-dashboard-plan.md` (phases 1–4). Needs a sample export (column headers) first.
4. Later: native GitHub webhook mode / GitHub App; PR comments (triage before merge); SSO + per-module access; deploy events; daily digest / alert throttling; evals for Assistant answers (a fixed question set) and for the AI form helpers; a "Rate this triage" link directly in alert emails.

## 10. Docs map

| File | What |
|---|---|
| `docs/01-overview.md` | Problem, what the app does, phases, limits |
| `docs/02-architecture.md` | Components, data flow, data model, masking |
| `docs/03-user-guide.md` | Setting up an agent (wizard, AI helpers, relevance threshold), pushes, alerts, incidents (step 5, Log incident) |
| `docs/04-wisdom-assistant.md` | Why/where the LLM is used, knowledge layers, tools, guardrails |
| `docs/05-test-pipelines.md` | Jenkins / TeamCity / GitHub Actions setup |
| `docs/06-local-testing.md` | Running everything with mocks |
| `docs/07-configuration-and-deployment.md` | Env vars, hosting, security checklist |
| `docs/08-github-workflow.md` | The per-repo workflow line by line, token handling, troubleshooting |
| `docs/09-incident-dashboard-plan.md` | Plan: XLSX import, classification, management/ops/dev/business dashboard |
| `docs/10-evals.md` | Labelling Wisdom AI's decisions and measuring accuracy with `npm run eval` |
| `docs/design-system.md`, `design-system/wisdom-ops/MASTER.md` | UI rules and tokens |
| `docs/Wisdom-Ops-Confluence.md` | One-page summary for Confluence |

## 11. Working style the owner expects

- Direct, honest feedback over encouragement; say plainly when something is a bad idea or isn't built.
- Concrete, copy-paste-ready output.
- The docs must only claim what the code does. When you add or change a feature, update the matching doc in the same change.
- Verify before saying "done": run tests, typecheck and build, and exercise the flow against the mock.
