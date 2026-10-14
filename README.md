# Wisdom Ops

Agents that watch the repos behind a module (e.g. the **discretion module**: POA, check, card, CIP verification), use Claude to triage every push, trigger the right test pipelines, tell the team, and correlate production incidents with recent changes.

**Documentation:** see [`docs/`](docs/README.md): overview, architecture, user guide with screenshots, Wisdom Assistant, test pipelines, local testing, configuration, and a Confluence-ready page.

```
 push to any watched repo
        │  (GitHub Action in the repo, or central poller)
        ▼
 POST /api/ingest/github ──► MongoDB `changes` (deduped on repo + head sha)
                                   │  worker
                                   ▼
                path filter ─► Claude triage (relevant? risk? which tests?)
                                   │ relevant & ≥ threshold
                     ┌─────────────┼──────────────┐
                     ▼             ▼              ▼
              Jenkins / GH     Email (SMTP)    Teams card
              Actions runs
                     │ tracked until done → notify on failure
                     ▼
 ServiceNow incident ─► Claude picks log terms ─► Splunk search ─► Claude correlates
                        with last 14 days of relevant changes ─► notify + work note
```

## What's in each phase

| Phase | Feature | Where |
|---|---|---|
| 1 | Create Agent flow: module scope, repo picker (GHE search), per-repo branches / path globs / notes, pipelines, recipients | `/agents/new`, `src/components/AgentForm.tsx` |
| 1 | Per-repo GitHub workflow (no webhook admin needed), generated per repo | Agent → **Setup** tab, `src/lib/workflow-template.ts` |
| 1 | Ingest API with per-agent token, MongoDB storage, dedupe | `src/app/api/ingest/github/route.ts`, `src/lib/changes.ts` |
| 1 | Claude triage: relevance, confidence, summary, risk, test focus, recommended pipelines | `src/lib/triage.ts` |
| 1 | Email (SMTP) + Teams notifications | `src/lib/notify.ts` |
| 1 | Test pipeline triggers: Jenkins (`buildWithParameters`), TeamCity (`/app/rest/buildQueue`), GitHub Actions (`workflow_dispatch`) | `src/lib/jenkins.ts`, `src/lib/teamcity.ts`, `src/lib/pipelines.ts` |
| 2 | Run tracking (queue → build → result + JUnit counts), notify on failure/always | `src/lib/pipelines.ts` |
| 2 | Dashboard, per-agent changes / runs views, change detail with diff | `src/app` |
| 3 | Central poller for repos where the Action can't be installed (`poll` / `both` modes) | `src/lib/poller.ts` |
| 3 | Module **wisdom**: daily Claude summaries per repo + module overview | `src/lib/wisdom.ts`, Agent → **Wisdom** tab |
| 3 | **Ask the module**: grounded Q&A over wisdom, changes, runs, incidents, with links | Agent → **Overview** |
| 4 | ServiceNow incident polling, manual incident entry | `src/lib/incidents.ts`, `src/lib/servicenow.ts` |
| 4 | Splunk evidence search + Claude correlation with recent changes, work note back to ServiceNow | `src/lib/splunk.ts`, `src/lib/incidents.ts` |

## Test locally with mocks (no credentials needed)

`npm run mock` fakes Claude, Jenkins, GitHub Enterprise, ServiceNow, Splunk and a Teams webhook on port 4000, so you can try the whole flow on a laptop. You only need Node 20+ and MongoDB.

```bash
cp .env.mock .env.local          # points every integration at the mock
# MongoDB must be running on localhost:27017 (see docs/06-local-testing.md)

# four terminals:
npm run mock                     # 1. fake services (watch this log: Jenkins/TeamCity triggers, Teams cards, work notes)
npm run dev                      # 2. app on http://localhost:3000
npm run worker                   # 3. background worker
npm run demo:seed                # 4. creates a demo agent + two pushes (rerun any time)
```

Within about 10 seconds you should see:
- Push 1 triaged as **high risk**. It triggers the fake Jenkins job, which "fails" 2 of 43 tests, and the fake TeamCity build *CIP smoke*, which passes 118 tests with 2 ignored. Teams cards appear in the mock log.
- Push 2 marked **path-filtered**, because it only touches docs.
- Incident **INC0099** pulled from the fake ServiceNow, searched in fake Splunk, linked to push 1, with a work note posted.

Then open `/chat`, pick **Discretion** in Scope, and ask the questions the mock Claude has scripted answers for:
1. `Why did the last regression run fail?`
2. `Re-run poa regression for that change`, then click **Confirm**. A new Jenkins trigger appears in the mock log.
3. `Show the client details on INC0099`. The answer shows SSN, account and email masked.
4. `Set up a new agent for a module`. The Assistant asks for details, so reply in one message, for example: *Module: Card Limits. Covers card issuance and daily limits. Repo acme/card-service. Jenkins job discretion/card-regression, TeamCity Discretion_CardSmoke. Notify cards-team@acme.com*. It searches the repo, shows a **Create agent** card, and after you confirm it shows the ingest token once. Then say `open the setup PRs` and confirm. The mock log shows the workflow file and the PR being created.

Any other question gets a canned "couldn't find anything" reply. **To try real answers**, put your real `ANTHROPIC_API_KEY` in `.env.local` and delete the `ANTHROPIC_BASE_URL` line. Claude then answers anything over the demo data, while Jenkins, GitHub, ServiceNow and Splunk stay fake. Email is off in the mock, so email notifications show as failed unless you set `SMTP_HOST`.

## Wisdom Assistant (chat)

`/chat` lets the team ask questions and take actions without installing anything.

- **What it can look up:** recorded changes and diffs, files in watched repos, test runs and Jenkins console logs, incidents with their analysis, Splunk (scoped to the agent's base search), module wisdom, activity stats, and GitHub Enterprise repo search.
- **What it can do:** trigger a pipeline, re-run triage, analyse an incident, refresh wisdom, create or update an agent, and open the setup PRs in each repo. Every action shows a card first, and **nothing runs until someone clicks Confirm**.
- **How it answers:** Claude decides which lookups to make (up to 8 rounds per question; a round can run several). The server runs them and returns trimmed, masked results, and Claude answers with links to the changes and incidents it used. Earlier turns are sent back as text only, so conversations stay cheap.
- **Client data:** every Claude call goes through one gateway (`src/lib/llm.ts`) that masks SSNs/ITINs, card and account numbers, long ids, dates of birth, phones, non-internal emails and secrets, including what users type in chat. A test fails the build if anything bypasses it. Set `REDACT_ALLOW_EMAIL_DOMAINS` and `REDACT_EXTRA_PATTERNS` for your environment. Names and free-text addresses aren't caught. See `docs/02-architecture.md`.
- **Audit:** every question, lookup and action, with the user and a timestamp, goes to the `audit` collection.
- **Identity:** there's no SSO yet. Users enter a display name, which is used for the audit log and to keep conversation lists separate. That isn't access control, so put the app behind `APP_BASIC_AUTH` or your network boundary until SSO is added.
- **Setup PRs** need the server's `GITHUB_TOKEN` to have `contents:write` and `pull_requests:write` on the watched repos.

## Run it locally

Requirements: Node 20+ and MongoDB.

```bash
cp .env.example .env        # fill in ANTHROPIC_API_KEY at minimum
npm install

npm run dev                 # web UI + API on http://localhost:3000
npm run worker              # second terminal: background worker
```

Set `DRY_RUN=true` to try everything without sending email/Teams or triggering pipelines.

## Set up the discretion module

1. **Create agent** → name it, and in *What belongs to this module* describe POA / check / card / CIP verification and what's out of scope. Claude's relevance decision is only as good as this paragraph.
2. **Add repos.** For each one set branches (`main, release/*`), path globs (`src/**/poa/**`, `!**/*.md`) and a line on what matters there and who owns it.
3. **Add pipelines.** Jenkins job path like `discretion/poa-regression`. Choose *Every relevant change* or *Only when Claude recommends it*. Every build gets `WISDOM_REPO`, `WISDOM_BRANCH`, `WISDOM_SHA`, `WISDOM_CHANGE_ID`, `WISDOM_RISK` plus your static params, so the job can check out the exact commit.
4. **Save** → copy the ingest token (shown once).
5. **Setup tab** → for each repo:
   - add secret `WISDOM_TOKEN` (repo or org level),
   - commit the generated workflow as `.github/workflows/wisdom-ops.yml`,
   - if Actions runners can't reach `APP_BASE_URL`, set variable `WISDOM_URL` to an address they can, and `WISDOM_RUNNER` to a self-hosted runner label.
6. Push a change. Within ~15 s the worker triages it, starts the pipelines and notifies the team.

The workflow uses `actions/github-script` and the job's own `GITHUB_TOKEN` to diff the push. It needs no checkout or extra tools on the runner, and it sends file patches capped at 20 KB each.

## Configuration notes

- **Claude**: `ANTHROPIC_MODEL` defaults to `claude-sonnet-5-5`. `ANTHROPIC_BASE_URL` points the SDK at a corporate gateway if you have one. The app sends diffs to the model, so confirm this is cleared with your security team before connecting production repos.
- **GitHub Enterprise**: `GITHUB_API_URL=https://github.yourco.com/api/v3`. The token needs repo read access for the picker, poller and wisdom, plus `actions:write` if you use GitHub Actions pipelines. `GITHUB_ORGS` scopes repo search.
- **Jenkins**: a user + API token. Jobs must be parameterised (or have no params). Test counts come from the JUnit `testReport` if the job publishes one.
- **TeamCity**: `TEAMCITY_URL` + an access token (`TEAMCITY_TOKEN`) whose user can run and view the build configurations. Each pipeline names a **build configuration id** (e.g. `Discretion_PoaRegression`) and a branch: the config default, a fixed branch, or *same branch as the push*. Change context is sent as `env.WISDOM_REPO`, `env.WISDOM_SHA`, `env.WISDOM_CHANGE_ID` and so on. Write your own parameters as plain names (`SUITE=poa`) and pick how they're sent with *Send parameters as*: `env.` (default), `system.` or configuration parameters. Test counts and build logs come from TeamCity's REST API, and the Assistant can read the log of a failed TeamCity build.
- **Teams**: create a channel Workflow, *"Post to a channel when a webhook request is received"*, and paste its URL into the agent. Adaptive Card payloads work with these.
- **ServiceNow**: the Table API with basic auth. The agent's encoded query scopes incidents (e.g. `assignment_group.name=Discretion Support`). Enable *work notes* to post the analysis back.
- **Splunk**: management port (`:8089`) with an auth token. The agent's base search is narrowed by terms Claude pulls from the incident, over a window around the open time. For internal CAs, set `NODE_EXTRA_CA_CERTS=/path/to/ca.pem`.
- **UI auth**: `APP_BASIC_AUTH=user:pass` puts basic auth in front of everything except `/api/ingest/*` and `/api/health`. Put it behind your SSO proxy for real use.

## Limits worth knowing

- Poll mode can't expand branch globs (`release/*`); list concrete branches for polling, or use the Action.
- Incident ↔ change correlation is based on when a change was *received*, not when it was *deployed*. Claude is told to weigh timing accordingly. Feeding deploy events in is the natural next step.
- GitHub `workflow_dispatch` inputs must be declared in the target workflow. Wisdom Ops sends your static params plus `wisdom_change_id` and `wisdom_sha`.

## API

| Method | Path | Purpose |
|---|---|---|
| POST | `/api/ingest/github` | Change events (Bearer agent token) |
| GET/POST | `/api/agents` | List / create agents |
| GET/PUT/DELETE | `/api/agents/:id` | Read / update / delete |
| POST | `/api/agents/:id/token` | Rotate ingest token |
| GET | `/api/agents/:id/workflow?repo=owner/name` | Generated workflow YAML |
| POST | `/api/agents/:id/poll` | Poll repos now |
| POST | `/api/agents/:id/wisdom` | Regenerate wisdom |
| POST | `/api/agents/:id/ask` | `{ "question": "…" }` |
| POST | `/api/agents/:id/incidents` | Log an incident manually |
| POST | `/api/changes/:id/reprocess` | Re-run triage + actions |
| POST | `/api/incidents/:id/analyze` | Re-run incident analysis |
| GET/POST | `/api/evals/labels` | Read / save a "Was this right?" label (`kind`, `targetId`, `expected`) |
| GET | `/api/health` | DB, worker heartbeat, integrations |

## Development

```bash
npm run typecheck
npm test            # unit tests: globs, Jenkins URLs, pipeline selection, schemas, workflow YAML, Splunk, prompts
npm run worker:once # one pass of every worker job, then exit
npm run eval -- --stored   # AI accuracy against the team's labels (see docs/10-evals.md)
```
