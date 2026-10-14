# Architecture

![Wisdom Ops architecture](images/architecture.png)

Wisdom Ops is **one internal service** with two processes sharing one MongoDB:

| Process | Command | Does |
|---|---|---|
| **Web app** (Next.js) | `npm run start` | UI, management API, ingest API, Wisdom Assistant |
| **Worker** | `npm run worker` | Change queue, pipeline tracking, repo polling, incident sync and analysis, daily wisdom |

Developers install nothing. Each watched repo gets one small workflow file, and everyone else uses the browser.

## How changes get in

| Mode | How | When to use |
|---|---|---|
| **GitHub Action** (default) | `.github/workflows/wisdom-ops.yml` in each repo runs on push. It uses `actions/github-script` and the job's own `GITHUB_TOKEN` to diff the push, then posts it to `/api/ingest/github` with the agent's token | Normal case. Works without webhook admin rights; a push is usually triaged within a minute (runner start-up + worker tick) |
| **Poller** | The worker compares each watched branch's head with the last seen sha through the GitHub API | Repos where you can't add a workflow. Branch globs like `release/*` can't be polled |
| **Both** | Workflow plus poller as a safety net | Dedupe on (agent, repo, head sha) means a change is never processed twice |

The workflow file is generated per repo on the agent's **Setup** tab. The Assistant can also open it as a pull request.

## Processing a change

`src/lib/changes.ts` → `processChange()`:

1. **Path filter** (plain code). If no file matches the repo's watched globs, the change is marked `path-filtered` and stops there, with no LLM call.
2. **Claude triage** (`src/lib/triage.ts`). The input is the module description, the repo notes, the commit messages and the diff: watched files first, about 60 KB budget, redacted. The output is structured: `relevant`, `confidence`, `summary`, `impactedAreas`, `riskLevel` and `riskReasons`, `testFocus`, and `recommendedPipelines`.
3. **Threshold.** Below the agent's confidence threshold, the change is marked `not-relevant`. It's kept for history but triggers nothing.
4. **Pipelines** (`src/lib/pipelines.ts`). Pipelines marked *always* run, plus any Claude recommended. Each is limited to its listed repos if set.
5. **Notify** (`src/lib/notify.ts`): an HTML email and an adaptive card for Teams.
6. **Track.** The worker polls runs every 30 seconds until they finish, records test counts, and alerts on failure (or always) per the agent's setting.

Failures are retried three times with backoff. A change stuck in `processing` for 15 minutes is put back in the queue.

## Incidents

`src/lib/incidents.ts`, run by the worker. ServiceNow is polled every `SERVICENOW_POLL_MINUTES` (default 5) for agents with it enabled, and new incidents are analysed within a minute:

1. Pull incidents that match the agent's encoded query.
2. Ask Claude for exact log search terms (error codes, exception names, endpoints).
3. Search Splunk with the agent's base search plus those terms, in a window around the open time.
4. Ask Claude to correlate the incident, logs and failed runs with the last 14 days of relevant changes. Any cause that cites a change id Claude wasn't given is dropped.
5. Notify the team, and optionally post a ServiceNow work note.

Incidents can also be logged by hand on the agent's **Incidents** tab. The Assistant can look up and re-analyse incidents, but it can't log new ones.

## Masking client data before Claude

Every call to Claude goes through **one module, `src/lib/llm.ts`**. Before anything leaves the process, it masks the system prompt, every user message (including what people type in chat) and every tool result. A test (`tests/llm-gateway.test.ts`) **fails the build** if any other file calls Claude directly. So triage, incident analysis, wisdom and the Assistant are all covered, and a future feature can't forget.

| Masked | Becomes |
|---|---|
| SSN and ITIN (`123-45-6789`, `123 45 6789`, or 9 digits next to "SSN", "TIN" or "tax id") | `[REDACTED-SSN]` |
| EIN next to "EIN" | `[REDACTED-EIN]` |
| Card numbers (13–19 digits that pass the Luhn check) | `[REDACTED-CARD …1111]` |
| Account and routing numbers named as such ("acct", "account", "routing", "IBAN" …) | `[REDACTED-ACCT …6789]` |
| Any other run of 10+ digits | `[REDACTED-NUM …2101]` |
| Dates of birth next to "DOB" or "date of birth" | `[REDACTED-DOB]` |
| Phone numbers | `[REDACTED-PHONE]` |
| Emails, except allow-listed internal domains | `[REDACTED-EMAIL]` |
| Passwords, tokens, API keys, bearer tokens, JWTs, AWS keys, private keys | `[REDACTED-SECRET]` and similar |
| Your own identifiers, via `REDACT_EXTRA_PATTERNS` | `[REDACTED-<NAME>]` |

- **The last 4 digits are kept** for card and account numbers, so people can still tell them apart.
- **Each call logs what it masked**, e.g. `[redact] triage acme/poa@b01a137: masked SSN×1 CARD×1`. The Assistant's audit entries record the counts too.
- **Chat input is masked before it's stored**, so the conversation history never holds the raw value. The user sees their message replaced with the masked version and a note saying what was hidden.
- **What it can't catch:** it matches patterns, so client **names, free-text addresses, and identifiers in formats it doesn't know** get through. Add your internal ID formats to `REDACT_EXTRA_PATTERNS`, and have security review the rules against real log and ticket samples. For names and addresses, the next step would be a named-entity service (for example your firm's DLP service) called from the same gateway.
- Data in **MongoDB** (diffs, Splunk lines) is stored as it came from GitHub and Splunk. Masking applies to what's sent to the LLM, and to chat transcripts.

## Data model (MongoDB)

| Collection | Holds | Key fields |
|---|---|---|
| `agents` | One per module | scope description, repos (branches, path globs, notes), pipelines, recipients, ServiceNow/Splunk settings, hashed ingest token |
| `changes` | One per push | repo, branch, sha, commits, files and patches, matched files, triage, status, notifications |
| `pipeline_runs` | Each triggered build | type (jenkins / teamcity / github-actions), status, build URL and number, test counts |
| `incidents` | ServiceNow or manual | description, Splunk query and events (redacted), analysis, likely causes |
| `wisdom` | Daily per agent | module summary, per-repo purpose, components, hotspots, risks, testing notes |
| `poll_state` | Poller bookmarks | last seen sha per repo and branch |
| `conversations` | Assistant chats | user display name, transcript, scope |
| `assistant_actions` | Write actions | tool, validated input, status (pending / confirmed / cancelled / failed), result |
| `audit` | Everything the Assistant did | user, question, tool, action, ok/error, size of each result |
| `meta` | Worker heartbeat, schedules | |

## Where the LLM is called

| Call | File | Output |
|---|---|---|
| Triage a change | `src/lib/triage.ts` | Structured (forced tool call, validated with zod) |
| Incident search terms | `src/lib/incidents.ts` | Structured |
| Incident correlation | `src/lib/incidents.ts` | Structured |
| Repo wisdom | `src/lib/wisdom.ts` | Structured |
| Module overview | `src/lib/wisdom.ts` | Markdown |
| Assistant chat | `src/lib/assistant/loop.ts` | Streamed text with tool use |

All calls go through `src/lib/llm.ts`, using `ANTHROPIC_MODEL` and, optionally, an `ANTHROPIC_BASE_URL` gateway.

## Code map

```
src/
  app/                 pages (dashboard, agents, changes, incidents, chat) and API routes
    api/ingest/github  push events from the workflow
    api/assistant/*    chat stream, conversations, action confirm/cancel
  components/          UI (AgentForm, Chat, Sidebar, shared ui)
  lib/
    changes.ts         record + process a change
    triage.ts          Claude triage prompt and schema
    pipelines.ts       trigger + track Jenkins / TeamCity / GitHub Actions
    jenkins.ts teamcity.ts github.ts servicenow.ts splunk.ts notify.ts
    incidents.ts       incident sync + analysis
    wisdom.ts          daily summaries + Ask
    redact.ts          client-data masking
    assistant/         tools, agent loop, conversation store
  worker/index.ts      background jobs
mock/server.js         fake Claude, Jenkins, TeamCity, GitHub, ServiceNow, Splunk, Teams
scripts/demo-seed.ts   demo agent + pushes
```
