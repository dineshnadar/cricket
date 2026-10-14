# Configuration and deployment

## Environment variables

Copy `.env.example` to `.env` (or `.env.local`). The web app and the worker both read it.

| Variable | Required | Purpose |
|---|---|---|
| `APP_BASE_URL` | yes | Public URL of the app. Used in links in emails, Teams cards and the generated workflow. Set the real internal address **before** generating workflow files; a `localhost` value gets written into them and every run fails (see [The GitHub workflow](08-github-workflow.md#the-most-common-mistake-localhost-in-the-file)) |
| `APP_BASIC_AUTH` | recommended | `user:password` basic auth in front of the UI and management API. `/api/ingest/*` (agent tokens) and `/api/health` stay open |
| `MONGODB_URI`, `MONGODB_DB` | yes | Database |
| `ANTHROPIC_API_KEY` | yes | Claude access |
| `ANTHROPIC_BASE_URL` | if applicable | Corporate gateway or approved deployment in front of Claude |
| `ANTHROPIC_MODEL` | no | Default `claude-sonnet-5-5` |
| `GITHUB_API_URL` | yes | e.g. `https://github.yourco.com/api/v3` |
| `GITHUB_TOKEN` | yes | Repo read access (picker, poller, wisdom, file reads). Add `actions:write` for GitHub Actions pipelines, and `contents:write` + `pull_requests:write` if the Assistant should open setup PRs |
| `GITHUB_ORGS` | no | Orgs the repo search is limited to, comma-separated |
| `JENKINS_URL`, `JENKINS_USER`, `JENKINS_API_TOKEN` | for Jenkins | Trigger and track builds |
| `TEAMCITY_URL`, `TEAMCITY_TOKEN` | for TeamCity | Access token of a user who can run and view the build configurations |
| `SMTP_HOST`, `SMTP_PORT`, `SMTP_SECURE`, `SMTP_USER`, `SMTP_PASS`, `SMTP_FROM` | for email | Corporate SMTP relay |
| `SERVICENOW_URL`, `SERVICENOW_USER`, `SERVICENOW_PASSWORD` | for incidents | Table API read; write too if work notes are on |
| `SPLUNK_URL`, `SPLUNK_TOKEN` (or `SPLUNK_USER` + `SPLUNK_PASSWORD`) | for Splunk | Management port, e.g. `:8089` |
| `WORKER_TICK_SECONDS` | no | Change queue interval (default 15) |
| `PIPELINE_REFRESH_SECONDS` | no | Build status polling (default 30) |
| `SERVICENOW_POLL_MINUTES`, `SERVICENOW_LOOKBACK_MINUTES` | no | Incident polling (default 5) and first-run lookback (default 120) |
| `WISDOM_REFRESH_HOURS` | no | Wisdom regeneration (default 24) |
| `REDACT_ALLOW_EMAIL_DOMAINS` | recommended | Internal email domains left readable (e.g. `yourco.com`). All other emails are masked before Claude |
| `REDACT_EXTRA_PATTERNS` | recommended | JSON list of your own identifier formats to mask, e.g. `[{"name":"CLIENT_ID","pattern":"\\bCL\\d{8}\\b"}]`. An invalid pattern stops the app rather than silently skipping masking |
| `DRY_RUN` | no | `true` logs notifications and pipeline triggers instead of sending them |

For internal certificate authorities, set `NODE_EXTRA_CA_CERTS=/path/to/ca.pem`.

## Running it

| Where | How |
|---|---|
| Laptop (demo) | See [Local testing](06-local-testing.md) |
| Single VM | `npm ci && npm run build`, then run `npm run start` and `npm run worker` under a process manager (systemd, pm2) |
| Kubernetes (later) | Needs a container image built from your approved base image (not included yet). Two Deployments: web (`npm run start`, port 3000, behind your ingress) and worker (`npm run worker`, 1 replica). Use your managed MongoDB |

**Run exactly one worker.** Change processing is claimed atomically, so a second worker wouldn't double-process. But the pollers and the wisdom schedule assume a single instance.

`GET /api/health` reports database status, the worker heartbeat, and which integrations are configured.

## Network access the service needs

| From | To | Why |
|---|---|---|
| GitHub Actions runners | App URL (`/api/ingest/github`) | Push events. Use a self-hosted runner, or set `WISDOM_URL`, if hosted runners can't reach it |
| App | GitHub Enterprise API | Repo search, compare, files, PRs, workflow dispatch |
| App | Claude endpoint / gateway | All LLM calls |
| App | Jenkins, TeamCity | Trigger and poll builds |
| App | SMTP relay, Teams webhook | Notifications |
| App | ServiceNow, Splunk `:8089` | Incidents and log evidence |
| Users | App URL | Browser |

## Security checklist before production

- [ ] Claude endpoint approved for sending source code, diffs, log excerpts and ticket text (redacted)
- [ ] Masking rules (`src/lib/redact.ts`) reviewed against real Splunk, ServiceNow and code samples; internal ID formats added to `REDACT_EXTRA_PATTERNS`; `REDACT_ALLOW_EMAIL_DOMAINS` set
- [ ] App behind SSO or `APP_BASIC_AUTH` plus network restrictions. The chat's display name is **not** authentication
- [ ] Least-privilege tokens for GitHub, Jenkins, TeamCity, ServiceNow and Splunk, stored in your secret manager
- [ ] Ingest tokens rotated when people leave (Setup tab → Rotate)
- [ ] Retention set for `audit`, `conversations` and `incidents`
- [ ] `DRY_RUN=true` for the first days with real repos, then switched off
