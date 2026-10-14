# Test pipelines

An agent can trigger any number of pipelines. Each one is **Jenkins**, **TeamCity** or **GitHub Actions**, and runs either:

- **on every relevant change** (`runWhen: always`), or
- **only when Claude recommends it** (`runWhen: recommended`), based on what the change touches.

You can also limit a pipeline to changes in specific repos (`onlyRepos`).

## What every build receives

| Name | Value |
|---|---|
| `WISDOM_CHANGE_ID` | Change id in Wisdom Ops |
| `WISDOM_AGENT` | Agent name |
| `WISDOM_REPO` | `owner/name` that was pushed |
| `WISDOM_BRANCH` | Branch that was pushed |
| `WISDOM_SHA` | Head commit sha, so the build can check out exactly this commit |
| `WISDOM_RISK` | `low`, `medium` or `high` from triage |

…plus the static parameters you configured. Parameter names are letters, digits, `_` and `-` only; they can't contain dots.

## Jenkins

| Setting | Example |
|---|---|
| Job path | `discretion/poa-regression` (folders become `/job/x/job/y`) or a full job URL |
| Server | `JENKINS_URL`, `JENKINS_USER`, `JENKINS_API_TOKEN` |

- **Triggering:** the build is triggered with `buildWithParameters`, so the job must accept the parameters above (or have none). A CSRF crumb is fetched automatically on older Jenkins versions.
- **Tracking:** Wisdom Ops follows the queue item to the build, then the result: `SUCCESS` → success, `UNSTABLE` → unstable, `ABORTED` → aborted, anything else → failure.
- **Test counts:** these come from the JUnit `testReport`, if the job publishes one.

## TeamCity

| Setting | Example |
|---|---|
| Build configuration id | `Discretion_PoaRegression` (from the build configuration's URL or settings) |
| Branch | *Build configuration default*, *Same branch as the push*, or a fixed branch |
| Send parameters as | `env.` (environment variables, default), `system.`, or configuration parameters |
| Server | `TEAMCITY_URL`, `TEAMCITY_TOKEN` (access token for a user that can run and view the builds) |

![TeamCity pipeline in the agent form](images/12-teamcity-pipeline-form.png)

- **Triggering:** builds are queued with `POST /app/rest/buildQueue`. Change context always goes as `env.WISDOM_*`. Your own parameters get the prefix you chose, so `SUITE=poa` becomes `env.SUITE`.
- **Tracking:** `GET /app/rest/builds/id:N` gives the state (queued → running → finished), the status (`SUCCESS` → success; cancelled → aborted; otherwise failure) and test counts. Muted and ignored tests count as skipped.
- **Logs:** the Assistant reads the end of the build log through `downloadBuildLog.html`.
- **Not yet verified on a real server:**
  - whether your server accepts token-authenticated POSTs without a CSRF header;
  - whether your builds publish test results that the REST API reports.

  Run one trigger against a real build configuration to confirm both.

## GitHub Actions

| Setting | Example |
|---|---|
| Workflow repo | `acme/test-harness` |
| Workflow file | `regression.yml` |
| Ref | `main` |

- **Triggering:** runs are started with `workflow_dispatch`. GitHub only accepts declared inputs, so declare `wisdom_change_id` and `wisdom_sha` (plus your static params) in the workflow's `on.workflow_dispatch.inputs`.
- **Tracking:** the run is found by created time and followed to its conclusion.
- **Token:** the server's `GITHUB_TOKEN` needs `actions:write` on the workflow repo.

## Alerts

Each agent has **Pipeline result notifications**: *On failure only* (default), *Always* or *Never*. Failure alerts name the change and author, and link to the build.
