# The GitHub workflow

Each watched repo gets one small GitHub Actions workflow, `.github/workflows/wisdom-ops.yml`. When code lands on a watched branch, it sends the change to Wisdom Ops. This page explains what it needs, when it runs, what each part of the file does, and how to fix it when it fails.

You never write the file by hand. The agent's **Setup** tab generates it for each repo, and the Assistant can open the pull request that adds it.

## 1. What it needs

| Value | What it is | Where it comes from | What you do |
|---|---|---|---|
| `WISDOM_TOKEN` | The agent's ingest token. It proves the push came from a repo you set up. | Shown **once** when the agent is created. If it's lost, use **Rotate ingest token** on the agent page. | Add it as a GitHub **secret** named `WISDOM_TOKEN`, at repo level or once at org level. **This is the only manual step.** |
| `WISDOM_AGENT_ID` | Which agent the push belongs to (the agent's id) | Filled into the file when it's generated | Nothing |
| `WISDOM_URL` | Where to send the change | Filled in from `APP_BASE_URL` when the file is generated | Nothing, unless you need to override it with a GitHub **variable** `WISDOM_URL` |
| `WISDOM_RUNNER` (optional) | Runner label to use | Your org's runner setup | Set a GitHub **variable** if jobs must run on a self-hosted runner. Inside a bank network this is usually needed, because GitHub-hosted runners can't reach internal addresses. |

The job also uses GitHub's built-in, read-only job token to read the diff, so no personal GitHub token goes into the repo.

### How the token is protected

- Wisdom Ops never stores the token itself, only a SHA-256 hash of it and its last 4 characters, which the UI shows as a hint. Someone who can read the database still can't use it.
- On each request, Wisdom Ops hashes the incoming token and compares it with the stored hash in constant time.
- **Rotating** issues a new token and the old one stops working at once, so every repo's `WISDOM_TOKEN` secret must be updated. The button warns about this. An org-level secret makes rotation a single update.
- The token belongs to the **agent**, not the repo, so every repo under one agent uses the same token.

## 2. When it runs

It runs on a **push to a watched branch**. Merging a PR is a push to the target branch, so all three merge styles trigger it:

| Event | Runs? |
|---|---|
| PR merged into `main` (merge commit, squash or rebase) | Yes, as one change with the full diff |
| Direct push to `main` (where allowed) | Yes |
| Push to a feature branch not in **Branches** | No |
| Opening or updating a PR | No. Wisdom Ops sees code only once it has landed. |
| Push that only changes files outside **Watched paths** | No. The workflow doesn't even start, so no runner time is used. |
| Branch deleted | Starts, then exits straight away with "nothing to send" |
| **Run workflow** button in the Actions tab | Yes, for testing |

### Gotchas

1. **The workflow file must exist on the branch being pushed to.** GitHub runs the copy of the workflow on that branch. Once the setup PR is merged into `main`, `main` is covered. A `release/*` branch cut *before* that has no copy, so pushes there are silently missed until the file is added to it. Use **Both** detection mode (Action plus poller) for repos with release branches; the poller catches what the Action misses.
2. **Merges made by bots that use GitHub's built-in token don't trigger workflows.** This is a GitHub rule to prevent loops, and it affects some auto-merge bots and merge queues. The poller catches these.
3. **The runner must be able to reach `WISDOM_URL`.** If it can't, the run fails (red X), but the merge is never blocked. The poller still picks the change up if **Both** is on.

## 3. The file, part by part

```yaml
name: Wisdom Ops change notify
on:
  push:
    branches: ['main', 'release/*']       # from the repo's Branches setting
    paths: ['src/**/poa/**', '!**/*.md']  # from Watched paths; other files never start a run
  workflow_dispatch: {}                   # adds a "Run workflow" button for testing
permissions:
  contents: read                          # read-only; enough to read the diff
concurrency:
  group: wisdom-ops-${{ github.ref }}
  cancel-in-progress: false               # pushes close together queue up; none are dropped
jobs:
  notify:
    runs-on: ${{ vars.WISDOM_RUNNER || 'ubuntu-latest' }}
    timeout-minutes: 5                    # give up instead of hanging if the app is unreachable
    steps:
      - uses: actions/github-script@v7    # GitHub's official "run a small script" action; nothing to install
        env:
          WISDOM_URL: ${{ vars.WISDOM_URL || 'https://wisdom-ops.internal.example.com' }}
          WISDOM_TOKEN: ${{ secrets.WISDOM_TOKEN }}
          WISDOM_AGENT_ID: '<filled in>'
        with:
          script: |
            # (the script, explained below)
```

What the script does:

1. **Skips branch deletions.**
2. **Works out the "before" commit.** Normally it's where the branch was before this push. For a brand-new branch, GitHub sends all zeros, so the script compares against the default branch instead.
3. **Reads the diff** between before and after, the same as a PR's *Files changed* tab. For each file it keeps the name, status, lines added and removed, and the patch. It's capped at 1,000 files and 20,000 characters of patch per file, so a huge push can't send a giant payload. If the diff can't be read, it logs a warning and continues without file details.
4. **Collects the commits** (id, message, author, time, link), up to 250. The author shown in failure alerts comes from here.
5. **Sends everything** to `<WISDOM_URL>/api/ingest/github`, with `Authorization: Bearer <WISDOM_TOKEN>`.
6. **Reports the result** in the Actions log. A rejection turns the run red.

What Wisdom Ops receives (shortened):

```json
{
  "agentId": "<agent id>", "repo": "acme/discretion-poa", "branch": "main",
  "before": "9f2c…", "after": "b01a137…", "pusher": "dev1",
  "commits": [{ "sha": "b01a137…", "message": "Move isExpired to UTC", "author": "dev1" }],
  "files": [{ "filename": "src/main/java/com/acme/poa/PoaValidator.java", "status": "modified",
              "additions": 6, "deletions": 2, "patch": "@@ -41,7 +41,9 @@ …" }]
}
```

Wisdom Ops then checks, in this order:
1. the token matches the agent;
2. the agent is enabled;
3. the repo is watched by this agent;
4. the branch is watched;
5. the push isn't a duplicate.

Only then is the change stored and queued for triage.

## 4. Reading the Actions log

| Log message | Meaning | Fix |
|---|---|---|
| `Wisdom Ops: {"accepted":true,"changeId":"…","watchedFiles":2}` | Received and queued for triage | Nothing |
| `{"accepted":false,"reason":"duplicate"}` | This push was already received (e.g. by the poller) | Nothing |
| `{"accepted":false,"reason":"branch … is not watched"}` | The branch isn't in this agent's **Branches** | Add the branch to the agent, or ignore it |
| `{"accepted":false,"reason":"… is not watched by this agent"}` | The repo isn't on this agent | Add the repo, or check the agent id in the file |
| `{"accepted":false,"reason":"agent disabled"}` | The agent is switched off | Enable it on the agent page |
| `Wisdom Ops ingest failed: 401 Invalid agent or token` | Wrong or old token, or the file is from a different agent | Update the `WISDOM_TOKEN` secret (after a rotation) or regenerate the file from the right agent |
| `fetch failed` / `ECONNREFUSED` / timeout | The runner can't reach `WISDOM_URL` | Check the URL (see below) and that the runner is inside the network (`WISDOM_RUNNER`) |
| `compare failed: …` (warning) | The diff couldn't be read | The change still arrives without file details; check the job has `contents: read` |

### The most common mistake: `localhost` in the file

If the file was generated while `APP_BASE_URL` was `http://localhost:3000` (the local demo setting), it contains:

```yaml
WISDOM_URL: ${{ vars.WISDOM_URL || 'http://localhost:3000' }}
```

On a GitHub runner, `localhost` means the runner itself, so every run fails to connect. Fix it either way:
- set `APP_BASE_URL` to the real internal address, restart Wisdom Ops, and copy the file again from the **Setup** tab; or
- without regenerating, add a GitHub **variable** `WISDOM_URL` with the real address. It takes priority over the value in the file.

Also check that the agent id in the file is your **real** agent's, not the demo agent's.

## 5. Other ways to detect changes

| Mode | When to use it |
|---|---|
| **GitHub Action** (this page) | Default. Needs no admin rights; any developer can merge the setup PR. |
| **Central poller** | Nothing can be installed in the repos, or no runner can reach the app. The worker checks each watched branch every few minutes using `GITHUB_TOKEN`. |
| **Both** | Recommended for repos with `release/*` branches or bot merges. The poller fills the gaps. |
| **GitHub webhook** *(not built yet)* | Better if you can get an org admin. One org-level webhook covers every repo with no files in the repos, but GitHub Enterprise must be able to reach the app. Keep the poller alongside it, because GitHub doesn't retry failed deliveries automatically. |
