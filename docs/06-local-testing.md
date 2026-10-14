# Local testing with mocks

You can run the whole product on a laptop with **no credentials**. `npm run mock` starts fake versions of Claude, Jenkins, TeamCity, GitHub Enterprise, ServiceNow, Splunk and a Teams webhook on port 4000. `npm run demo:seed` then creates a demo agent and two pushes through the real API.

**Needs:** Node 20+ and a MongoDB on `localhost:27017`. Any of these will do:

| Option | Command |
|---|---|
| Homebrew | `brew tap mongodb/brew && brew install mongodb-community && brew services start mongodb-community` |
| MongoDB Atlas free tier | put its `mongodb+srv://…` URI in `MONGODB_URI` (demo data only) |

## Run it

```bash
cp .env.mock .env.local          # points every integration at the mock
# make sure MongoDB is running (see above)

# four terminals:
npm run mock                     # 1. fake services (watch this log: Jenkins/TeamCity triggers, Teams cards, work notes)
npm run dev                      # 2. app on http://localhost:3000
npm run worker                   # 3. background worker
npm run demo:seed                # 4. creates a demo agent + two pushes (rerun any time)
```

Within about 10–20 seconds you should see:
- Push 1 triaged as **high risk**. It triggers the fake Jenkins job, which "fails" 2 of 43 tests, and the fake TeamCity build *CIP smoke*, which passes 118 tests with 2 ignored. Teams cards appear in the mock log.
- Push 2 marked **path-filtered**, because it only touches docs.
- Incident **INC0099** pulled from the fake ServiceNow, searched in fake Splunk, linked to push 1, with a work note posted.

## Try the Assistant

Open `/chat`, pick **Discretion** in Scope, and ask the questions the mock Claude has scripted answers for:
1. `Why did the last regression run fail?`
2. `Re-run poa regression for that change`, then click **Confirm**. A new Jenkins trigger appears in the mock log.
3. `Show the client details on INC0099`. The answer shows SSN, account and email masked.
4. `Set up a new agent for a module`. The Assistant asks for details, so reply in one message, for example: *Module: Card Limits. Covers card issuance and daily limits. Repo acme/card-service. Jenkins job discretion/card-regression, TeamCity Discretion_CardSmoke. Notify cards-team@acme.com*. It searches the repo, shows a **Create agent** card, and after you confirm it shows the ingest token once. Then say `open the setup PRs` and confirm. The mock log shows the workflow file and the PR being created.
   To see the repo check refuse a wrong name, use a repo with `missing` in it (e.g. `acme/card-missing`): no card appears, and the Assistant says the repo doesn't exist.
5. `Answer without looking: what caused the last outage?` The mock Claude cites a change it never looked up. The link is removed from the answer, with a note saying so, and a `citation_check` entry is written to the audit.
6. When the seeded build fails, the Teams/email failure alert lists the change and its **Author** (the mock log prints the alert's fields).

Any other question gets a canned "couldn't find anything" reply.


## Trying the evals

1. Open the demo change and click **Was this triage right? → No** (or Yes). Open incident INC0099 and record the cause in **Feedback**.
2. Run `npm run eval -- --stored` for the stored answers, or `npm run eval` to re-run through the mock Claude.
3. To see the gate fail, record a different cause on the incident, then run `npm run eval -- --stored --kind incident --min-hit3 0.5`. It exits with code 1.

With the mock, scores are meaningless (canned answers); this only shows the mechanics. See [Evals](10-evals.md).

## Checking masking

The demo push deliberately contains a fake SSN, card number, account number and client email. When the worker triages it, it logs:

```
[redact] triage acme/discretion-poa@…: masked SSN×1 CARD×1 ACCOUNT×1 EMAIL×1
```

The mock Claude prints `!!! UNMASKED … RECEIVED BY CLAUDE !!!` if raw client data ever reaches it. Watch for that line. In chat, type something like *"Client 123-45-6789 card 4111 1111 1111 1111 hit an error"*: your message is replaced with the masked version, with a note saying what was hidden.

## What the mock covers

| Fake | Behaviour |
|---|---|
| Claude | Scripted triage (high risk), incident terms and analysis, wisdom, and streamed chat for the scripted questions |
| Jenkins | Every build fails 2 of 43 tests, so you can see failure alerts |
| TeamCity | Every build succeeds: 120 tests, 2 ignored |
| GitHub | Repo search, branches, compare, file reads, and creating the setup PR. Any repo with `missing` in its name returns 404, so you can see setup refuse a wrong repo |
| ServiceNow | Always returns incident INC0099 and accepts work notes |
| Splunk | Returns one matching `PoaExpiredException` log line |
| Teams | Logs each card's title |
| Email | Not faked: email shows as failed unless `SMTP_HOST` is set |

To get real answers over the demo data, put a real `ANTHROPIC_API_KEY` in `.env.local` and delete `ANTHROPIC_BASE_URL`. Everything else stays fake.

## Automated tests

```bash
npm run typecheck
npm test     # globs, Jenkins URLs, TeamCity params, pipeline selection, schemas, workflow YAML, Splunk, redaction, tools
```
