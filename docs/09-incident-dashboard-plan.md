# Incident import, classification and dashboard: plan

> **Status:** plan, not built yet. Built in phases (section 9) once the code has moved to the work environment.
> **Phase-1 reality:** no ServiceNow API or Splunk access yet. Incidents arrive as ServiceNow XLSX exports (about 40 old files, plus a new export every day) or through **Log incident**.

## 1. The problem

- ServiceNow incidents are exported to XLSX **every day** and dumped in a shared place. Nobody can see trends across them.
- About **40 old export files** hold the history: what broke, where and how it was fixed. None of it is classified consistently.
- Management, ops, dev and business each want different answers from the same data:
  - **Management:** are we getting better? What hurts most?
  - **Ops:** what's open, how old is it, who has it?
  - **Dev:** which code areas keep causing incidents, and which change caused this one?
  - **Business:** how is *my* platform (POA, Check, Card, CIP) doing?

## 2. Key decision: one incident store

Old and new incidents are the **same kind of record**: the daily export also contains today's incidents. So there is **one collection, `incidents`** (the one Wisdom Ops already uses), with three new parts:

| New field | Values | Why |
|---|---|---|
| `source` | `import` · `manual` · `servicenow` | Where the record came from |
| `classification` | type, platform, root cause, fix type, symptom/cause/fix lines, confidence, reviewed-by | Dashboards and similar-incident search |
| `importBatch` | batch id + file name | Trace a row to its file; remove a bad import in one go |

```
  40 old XLSX ─┐
  daily XLSX  ─┤  Import page ──┐
  Log incident ────────────────┼──▶  incidents  ──▶ A. correlate with code changes (exists today)
  ServiceNow API (later) ──────┘     (one store)  ──▶ B. classification · dashboard · "seen this before?"
```

What applies to which incidents:

| | Old incidents (40 files) | New incidents (daily file / Log incident) |
|---|---|---|
| Classification | Yes, one bulk run | Yes, automatic on import |
| Dashboard | Yes | Yes |
| "Similar past incidents" | They *are* the history | Matched against the history |
| Correlate with code changes | **No.** Wisdom Ops wasn't recording pushes back then | **Yes**, for incidents opened after the agent started watching, or on **Analyse** |

## 3. Import (replaces "dump the file in a folder")

**Import page** (Incidents → Import):

1. Drag in one or more `.xlsx` / `.csv` files.
2. **First time only:** map columns to fields (*Number → number*, *Short description → shortDescription*, *Assignment group → assignmentGroup* …). The mapping is saved, so every daily export after that is a single drag-and-drop.
3. **Preview** the first 20 rows as they'll be stored, with any problems flagged (missing number, unreadable date).
4. **Import:**
   - **Upsert by incident number.** The daily export repeats open incidents with new status, notes and resolution, so existing rows are updated, not duplicated.
   - **Mask on the way in** with the existing masking (`src/lib/redact.ts`), even though the exports are already masked.
   - Tag every row with `source: "import"` and the `importBatch`.
5. **Report:** *"412 rows: 37 new, 375 updated, 2 skipped (no number)."* It's kept in an import history list with an **Undo batch** button.

Later, when ServiceNow API access is approved, the existing sync (`src/lib/servicenow.ts`) replaces the daily upload. Until then, the upload *is* the integration.

### Fields to keep

| Group | Fields |
|---|---|
| Identity | number, sysId (if exported), importBatch, source |
| ServiceNow facts | opened, resolved, closed, priority, impact, urgency, state, category, subcategory, assignment group, assigned to (optional), configuration item / service |
| Text (masked) | short description, description, close notes / resolution, work notes (trimmed) |
| Derived (no AI) | time to resolve, month opened, platform (from mapping), is-open, age in days |
| Classification (AI) | see section 4 |

**Not kept:** caller/contact details, attachments, full email threads.

## 4. Classification

Three layers, cheapest first:

| Layer | Fills | How |
|---|---|---|
| **1. ServiceNow columns** | priority, state, dates, category, assignment group, CI, time to resolve | Column mapping. No AI. |
| **2. Rules** | **platform / module** (POA, Check, Card, CIP, Shared, Other) | A **mapping table** the team fills in once: assignment group or CI → platform, e.g. *Discretion-POA-Support → POA*. No AI. |
| **3. Wisdom AI** | **issue type**, **root-cause category**, **fix type**, one-line symptom, cause and fix, confidence | Reads the masked description and close notes against a **fixed category list** |

### Agreeing the categories (before any AI run)

1. Wisdom AI reads ~200 sample incidents and **proposes** categories.
2. The team trims them to a fixed list in one short review. Starting point:
   - **Issue type:** data validation · time zone/date · vendor/third-party timeout · config/feature flag · deployment/release · access/permissions · performance · data fix needed · user error/not a bug · other
   - **Root-cause layer:** code · config · data · infrastructure · external vendor · unknown
   - **Fix type:** code fix · config change · data correction · restart/rollback · vendor fix · no action
3. The list is stored with a version (`incident_taxonomy`), so you always know which version labelled which incident.

### Running it

- A background batch job classifies every unclassified incident, one Wisdom AI call each (or a few per call). Everything goes through the masking gateway (`src/lib/llm.ts`).
- **"Unknown" is allowed and expected.** Many old close notes just say "resolved", and the AI must not guess.
- **Review:** the team checks 50 random rows plus every low-confidence one. Corrections are stored (`reviewedBy`, corrected labels) and **always win** over the AI's labels. If accuracy is poor, fix the category definitions and re-run.
- New incidents from the daily import are classified automatically with the same list.

## 5. Dashboard

One data set, four tabs. All numbers come from **MongoDB aggregation on the classified fields**: instant, exact, no AI per page view.

| Tab | Audience | Contents |
|---|---|---|
| **Overview** | Management, business | KPI tiles: incidents this month and **MoM %**, P1/P2 count, median time to resolve, open backlog, repeat-incident rate · incidents by platform · top 5 recurring issue types · **AI monthly summary** (2–3 sentences) |
| **Incident register** | Ops | Searchable, filterable table (platform, priority, state, issue type, root cause, date range, assignment group) · ageing of open incidents (0–2d / 3–7d / 8–30d / 30d+) · export to Excel · click through to the incident page |
| **Root cause** | Dev | Root-cause layer × platform · issue type × platform · top recurring causes with example incidents · "fixed by code change" rate · for new incidents, the **linked code change**, repo and author |
| **Trends** | Everyone | Month over month by platform, priority and issue type · MTTR trend · repeat-incident trend · last 12 months by default |

- **Business view:** a **platform filter** at the top of every tab (POA / Check / Card / CIP / All), so each business owner sees their own area. It isn't a separate page.
- **AI monthly summary:** generated **once a day** from the aggregated numbers (not raw tickets), cached, and labelled as AI-written. For example: *"CIP vendor timeouts doubled in September (4 → 9), mostly after release 4.2; POA date issues are down 60% since the UTC fix."*
- **Design:** the app's existing design system (dark/light themes, Fira fonts, status colours with icons). Charts follow the dataviz rules: accessible colours, legends, tooltips, never colour alone.

### KPI definitions (agree these with management)

| KPI | Definition |
|---|---|
| Incidents (month) | Count by `openedAt` month |
| MoM % | (this month − last month) / last month |
| MTTR | Median of `resolvedAt − openedAt`, resolved incidents only |
| Open backlog | `state` not resolved/closed |
| Repeat-incident rate | % of incidents whose (platform, issue type) also occurred in the previous 30 days |
| P1/P2 | Priority 1 or 2 |

## 6. Do we need embeddings?

| Feature | Needs embeddings? |
|---|---|
| Import, classification | No |
| Dashboard, KPIs, trends, MoM | No: structured fields and counts |
| Monthly AI summary | No: summarises aggregated numbers |
| **"Similar past incidents"** | **Optional.** Start without; add later if matches are missed |

**Similar incidents without embeddings:**
1. Filter by platform and issue type.
2. Keyword search on symptom and error codes (MongoDB text index).
3. Wisdom AI picks the truly similar ones from about 20 candidates.

Classification makes this work well, because "same platform + same issue type" already narrows things down a lot.

**Add embeddings only if** people report real matches being missed. MongoDB Atlas has vector search built in; self-hosted MongoDB would need a separate vector store, which is extra infrastructure to approve.

## 7. How it connects to the agent

| Where | What it adds |
|---|---|
| Agent ↔ incident | The platform mapping links each incident to the agent (Discretion) and sub-area, so the agent's **Incidents** tab shows imported incidents too |
| Incident analysis (exists) | Adds *"Similar to INC0045123 (Mar 2025, CIP vendor timeout), fixed by raising the retry limit"* to the change correlation |
| Push triage (exists) | When a push touches an area with incident history: *"POA date handling: 7 incidents in 12 months"*, and the risk goes up |
| Module wisdom (exists) | A **Known failure patterns** section per repo and area |
| Assistant (exists) | New read tools: `search_incidents_history`, `incident_trends`, `incident_kpis` → *"How often has CIP verification failed because of the vendor timeout?"* |

## 8. Data model additions

| Collection | Change |
|---|---|
| `incidents` | add `source`, `importBatch`, `classification{issueType, rootCause, fixType, symptom, cause, fix, confidence, taxonomyVersion, reviewedBy, reviewedAt}`, `platform`, `assignmentGroup`, `category`, `ci`, `resolvedAt`, `closedAt`, `state` |
| `incident_imports` (new) | one row per import: file name, user, time, counts, column mapping used, status (for **Undo batch**) |
| `incident_taxonomy` (new) | versioned category list with definitions and examples |
| `platform_mapping` (new) | assignment group / CI → platform → agent |
| `incident_stats` (new, optional) | nightly pre-aggregated monthly counts, if live aggregation becomes slow |

**Indexes** (added to `ensureIndexes` in `src/lib/db.ts`; the app account can create indexes):
- `incidents`: `{number: 1}` unique (upsert key); `{platform: 1, openedAt: -1}`; `{"classification.issueType": 1, openedAt: -1}`; a text index on short description, description and symptom.
- `incident_imports`: `{at: -1}`.

## 9. Phases

| Phase | Scope | Effort |
|---|---|---|
| **1. Import** | Import page (XLSX/CSV), column mapping, preview, upsert, masking, import history + undo, platform mapping table · load the 40 files | ~2–3 days |
| **2. Classify** | Propose categories → team review → batch classification → review screen for corrections | ~2 days + team time |
| **3. Dashboard** | Overview, Incident register, Root cause, Trends · platform filter · AI monthly summary · Excel export | ~3–4 days |
| **4. Connect** | Similar incidents in analysis, risk hints in triage, failure patterns in wisdom, Assistant tools | ~2 days |
| Later | ServiceNow API sync replaces upload · embeddings only if needed · scheduled email of the monthly summary to management | n/a |

## 10. Open questions (needed before phase 1)

1. **One sample export with client data removed**, or its column headers plus 3–4 rows. This decides the column mapping and how much classification the rules can do.
2. **Assignment group / CI → platform list.** Who owns it?
3. **Category list sign-off.** Who reviews the proposed categories?
4. **KPI definitions.** Does management agree with section 5's definitions (especially MTTR and repeat rate)?
5. **Daily export.** Same columns every day? Who uploads it, or can it land somewhere the app can read automatically?
6. **Access.** Should everyone see every platform, or should the business view be limited per platform? This matters once SSO exists.
