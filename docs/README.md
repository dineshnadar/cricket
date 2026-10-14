# Wisdom Ops documentation

Wisdom Ops watches the GitHub Enterprise repos behind a business module, such as the **discretion module** (POA, check, card and CIP verification). Every push is triaged with Claude, the right test pipelines run, and the team is told what changed and why it matters. When production breaks, the app links the incident to the change that most likely caused it. The **Wisdom Assistant** chat lets anyone ask about all of this, or set up new modules, without installing anything.

| Page | For | What's in it |
|---|---|---|
| [Overview](01-overview.md) | Everyone | The problem, what the app does, the phases, what it doesn't do |
| [Architecture](02-architecture.md) | Engineers, architects | Components, data flow, data model, where the LLM is called |
| [User guide](03-user-guide.md) | Module leads, developers, on-call | Setting up an agent, what happens on a push, alerts, incidents (with screenshots) |
| [Wisdom Assistant](04-wisdom-assistant.md) | Everyone, security reviewers | Why Claude is used, where the knowledge comes from, how answers are built, guardrails |
| [Test pipelines](05-test-pipelines.md) | Leads, CI owners | Jenkins, TeamCity and GitHub Actions setup and what gets passed to builds |
| [Local testing with mocks](06-local-testing.md) | Anyone trying it out | Running the full flow on a laptop with no credentials |
| [Configuration and deployment](07-configuration-and-deployment.md) | Platform, security | Environment variables, hosting, network access, security checklist |
| [The GitHub workflow](08-github-workflow.md) | Developers, GitHub admins | What the per-repo workflow needs, when it runs, every line explained, troubleshooting the Actions log |
| [Evals](10-evals.md) | Module leads, anyone changing prompts | How the team labels Wisdom AI's decisions and how `npm run eval` measures accuracy (recall, precision, incident hit rate) |
| [Incident dashboard plan](09-incident-dashboard-plan.md) | Management, ops, dev leads | *Plan, not built yet:* XLSX import of ServiceNow exports, classification, and the management / ops / dev / business dashboard |
| [Design system](design-system.md) | Frontend | Colours, type, spacing and UI rules the app follows |
| [Confluence page](Wisdom-Ops-Confluence.md) | Sharing | One page with diagrams and screenshots, ready to import into Confluence |

Context for AI coding tools (Codex, Claude Code…) is in [`../AGENTS.md`](../AGENTS.md).

Diagrams and screenshots are in [`images/`](images/). Each diagram comes as an editable `.svg` plus a `.png` for Confluence.

> The screenshots use the demo data from `npm run demo:seed` with the mock services, so repo names like `acme/discretion-poa` are placeholders.
