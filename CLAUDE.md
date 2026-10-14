# CLAUDE.md

All project context for AI coding tools lives in **[AGENTS.md](AGENTS.md)**. Read it before making changes.

Most important rules (details in AGENTS.md §6):
- Every LLM call goes through `src/lib/llm.ts` (it masks client data); a test fails the build otherwise.
- On screen, the AI is "Wisdom AI", never "Claude".
- Write actions only run after a person clicks Confirm.
- Changing a triage/incident prompt? Bump its prompt version and compare `npm run eval` before and after (docs/10-evals.md).
- Keep everything runnable with `npm run mock`; update the matching doc in `docs/` with every feature change.
