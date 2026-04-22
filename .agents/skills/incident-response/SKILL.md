---
name: incident-response
description: Document home-ops incidents and operational issues in `~/nixcfg-private/incidents/YYYY/MM-DD title.md` using a grepable Markdown format that cleanly separates facts, root cause, actions taken, and future improvements. Use when Codex needs to create, update, or review incident records for outages, auth failures, deploy regressions, service misconfigurations, or postmortems in the `nixcfg` / `nixcfg-private` environment.
---

# Incident Response

Keep incident records in the private repo only.

- Write incident files under `~/nixcfg-private/incidents/YYYY/MM-DD <title>.md`.
- Never put incident records in `~/nixcfg` or other public paths unless the human explicitly asks.
- Use plain Markdown. Do not use YAML frontmatter in incident documents.
- Use exact timestamps and concrete hostnames, service names, and file paths when known.
- Keep facts, diagnosis, actions taken, and future improvements in separate sections.

## Workflow

1. Inspect existing incident records first.
   - Use `find ~/nixcfg-private/incidents ...` and `rg` to avoid duplicate files for the same issue.
   - If the incident is ongoing or already logged, update the existing file instead of creating a new one.
2. Choose the incident path.
   - Use the incident start date when known.
   - Use a short ASCII symptom-first title such as `04-20 ocis auth fails after authentik change.md`.
3. Read and follow [references/incident-template.md](references/incident-template.md).
4. Fill each section with the correct kind of information.
   - `Summary`: short problem statement plus final root cause when known.
   - `Impact`: who or what was affected, and for how long.
   - `What Happened`: facts only, timeline and observations only.
   - `Why It Happened`: root cause, trigger, contributing factors, and what was ruled out.
   - `What We Did`: only actions actually taken, not proposed work.
   - `What We Should Do Better`: follow-up work, prevention, monitoring, runbooks, and process gaps.
   - `References`: commands, files, logs, related hosts, modules, or prior incidents.
5. Before finishing, verify the record is grepable.
   - Prefer short labels like `Date:`, `Status:`, `Severity:`, `Systems:`, and `Tags:`.
   - Prefer bullets over long prose in timeline and action sections.
   - State clearly whether the issue is `open`, `mitigated`, or `resolved`.

## Editing Rules

- Preserve the fixed section order from the template unless the human asks otherwise.
- Do not mix proposed fixes into `What We Did`.
- Do not mix speculation into `What Happened`.
- If root cause is still unknown, say so explicitly in `Why It Happened`.
- If recovery was partial, mark `Status: mitigated` instead of `resolved`.
- When adding commands or file references, keep them concrete enough that future `rg` searches can find them.
