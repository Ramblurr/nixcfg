# Issue tracker: Local Org mode

Issues and specs (you may know a spec as a PRD) for this repo live as Org mode files in `.scratch-org/`. This directory is local working state: never stage, commit, or otherwise add it to Git.

Use Skill(local-org-issues-cli) to inspect tracker metadata, readiness, and dependency health.

## Conventions

- One work item per directory: `.scratch-org/NNN-<slug>/`
- `NNN` comes from one shared repo-wide three-digit sequence. Features, concepts, efforts, and every other top-level work-item kind use the same sequence.
- A work-item directory has no status of its own. Its number and slug identify the concept.
- The spec is `.scratch-org/NNN-<slug>/spec.org`.
- Implementation tickets are one file each at `.scratch-org/NNN-<slug>/issues/<NN>-<slug>.org`, numbered from `01` within the work item.
- The canonical ticket ID combines both numbers: `NNN-NN`. Use it whenever referring to a ticket outside its own file.
- The first top-level Org heading carries the ticket state as a TODO keyword: `NEEDS-TRIAGE`, `NEEDS-INFO`, `READY-FOR-AGENT`, `READY-FOR-HUMAN`, `IN-PROGRESS`, `CLAIMED`, `RESOLVED`, or `WONTFIX`.
- Store the canonical ID in a `TICKET_ID` property. Store canonical blocker IDs in `BLOCKED_BY`, and the claimant in `ASSIGNEE`.
- Use the heading tags `bug` or `enhancement` for the triage category.
- Append comments and conversation history under a `** Comments` heading.
- Do not mention ticket issue numbers in commit messages

A local ticket starts with this shape:

```org
* READY-FOR-AGENT Ticket title :enhancement:
:PROPERTIES:
:TICKET_ID: 006-02
:BLOCKED_BY: 006-01
:ASSIGNEE:
:END:

** What to build
The end-to-end behavior this ticket delivers.

** Acceptance criteria
- [ ] A testable criterion

** Comments
```

## When a skill says "publish to the issue tracker"

Create an Org file under `.scratch-org/NNN-<slug>/`, creating the directory when needed. Choose the next available `NNN` from the shared sequence across all top-level numbered `.scratch-org/` work-item directories.

## When a skill says "fetch the relevant ticket"

Resolve a canonical `NNN-NN` ID to `.scratch-org/NNN-<work-item>/issues/NN-<ticket>.org`, then read that file. A user may also pass the path directly.

## Wayfinding operations

Used by Skill(wayfinder). The **map** is one Org file with one **child** Org file per ticket.

- **Map**: `.scratch-org/NNN-<effort>/map.org` — the Destination / Notes / Decisions-so-far / Fog body.
- **Child ticket**: `.scratch-org/NNN-<effort>/issues/NN-<slug>.org`. Its first heading carries the TODO state; a `TYPE` property records `research`, `prototype`, `grilling`, or `task`.
- **Research report**: `.scratch-org/NNN-<effort>/research/NN-<slug>.org`, linked from its child ticket. Parallel research tickets always receive distinct report paths.
- **Blocking**: `BLOCKED_BY` contains canonical ticket IDs separated by spaces. A ticket is unblocked when every listed ticket is `RESOLVED`.
- **Frontier**: scan the effort's `issues/` directory for tickets whose blockers are resolved, whose state is open, and whose `ASSIGNEE` is empty; lowest ticket number wins.
- **Claim**: change the TODO state to `CLAIMED`, set `ASSIGNEE`, and save before any work.
- **Resolve**: append the answer under `** Answer`, check completed acceptance criteria, change the TODO state to `RESOLVED`, then append an Org file link plus one-line gist to the map's `** Decisions so far`.
