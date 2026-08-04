# Triage States

The skills use five canonical triage roles. The local Org tracker records each role as the TODO keyword on a ticket's first heading.

| Canonical role    | Org TODO keyword  | Meaning                                   |
|-------------------|-------------------|-------------------------------------------|
| `needs-triage`    | `NEEDS-TRIAGE`    | Maintainer needs to evaluate this ticket  |
| `needs-info`      | `NEEDS-INFO`      | Waiting for more information              |
| `ready-for-agent` | `READY-FOR-AGENT` | Fully specified, ready for an AFK agent   |
| `ready-for-human` | `READY-FOR-HUMAN` | Requires human implementation or judgment |
| `wontfix`         | `WONTFIX`         | Closed without action                     |

Workflow-only states are `IN-PROGRESS`, `CLAIMED`, and `RESOLVED`. Use `CLAIMED` plus the `ASSIGNEE` property for a wayfinder claim; use `RESOLVED` for completed work.

For a remote tracker, map the canonical roles to its native labels or states without changing these local Org keywords.
