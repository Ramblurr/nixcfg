# Incident Template

Use this exact section order for new incident records.

```md
# <Title>

Date: YYYY-MM-DD
Status: open | mitigated | resolved
Severity: low | medium | high | critical
Systems: host1, host2, service1, service2
Tags: auth, oidc, deploy, cert-expiry

## Summary

1-3 sentences describing the user-visible problem and the current or final root cause.

## Impact

- Who or what was affected
- What failed
- Start time
- End time or current state

## What Happened

Facts only.

- Detection method
- Symptom summary
- Timeline with timestamps
- Relevant logs, errors, and observations

## Why It Happened

Analysis only.

- Root cause
- Trigger
- Contributing factors
- What was ruled out

## What We Did

Only actions actually taken.

- Investigation steps
- Commands run
- Changes made
- Recovery or mitigation performed
- Verification after changes

## What We Should Do Better

Future work only.

- Short-term follow-up
- Long-term fix
- Monitoring or alerting gaps
- Documentation or runbook gaps
- Preventive changes

## References

- Related files
- Relevant hosts or modules
- Prior incidents
- Notes or tracker links
```

Conventions:

- Keep the title short, ASCII, and symptom-first.
- Use the incident start date in the path when known.
- Keep `What Happened` factual even if that repeats information from later sections.
- Keep `What We Did` limited to completed actions.
- Put ideas, prevention, and unfinished work only in `What We Should Do Better`.
