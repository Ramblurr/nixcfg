---
name: invoiceninja-api
description: Use when working with the Invoice Ninja API to verify access, inspect invoices, fetch invoice details, filter open or sent invoices, or download invoice PDFs from a configured instance.
---

# Invoice Ninja API

Use Babashka for all scripting and interactive API work. Keep the workflow Clojure-first.

## Preconditions

- Store config in `~/.config/invoiceninja-cli/config.edn`.
- Keep the config file owner-readable only with mode `0600`.
- Required keys:
  - `:base-url`
  - `:api-token`
- Accept `:api-key` as a legacy alias when `:api-token` is absent.
- Optional bootstrap-only keys:
  - `:email`
  - `:password`
  - `:one_time_password`
  - `:api-secret`

Use the sibling files in this skill directory when working with Invoice Ninja:

- [api-notes.md](api-notes.md) records only verified routes, filters, and response quirks.
- [scratch_invoiceninja.clj](scratch_invoiceninja.clj) is the ad hoc Babashka/Clojure scratchpad for live API work.

## REPL Workflow

Use a Babashka nREPL for ad hoc API work.

- If you already know the port, verify the REPL with:
  `BREPL_PORT=<port> brepl '(+ 1 2)'`
- If there is no running REPL, start one with:
  `bb --nrepl-server 0`
- Babashka prints the chosen port on startup. Use that port with `brepl`.
- Load the sibling scratch file with:
  `BREPL_PORT=<port> brepl -f ./scratch_invoiceninja.clj`

## Auth Model

- Protected endpoints need:
  - `X-API-TOKEN: <token>`
  - `X-Requested-With: XMLHttpRequest`
- Add `Accept: application/json` for JSON endpoints.
- Prefer a pre-created API token from the Invoice Ninja UI over password login.
- If you must bootstrap a token, `POST /api/v1/login` with JSON credentials and optional `X-API-SECRET`, then read `[:token :token]` from the response.
- `POST /api/v1/login` falls through to the SPA HTML if you omit `X-Requested-With: XMLHttpRequest`.

## Common Operations

- Verify token and instance:
  `GET /api/v1/ping`
- List invoices:
  `GET /api/v1/invoices`
- Find the latest open invoice:
  `client_status=unpaid,overdue`
  `status=active`
  `without_deleted_clients=true`
  `sort=date|desc`
  `per_page=1`
- Fetch invoice details:
  `GET /api/v1/invoices/{id}`
- Download an invoice PDF:
  `GET /api/v1/invoice/{invitation_key}/download`

Important response quirks:

- The invoice detail route returns the invoice under `:data`.
- The PDF route needs the invitation key from `[:invitations 0 :key]`.
- On the tested instance, `payable` did not work as a general “latest open invoice” filter. Prefer the verified open-invoice query above.

## Workflow

1. Read config and build auth headers.
2. Call `ping` before deeper work when you need to confirm the token.
3. For open invoices, start from the verified open-invoice query.
4. If you need a PDF, take the invitation key from the invoice response and call the download route.
5. Save PDFs to a temp file unless the human asked for a permanent location.
6. Delete temp files after verification if the file was only for a smoke test.

## Safety

- Treat create, update, delete, and bulk routes as mutating.
- Get explicit human approval before mutating invoices, clients, payments, tokens, or company settings.
- For destructive or bulk changes, print the exact target set before applying.
- Do not store live invoice ids, invitation keys, or tokens in skill files.
