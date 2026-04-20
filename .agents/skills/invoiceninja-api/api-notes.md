# Invoice Ninja API Notes

Keep this file short and current. Update it only with verified routes or observed response quirks.

Do not store real invoice ids, invitation keys, tokens, passwords, or company-specific values here. Use placeholders only.

## Config

- Config file: `~/.config/invoiceninja-cli/config.edn`
- Required keys:
  - `:base-url`
  - `:api-token`
- Accepted legacy alias:
  - `:api-key`
- File mode must be `0600`

## Auth Model

Protected endpoints use:

- `X-API-TOKEN`
- `X-Requested-With: XMLHttpRequest`

For JSON endpoints, also send:

- `Accept: application/json`

Login bootstrap route:

- `POST /api/v1/login`
- JSON body with `email` and `password`
- Optional header:
  - `X-API-SECRET`

Observed behavior:

- Omitting `X-Requested-With: XMLHttpRequest` on `/api/v1/login` returns the SPA HTML instead of the API response.
- Successful login responses include a `token` object. The token value is at `[:token :token]`.

## Verified Reads

### Ping

```text
GET /api/v1/ping
```

Useful for verifying the configured token before deeper work.

Observed response shape:

```json
{
  "company_name": "<COMPANY_NAME>",
  "user_name": "<USER_NAME>"
}
```

### List Invoices

```text
GET /api/v1/invoices
```

Verified query for latest open invoice:

```text
client_status=unpaid,overdue
status=active
without_deleted_clients=true
sort=date|desc
per_page=1
```

Observed behavior:

- On the tested instance, this returned the latest sent unpaid invoice.
- `payable` did not work as a general open-invoice discovery filter and returned no results without a client-specific value.

### Show Invoice

```text
GET /api/v1/invoices/<INVOICE_ID>
```

Observed response shape:

- The invoice is wrapped under `data`.
- Invitation keys are available at `[:data :invitations 0 :key]`.

### Download Invoice PDF

```text
GET /api/v1/invoice/<INVITATION_KEY>/download
```

Required headers:

- `X-API-TOKEN`
- `X-Requested-With: XMLHttpRequest`

Observed behavior:

- Response status `200`
- `Content-Type: application/pdf`
- `Content-Disposition` includes the invoice filename
- Saving the bytes to a temp file produced a valid PDF with magic bytes `%PDF-1.4`

## Practical Shapes

Latest open invoice:

```text
GET /api/v1/invoices?client_status=unpaid,overdue&status=active&without_deleted_clients=true&sort=date|desc&per_page=1
```

Download that invoice’s PDF:

1. Read the invoice invitation key from `[:invitations 0 :key]`
2. Call:

```text
GET /api/v1/invoice/<INVITATION_KEY>/download
```
