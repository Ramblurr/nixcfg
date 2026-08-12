# Managing DNS

This folder defines the rules used to manage DNS. The actual zones and records are private.

For normal DNS work, start here:

```text
~/nixcfg-private/terranix/dns/
```

Run all commands from `~/nixcfg-private`.

## Local record editor

Start the file-only editor from `~/nixcfg`:

```bash
./scripts/dns-admin-ui.clj
```

Open `http://127.0.0.1:8083/`. The editor binds only to loopback and stages
changes in memory. **Commit changes** writes the affected JSON files atomically;
it does not create a Git commit, run Nix or OpenTofu, or contact DNS providers.
Review the resulting file diff before using the plan and apply workflow below.

Use `--zones-dir PATH` for fixtures or another checkout and `--port PORT` to
select a different loopback port.

## Add or change a record

Records are grouped into authoritative JSON documents, such as
`terranix/dns/zones/home.json` and `terranix/dns/zones/work.json`.

Search the zone document for the name or record ID. Edit the record where it
already exists, or append a new record to its `records` array. Records do not
contain a `zone` field; the document's literal `domain` supplies it.

A record may appear on any combination of these networks:

- `public`: internet DNS, served by deSEC
- `lan`: DNS for the local network, served by PowerDNS
- `tailscale`: DNS for Tailscale devices, served by PowerDNS

Only fields present in the record are managed. For example, a record with `lan` and `tailscale` but no `public` field stays private.

```json
{
  "id": "home-books-cname",
  "name": "books",
  "type": "CNAME",
  "public": ["dewey.example.net."],
  "lan": ["dewey.prim.example.net."],
  "tailscale": ["dewey.prim.example.net."]
}
```

For an address record:

```json
{
  "id": "home-printer-a",
  "name": "printer",
  "type": "A",
  "lan": ["10.9.4.20"],
  "tailscale": ["10.9.4.20"]
}
```

## Record fields

- `id`: a unique permanent name used to track the record. Do not change it after the first apply.
- `name`: the part before the zone. Use `"@"` for the zone itself.
- `type`: the uppercase DNS type, such as `A`, `AAAA`, `CNAME`, `MX`, `TXT`, `SRV`, or `CAA`.
- `public`, `lan`, `tailscale`: non-empty lists of values for each network.

Store fully resolved literal DNS values. Domain names used as values normally
need a trailing dot:

```json
"lan": ["host.prim.example.net."]
```

TTL fields are optional. The defaults are:

- `"publicTtl": 3600`
- `"lanTtl": 300`
- `"tailscaleTtl": 300`

Set a TTL only when the record needs a different value:

```json
"lanTtl": 3600
```

## Review and publish one zone

Run all commands from `~/nixcfg-private`. Give the zone name before `plan`:

```bash
cd ~/nixcfg-private
nix run .#dns -- home plan -out=home.tfplan
```

Use `work` instead of `home` for the work zone. The plan includes only the
selected zone. Read it carefully; it must show only the changes you intended.
Creating a plan does not change DNS.

Apply that exact saved plan:

```bash
nix run .#dns -- home apply home.tfplan
```

The zone name makes the command reject an apply without a saved plan. OpenTofu
does not recalculate the plan during apply.

Run a full plan when you want to check every zone and the shared Tailscale
network settings:

```bash
nix run .#dns -- plan -detailed-exitcode
```

An exit code of `0` means the files and DNS providers agree. An exit code of
`2` means OpenTofu proposes changes.

Credentials are loaded automatically from SOPS. OpenTofu's encrypted tracking
data is stored in the private S3 backend; do not create or commit local state
files.

## Removing records or networks

Deleting a record from the files makes the next plan propose deleting it from DNS. Removing `public`, `lan`, or `tailscale` removes only that version of the record.

Always inspect the plan before applying a deletion.

## Add another zone

1. Choose a short private zone key.
2. Create `terranix/dns/zones/KEY.json` with the zone key, literal domain, exact
   `public`, `lan`, and `tailscale` surfaces, and its ordered record list.

Use all three surfaces only when the zone needs all three. Public-only zones
create no PowerDNS resources.

For a new zone with existing records, create review artifacts before accepting
the authoritative zone document:

```bash
nix run .#dns-dump -- example OUTPUT-DIR=terranix/dns-staging/example
```

Review `zone.json`, `imports.nix`, and `report.json`. Copy the accepted
`zone.json` to `terranix/dns/zones/example.json`, then create and apply one
encrypted, configuration-driven import plan:

```bash
nix run .#dns-import -- plan example terranix/dns-staging/example example.tfplan
nix run .#dns-import -- apply example terranix/dns-staging/example example.tfplan
```

The importer rejects record mutations, destructive actions, unrelated module
addresses, and topology changes absent from the reviewed report. Confirm the
result with a zone plan and then a full plan.

## Files in this folder

The public files in this directory validate records and turn them into deSEC
and PowerDNS configuration. Most DNS changes require no edits here; edit the
authoritative private JSON document under `terranix/dns/zones/` instead.

The private `zones.nix` discovers the JSON documents. Its `records.nix` loader
only adds the correct literal domain to each record.
