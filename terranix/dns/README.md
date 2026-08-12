# Managing DNS

This folder defines the rules used to manage DNS. The actual zones and records are private.

For normal DNS work, start here:

```text
~/nixcfg-private/terranix/dns/
```

Run all commands from `~/nixcfg-private`.

## Add or change a record

Records are grouped by zone:

- `terranix/dns/zones/home.nix` contains the home zone.
- `terranix/dns/zones/work.nix` contains the work zone.

Search those files for the name or record ID. Edit the record where it already
exists. Add a new record to the file for its zone. You do not need to write a
`zone` field; the file supplies it automatically.

A record may appear on any combination of these networks:

- `public`: internet DNS, served by deSEC
- `lan`: DNS for the local network, served by PowerDNS
- `tailscale`: DNS for Tailscale devices, served by PowerDNS

Only fields present in the record are managed. For example, a record with `lan` and `tailscale` but no `public` field stays private.

```nix
{
  id = "home-books-cname";
  name = "books";
  type = "CNAME";
  public = [ "dewey.${zones.home}." ];
  lan = [ "dewey.prim.${zones.home}." ];
  tailscale = [ "dewey.prim.${zones.home}." ];
}
```

For an address record:

```nix
{
  id = "home-printer-a";
  name = "printer";
  type = "A";
  lan = [ "10.9.4.20" ];
  tailscale = [ "10.9.4.20" ];
}
```

## Record fields

- `id`: a unique permanent name used to track the record. Do not change it after the first apply.
- `name`: the part before the zone. Use `"@"` for the zone itself.
- `type`: the uppercase DNS type, such as `A`, `AAAA`, `CNAME`, `MX`, `TXT`, `SRV`, or `CAA`.
- `public`, `lan`, `tailscale`: non-empty lists of values for each network.

Use zone references instead of writing private zone names directly. Domain names used as DNS values normally need a trailing dot:

```nix
lan = [ "host.prim.${zones.home}." ];
```

TTL fields are optional. The defaults are:

- `publicTtl = 3600`
- `lanTtl = 300`
- `tailscaleTtl = 300`

Set a TTL only when the record needs a different value:

```nix
lanTtl = 3600;
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

1. Add a short private key and zone name to the domain settings used by
   `nixcfg-private`.
2. Create `terranix/dns/zones/KEY.nix` with a list of records.
3. Register the file and exact surfaces in `terranix/dns/zones.nix`:

```nix
example = {
  file = ./zones/example.nix;
  surfaces = [ "public" ];
};
```

Use `[ "public" "lan" "tailscale" ]` only when the zone needs all three
surfaces. Public-only zones create no PowerDNS resources.

For a new zone with existing records, create review artifacts before copying
records into the authoritative zone file:

```bash
nix run .#dns-dump -- example OUTPUT-DIR=terranix/dns-staging/example
```

Review `zone.nix`, `imports.nix`, and `report.json`. After copying the accepted
`zone.nix` list to `terranix/dns/zones/example.nix`, create and apply one
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
private file under `terranix/dns/zones/` instead.

The private `records.nix` file only loads the zone files and adds their zone to
each record. Do not add individual records to that loader.
