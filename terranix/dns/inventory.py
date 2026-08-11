#!/usr/bin/env python3
"""Create reviewed DNS inventory candidates from deSEC and PowerDNS GET responses."""

import argparse
import json
import os
import re
import sys
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from pathlib import Path

AUTHORITY_TYPES = {"CDNSKEY", "CDS", "DNSKEY", "DS", "NS", "NSEC", "NSEC3", "RRSIG", "SOA"}
SUPPORTED_TYPES = {"A", "AAAA", "CAA", "CNAME", "MX", "SRV", "TXT"}
KEA_GENERATED_ADDRESS_LABEL = re.compile(r"^-\d{1,3}(?:-\d{1,3}){3}\.")


def fail(message):
    raise SystemExit(f"dns-dump: {message}")


def get_json(url, headers):
    request = urllib.request.Request(url, headers=headers, method="GET")
    with urllib.request.urlopen(request, timeout=30) as response:
        return json.load(response)


def relative_name(owner, zone):
    owner = owner.rstrip(".")
    if owner == zone:
        return "@"
    suffix = f".{zone}"
    if not owner.endswith(suffix):
        fail(f"owner {owner!r} is outside selected zone {zone!r}")
    return owner[: -len(suffix)]


def nix_string(value, zone, zone_key):
    if not isinstance(value, str) or not value:
        fail("encountered an empty or non-string RDATA value")
    return json.dumps(value.replace(f"{zone}.", f"${{zones.{zone_key}}}."))


def stable_id(zone_key, name, record_type):
    label = "apex" if name == "@" else re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-")
    return f"{zone_key}-{label}-{record_type.lower()}"


def exclude_reason(zone, name, record_type):
    if zone == "home.arpa" or zone.endswith(".in-addr.arpa") or zone.endswith(".ip6.arpa"):
        return "non-forward-zone"
    if record_type in AUTHORITY_TYPES:
        return "authority-or-dnssec"
    if record_type == "DHCID":
        return "kea-dynamic"
    if KEA_GENERATED_ADDRESS_LABEL.match(name):
        return "kea-generated-address-label"
    if name == "_acme-challenge" or name.startswith("_acme-challenge."):
        return "acme"
    if record_type not in SUPPORTED_TYPES:
        return "unsupported-type"
    return None


def add_rrset(groups, excluded, excluded_records, anomalies, zone_key, zone, surface, name, record_type, values, ttl):
    reason = exclude_reason(zone, name, record_type)
    if reason:
        excluded[reason] += 1
        excluded_records[(zone_key, name, record_type)].add(reason)
        return
    if not values or any(not isinstance(value, str) or not value for value in values):
        anomalies["invalid-rdata"] += 1
        return
    key = (zone_key, name, record_type)
    existing = groups[key].get(surface)
    candidate = {"values": sorted(set(values)), "ttl": ttl}
    if existing and existing != candidate:
        anomalies["conflicting-surface"] += 1
        groups[key]["conflict"] = True
        return
    groups[key][surface] = candidate


def fetch_desec(zone_key, zone, groups, excluded, excluded_records, anomalies):
    url = f"https://desec.io/api/v1/domains/{urllib.parse.quote(zone, safe='')}/rrsets/"
    data = get_json(url, {"Authorization": f"Token {os.environ['DESEC_API_TOKEN']}"})
    for rrset in data:
        name = rrset.get("subname") or "@"
        record_type = rrset.get("type", "").upper()
        add_rrset(groups, excluded, excluded_records, anomalies, zone_key, zone, "public", name, record_type, rrset.get("records", []), rrset.get("ttl"))


def fetch_powerdns(zone_key, zone, surface, groups, excluded, excluded_records, anomalies):
    api = os.environ["PDNS_SERVER_URL"].rstrip("/")
    zone_id = zone if surface == "lan" else f"{zone}..tailscale"
    url = f"{api}/api/v1/servers/localhost/zones/{urllib.parse.quote(zone_id, safe='')}"
    data = get_json(url, {"X-API-Key": os.environ["PDNS_API_KEY"]})
    for rrset in data.get("rrsets", []):
        records = rrset.get("records", [])
        if any(record.get("disabled") for record in records):
            anomalies["disabled-rrset"] += 1
            continue
        try:
            name = relative_name(rrset["name"], zone)
        except (KeyError, TypeError):
            anomalies["invalid-owner"] += 1
            continue
        add_rrset(
            groups,
            excluded,
            excluded_records,
            anomalies,
            zone_key,
            zone,
            surface,
            name,
            rrset.get("type", "").upper(),
            [record.get("content") for record in records],
            rrset.get("ttl"),
        )


def render_records(groups, zones, eligible_keys):
    lines = ["{ zones }:", "["]
    for (zone_key, name, record_type), surfaces in sorted(groups.items()):
        if (zone_key, name, record_type) not in eligible_keys:
            continue
        lines.extend([
            "  {",
            f'    id = {json.dumps(stable_id(zone_key, name, record_type))};',
            f"    zone = zones.{zone_key};",
            f"    name = {json.dumps(name)};",
            f"    type = {json.dumps(record_type)};",
        ])
        for surface in ("public", "lan", "tailscale"):
            if surface in surfaces:
                values = " ".join(nix_string(value, zones[zone_key], zone_key) for value in surfaces[surface]["values"])
                lines.append(f"    {surface} = [ {values} ];")
                lines.append(f"    {surface}Ttl = {surfaces[surface]['ttl']};")
        lines.extend(["  }", ""])
    lines.append("]")
    return "\n".join(lines) + "\n"


def render_imports(groups, eligible_keys):
    lines = ["{ zones }:", "["]
    for (zone_key, name, record_type), surfaces in sorted(groups.items()):
        if (zone_key, name, record_type) not in eligible_keys:
            continue
        identifier = stable_id(zone_key, name, record_type)
        lines.append("  {")
        lines.append(f'    id = {json.dumps(identifier)};')
        lines.append("    imports = [")
        if "public" in surfaces:
            lines.extend([
                "      {",
                "        address = " + json.dumps(f'desec_rrset.public["{identifier}"]') + ";",
                "        provider = \"desec\";",
                f"        domain = zones.{zone_key};",
                f"        subname = {json.dumps(name)};",
                f"        type = {json.dumps(record_type)};",
                "      }",
            ])
        for surface in ("lan", "tailscale"):
            if surface in surfaces:
                lines.extend([
                    "      {",
                    "        address = " + json.dumps(f'powerdns_record.{surface}["{identifier}"]') + ";",
                    "        provider = \"powerdns\";",
                    f"        zone = zones.{zone_key}" + (" + \".\";" if surface == "lan" else " + \"..tailscale\";"),
                    f"        name = {json.dumps(name)};",
                    f"        type = {json.dumps(record_type)};",
                    "      }",
                ])
        lines.extend(["    ];", "  }", ""])
    lines.append("]")
    return "\n".join(lines) + "\n"


def load_managed_records(path, zones):
    try:
        records = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        fail(f"cannot read managed-records file: {error}")
    if not isinstance(records, list):
        fail("managed-records file must contain a list")
    identities = set()
    for record in records:
        if not isinstance(record, dict):
            fail("managed-records entries must be objects")
        zone_key = record.get("zoneKey")
        name = record.get("name")
        record_type = record.get("type")
        if zone_key not in zones or not isinstance(name, str) or not isinstance(record_type, str):
            fail("managed-records entry is invalid")
        identities.add((zone_key, name, record_type.upper()))
    return identities


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--managed-records", required=True, type=Path)
    parser.add_argument("--zone", action="append", required=True, metavar="KEY=ZONE")
    args = parser.parse_args()

    output = args.output_dir
    if output.exists() and any(output.iterdir()):
        fail(f"output directory {output} must be new or empty")
    output.mkdir(mode=0o700, parents=True, exist_ok=True)

    zones = {}
    for item in args.zone:
        key, separator, zone = item.partition("=")
        if not separator or not key or not zone or key in zones:
            fail("--zone values must be unique KEY=ZONE pairs")
        zones[key] = zone.rstrip(".")
    managed_records = load_managed_records(args.managed_records, zones)

    for required in ("DESEC_API_TOKEN", "PDNS_API_KEY", "PDNS_SERVER_URL"):
        if not os.environ.get(required):
            fail(f"missing required environment variable {required}")

    groups = defaultdict(dict)
    excluded = Counter()
    excluded_records = defaultdict(set)
    anomalies = Counter()
    for zone_key, zone in sorted(zones.items()):
        fetch_desec(zone_key, zone, groups, excluded, excluded_records, anomalies)
        fetch_powerdns(zone_key, zone, "lan", groups, excluded, excluded_records, anomalies)
        fetch_powerdns(zone_key, zone, "tailscale", groups, excluded, excluded_records, anomalies)

    groups = dict(groups)
    included_records = []
    included_keys = set()
    included_surface_counts = Counter()
    for key, surfaces in sorted(groups.items()):
        zone_key, name, record_type = key
        identifier = stable_id(zone_key, name, record_type)
        surface_names = set(surfaces) - {"conflict"}
        if surfaces.get("conflict"):
            excluded["conflicting-surface"] += 1
            excluded_records[key].add("conflicting-surface")
        elif key in managed_records:
            excluded["already-managed"] += 1
            excluded_records[key].add("already-managed")
        else:
            included_keys.add(key)
            included_records.append({"id": identifier, "surfaces": sorted(surface_names)})
            included_surface_counts["+".join(sorted(surface_names))] += 1

    records = render_records(groups, zones, included_keys)
    imports = render_imports(groups, included_keys)
    report = {
        "included_records": included_records,
        "included_surface_counts": dict(sorted(included_surface_counts.items())),
        "excluded_records": [
            {
                "id": stable_id(zone_key, name, record_type),
                "reasons": sorted(reasons),
            }
            for (zone_key, name, record_type), reasons in sorted(excluded_records.items())
        ],
        "excluded": dict(sorted(excluded.items())),
        "anomalies": dict(sorted(anomalies.items())),
        "generated_record_count": len(included_keys),
    }
    for path, content in ((output / "records.nix", records), (output / "imports.nix", imports)):
        path.write_text(content)
        path.chmod(0o600)
    report_path = output / "report.json"
    report_path.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    report_path.chmod(0o600)
    print(f"dns-dump: wrote {len(included_keys)} included groups to {output}", file=sys.stderr)


if __name__ == "__main__":
    main()
