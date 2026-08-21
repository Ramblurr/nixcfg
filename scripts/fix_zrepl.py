#!/usr/bin/env python3
"""Plan and apply exact Debord-to-Mali zrepl snapshot reconciliation."""

import argparse
import datetime as dt
import hashlib
import json
import re
import shlex
import subprocess
import sys
from pathlib import Path

SENDER_HOST = "debord"
RECEIVER_HOST = "mali"
RECEIVER_PREFIX = "tank2/replication/debord/"
ALLOWED_SENDER_ROOTS = (
    "rpool/encrypted/safe/extra/atuin",
    "rpool/encrypted/safe/persist",
    "rpool/encrypted/safe/svc",
    "rpool/encrypted/safe/vms",
)
DATASET_RE = re.compile(r"^[A-Za-z0-9_.:-]+(?:/[A-Za-z0-9_.:-]+)*$")
SCHEMA = "fix-zrepl-debord-mali-v1"
BUNDLE_SCHEMA = "fix-zrepl-debord-mali-bundle-v1"


class ReconcileError(RuntimeError):
    pass


def fail(message):
    raise ReconcileError(message)


def validate_mapping(sender, receiver):
    if not DATASET_RE.fullmatch(sender) or not DATASET_RE.fullmatch(receiver):
        fail("invalid dataset path")
    if ".." in sender.split("/") or ".." in receiver.split("/"):
        fail("invalid dataset path")
    if not any(
        sender == root or sender.startswith(root + "/") for root in ALLOWED_SENDER_ROOTS
    ):
        fail(f"sender is outside the Debord zrepl roots: {sender}")
    expected = RECEIVER_PREFIX + sender
    if receiver != expected:
        fail(f"receiver mapping mismatch: expected {expected}")


def parse_cutoff(value):
    if not value.endswith("Z"):
        fail("cutoff must be UTC and end in Z")
    try:
        parsed = dt.datetime.fromisoformat(value[:-1] + "+00:00")
    except ValueError as exc:
        raise ReconcileError(f"invalid cutoff: {value}") from exc
    if parsed.second or parsed.microsecond:
        fail("cutoff must be minute-aligned")
    return int(parsed.timestamp())


def run_remote(host, argv, check=True):
    command = shlex.join(argv)
    result = subprocess.run(
        ["ssh", host, command],
        text=True,
        capture_output=True,
        check=False,
    )
    if check and result.returncode:
        fail(
            f"{host}: command failed ({result.returncode}): {command}\n{result.stderr.strip()}"
        )
    return result


def run_remote_script(host, script):
    result = subprocess.run(
        ["ssh", host, "/bin/sh -s"],
        input=script,
        text=True,
        capture_output=True,
        check=False,
    )
    if result.returncode:
        fail(
            f"{host}: reconciliation batch failed ({result.returncode})\n"
            f"stdout:\n{result.stdout.strip()}\nstderr:\n{result.stderr.strip()}"
        )
    return result


def dataset_exists(host, dataset):
    result = run_remote(
        host, ["sudo", "zfs", "list", "-H", "-o", "name", dataset], check=False
    )
    if result.returncode == 0:
        return True
    if result.returncode == 1 and "does not exist" in result.stderr.lower():
        return False
    fail(
        f"{host}: cannot determine whether ZFS object exists ({result.returncode}): "
        f"{result.stderr.strip()}"
    )


def parse_versions(text):
    versions = []
    for line in text.splitlines():
        fields = line.split("\t")
        if len(fields) != 4 or fields[1] not in {"snapshot", "bookmark"}:
            fail(f"unexpected zfs version row: {line!r}")
        versions.append(
            {
                "name": fields[0],
                "type": fields[1],
                "guid": fields[2],
                "creation": int(fields[3]),
            }
        )
    return versions


def inventory(host, dataset, absent_ok=False):
    if not dataset_exists(host, dataset):
        if absent_ok:
            return {"host": host, "dataset": dataset, "exists": False}
        fail(f"{host}: dataset does not exist: {dataset}")

    version_result = run_remote(
        host,
        [
            "sudo",
            "zfs",
            "list",
            "-H",
            "-p",
            "-d",
            "1",
            "-t",
            "snapshot,bookmark",
            "-o",
            "name,type,guid,creation",
            "-s",
            "creation",
            dataset,
        ],
    )
    versions = parse_versions(version_result.stdout)

    refs_result = run_remote(
        host,
        [
            "sudo",
            "zfs",
            "get",
            "-H",
            "-p",
            "-r",
            "-o",
            "name,value",
            "userrefs",
            dataset,
        ],
    )
    userrefs = {}
    prefix = dataset + "@"
    for line in refs_result.stdout.splitlines():
        fields = line.split("\t")
        if (
            len(fields) == 2
            and fields[0].startswith(prefix)
            and "/" not in fields[0][len(prefix) :]
        ):
            userrefs[fields[0]] = int(fields[1])

    snapshots = [
        version["name"] for version in versions if version["type"] == "snapshot"
    ]
    if set(userrefs) != set(snapshots):
        fail(f"{host}: incomplete direct snapshot userrefs inventory for {dataset}")

    holds = {}
    for snapshot in snapshots:
        if not userrefs[snapshot]:
            holds[snapshot] = []
            continue
        result = run_remote(host, ["sudo", "zfs", "holds", "-H", snapshot])
        tags = []
        for line in result.stdout.splitlines():
            fields = line.split("\t")
            if len(fields) < 2 or fields[0] != snapshot:
                fail(f"{host}: unexpected hold row: {line!r}")
            tags.append(fields[1])
        if len(tags) != userrefs[snapshot]:
            fail(f"{host}: hold count mismatch for {snapshot}")
        holds[snapshot] = sorted(tags)

    abstractions = sorted(
        line
        for line in run_remote(
            host, ["sudo", "zrepl", "zfs-abstraction", "list", "--fs", dataset]
        ).stdout.splitlines()
        if line
    )
    guid = run_remote(
        host, ["sudo", "zfs", "list", "-H", "-p", "-o", "guid", dataset]
    ).stdout.strip()
    token = run_remote(
        host,
        ["sudo", "zfs", "get", "-H", "-o", "value", "receive_resume_token", dataset],
    ).stdout.strip()

    return {
        "host": host,
        "dataset": dataset,
        "exists": True,
        "guid": guid,
        "receive_resume_token": "absent" if token == "-" else "present-redacted",
        "versions": versions,
        "userrefs": userrefs,
        "holds": holds,
        "abstractions": abstractions,
    }


def is_zrepl_snapshot(version):
    return version["type"] == "snapshot" and version["name"].split("@", 1)[
        1
    ].startswith("zrepl_")


def derive_plan(sender, receiver, cutoff_text):
    validate_mapping(sender["dataset"], receiver["dataset"])
    if sender.get("host") != SENDER_HOST or receiver.get("host") != RECEIVER_HOST:
        fail("manifest endpoint host mismatch")
    if sender.get("receive_resume_token") != "absent":
        fail("sender receive token must be absent")
    if receiver.get("exists") and receiver.get("receive_resume_token") != "absent":
        fail("receiver receive token must be absent")
    cutoff = parse_cutoff(cutoff_text)
    sender_snapshots = [v for v in sender["versions"] if v["type"] == "snapshot"]
    if not any(v["creation"] >= cutoff for v in sender_snapshots):
        fail("sender has no snapshot at or after cutoff")

    common = None
    common_sender = None
    receiver_delete = []
    mode = "initial-receive"
    if receiver["exists"]:
        sender_guids = {v["guid"] for v in sender["versions"]}
        receiver_snapshots = [
            v for v in receiver["versions"] if v["type"] == "snapshot"
        ]
        common_candidates = [v for v in receiver_snapshots if v["guid"] in sender_guids]
        if not common_candidates:
            fail("receiver exists but no common GUID was found")
        common = common_candidates[-1]
        sender_common_versions = [
            v for v in sender["versions"] if v["guid"] == common["guid"]
        ]
        common_sender = next(
            (v for v in reversed(sender_common_versions) if v["type"] == "snapshot"),
            sender_common_versions[-1],
        )
        common_index = receiver_snapshots.index(common)
        receiver_delete = receiver_snapshots[common_index + 1 :]
        mode = "proven-common"
        for version in receiver_delete:
            if not is_zrepl_snapshot(version):
                fail(
                    f"receiver-only non-zrepl snapshot requires manual disposition: {version['name']}"
                )
            if (
                receiver["userrefs"][version["name"]]
                or receiver["holds"][version["name"]]
            ):
                fail(f"receiver-only snapshot is held: {version['name']}")

    common_guid = common["guid"] if common else None
    sender_delete = []
    for version in sender_snapshots:
        if version["creation"] >= cutoff or version["guid"] == common_guid:
            continue
        if not is_zrepl_snapshot(version):
            continue
        if sender["userrefs"][version["name"]] or sender["holds"][version["name"]]:
            fail(f"old sender snapshot is held: {version['name']}")
        sender_delete.append(version)

    return {
        "schema": SCHEMA,
        "unit_id": sender["dataset"].replace("/", "-"),
        "cutoff": cutoff_text,
        "mode": mode,
        "sender": sender,
        "receiver": receiver,
        "common": common,
        "common_sender": common_sender,
        "sender_delete": sender_delete,
        "receiver_delete": receiver_delete,
        "preserved_sender_bookmarks": [
            v for v in sender["versions"] if v["type"] == "bookmark"
        ],
        "preserved_receiver_bookmarks": (
            [v for v in receiver["versions"] if v["type"] == "bookmark"]
            if receiver["exists"]
            else []
        ),
    }


def manifest_digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def target_names(plan, side):
    return [v["name"] for v in plan[f"{side}_delete"]]


def inventory_without_targets(original, deleted_names):
    expected = json.loads(json.dumps(original))
    if not expected["exists"]:
        return expected
    deleted = set(deleted_names)
    expected["versions"] = [v for v in expected["versions"] if v["name"] not in deleted]
    for name in deleted:
        expected["userrefs"].pop(name, None)
        expected["holds"].pop(name, None)
    return expected


def deleted_prefix(plan, current, side):
    targets = target_names(plan, side)
    current_names = {v["name"] for v in current["versions"]}
    states = [name not in current_names for name in targets]
    seen_present = False
    for missing in states:
        if not missing:
            seen_present = True
        elif seen_present:
            fail(f"{side}: deleted targets are not an exact prefix")
    return sum(states)


def check_side(plan, side):
    endpoint = plan[side]
    if side == "receiver" and not endpoint["exists"]:
        if plan["receiver_delete"]:
            fail("absent receiver has deletion targets")
        return endpoint, 0
    current = inventory(endpoint["host"], endpoint["dataset"])
    count = deleted_prefix(plan, current, side)
    expected = inventory_without_targets(endpoint, target_names(plan, side)[:count])
    if current != expected:
        fail(f"{side}: live inventory drifted from the manifest")
    return current, count


def require_idle(host):
    active = run_remote(host, ["systemctl", "is-active", "zrepl"], check=False)
    if active.stdout.strip() != "inactive":
        fail(f"{host}: zrepl must be inactive")
    transfers = run_remote(
        host,
        ["/bin/sh", "-c", "pgrep -af 'zfs (send|receive|recv)' || true"],
    ).stdout.strip()
    if transfers:
        fail(f"{host}: active ZFS transfer: {transfers}")
    pool = "rpool" if host == SENDER_HOST else "tank2"
    health = run_remote(host, ["sudo", "zpool", "status", "-x", pool]).stdout.strip()
    if "healthy" not in health:
        fail(f"{host}: pool is not healthy: {health}")


def require_pair_idle():
    require_idle(SENDER_HOST)
    require_idle(RECEIVER_HOST)


def version_exists_with_guid(host, version):
    result = run_remote(
        host,
        [
            "sudo",
            "zfs",
            "list",
            "-H",
            "-p",
            "-t",
            version["type"],
            "-o",
            "guid",
            version["name"],
        ],
        check=False,
    )
    if result.returncode == 0:
        if result.stdout.strip() != version["guid"]:
            fail(f"{host}: GUID changed for {version['name']}")
        return True
    if result.returncode == 1 and "does not exist" in result.stderr.lower():
        return False
    fail(
        f"{host}: cannot verify ZFS version ({result.returncode}): "
        f"{result.stderr.strip()}"
    )


def require_common(plan):
    if not plan["common"]:
        return
    checks = (
        (SENDER_HOST, plan["common_sender"]),
        (RECEIVER_HOST, plan["common"]),
    )
    for host, version in checks:
        if not version_exists_with_guid(host, version):
            fail(f"{host}: common GUID disappeared")


def build_destroy_script(endpoint, targets, common):
    prefix = endpoint["dataset"] + "@zrepl_"
    for version in targets:
        suffix = (
            version["name"][len(prefix) :] if version["name"].startswith(prefix) else ""
        )
        if (
            version["type"] != "snapshot"
            or not suffix
            or not re.fullmatch(r"[A-Za-z0-9_.:-]+", suffix)
            or not version["guid"].isdigit()
        ):
            fail(f"unsafe batch target: {version['name']}")

    if common:
        common_check = f"""
check_common() {{
    actual=$(sudo zfs list -H -p -t {shlex.quote(common['type'])} -o guid {shlex.quote(common['name'])})
    [ "$actual" = {shlex.quote(common['guid'])} ] || {{ echo 'common GUID changed' >&2; exit 70; }}
}}
"""
    else:
        common_check = "check_common() { :; }\n"

    rows = "\n".join(f"{version['name']} {version['guid']}" for version in targets)
    return f"""#!/bin/sh
set -eu
{common_check}
check_common
while IFS=' ' read -r snapshot expected_guid; do
    [ -n "$snapshot" ] || continue
    actual_guid=$(sudo zfs list -H -p -t snapshot -o guid "$snapshot")
    [ "$actual_guid" = "$expected_guid" ] || {{ echo "GUID changed: $snapshot" >&2; exit 70; }}
    userrefs=$(sudo zfs get -H -p -o value userrefs "$snapshot")
    [ "$userrefs" = 0 ] || {{ echo "snapshot became held: $snapshot" >&2; exit 70; }}
    sudo zfs destroy "$snapshot"
    if sudo zfs list -H -t snapshot -o name "$snapshot" >/dev/null 2>&1; then
        echo "snapshot still exists: $snapshot" >&2
        exit 70
    fi
    check_common
    printf 'DELETED %s guid=%s\\n' "$snapshot" "$expected_guid"
done <<'__FIX_ZREPL_TARGETS__'
{rows}
__FIX_ZREPL_TARGETS__
"""


def destroy_batch(endpoint, targets, common):
    if not targets:
        return
    result = run_remote_script(
        endpoint["host"], build_destroy_script(endpoint, targets, common)
    )
    if result.stdout:
        print(result.stdout, end="")


def apply_side(plan, side):
    endpoint = plan[side]
    if side == "receiver" and not endpoint["exists"]:
        print("receiver absent; no divergent snapshots to delete")
        return
    require_pair_idle()
    _, completed = check_side(plan, side)
    require_common(plan)
    targets = plan[f"{side}_delete"]
    common = plan["common_sender"] if side == "sender" else plan["common"]
    destroy_batch(endpoint, targets[completed:], common)
    require_common(plan)
    _, completed = check_side(plan, side)
    if completed != len(targets):
        fail(f"{side}: deletion did not complete")
    print(f"COMPLETE side={side} deleted={completed}")


def validate_plan(plan):
    if plan.get("schema") != SCHEMA:
        fail("unsupported manifest schema")
    validate_mapping(plan["sender"]["dataset"], plan["receiver"]["dataset"])
    derived = derive_plan(plan["sender"], plan["receiver"], plan["cutoff"])
    if plan != derived:
        fail("manifest targets do not match its embedded inventories")
    return plan


def load_plan(path):
    try:
        plan = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        raise ReconcileError(f"cannot load manifest: {exc}") from exc
    return validate_plan(plan)


def command_plan(args):
    validate_mapping(args.sender_dataset, args.receiver_dataset)
    sender = inventory(SENDER_HOST, args.sender_dataset)
    receiver = inventory(RECEIVER_HOST, args.receiver_dataset, absent_ok=True)
    plan = derive_plan(sender, receiver, args.cutoff)
    output = Path(args.output)
    output.write_text(json.dumps(plan, sort_keys=True, indent=2) + "\n")
    digest = manifest_digest(output)
    print(f"manifest={output}")
    print(f"sha256={digest}")
    print(f"mode={plan['mode']}")
    print(f"sender_delete={len(plan['sender_delete'])}")
    print(f"receiver_delete={len(plan['receiver_delete'])}")
    if plan["common"]:
        print(f"common_guid={plan['common']['guid']}")
    print(f"approval=GO manifest SHA-256 {digest} for repair unit {plan['unit_id']}.")


def command_check(args):
    path = Path(args.manifest)
    if manifest_digest(path) != args.sha256:
        fail("manifest SHA-256 mismatch")
    plan = load_plan(path)
    require_pair_idle()
    current, completed = check_side(plan, args.side)
    require_common(plan)
    print(
        f"CHECK_OK side={args.side} completed={completed} remaining={len(target_names(plan, args.side)) - completed}"
    )


def command_apply(args):
    path = Path(args.manifest)
    if manifest_digest(path) != args.sha256:
        fail("manifest SHA-256 mismatch")
    plan = load_plan(path)
    expected = f"GO manifest SHA-256 {args.sha256} for repair unit {plan['unit_id']}."
    if args.approval != expected:
        fail(f"exact approval required: {expected}")
    apply_side(plan, args.side)


def validate_bundle(bundle):
    if bundle.get("schema") != BUNDLE_SCHEMA or not isinstance(
        bundle.get("units"), list
    ):
        fail("unsupported bundle schema")
    if not bundle["units"]:
        fail("bundle has no repair units")
    units = [validate_plan(plan) for plan in bundle["units"]]
    datasets = [plan["sender"]["dataset"] for plan in units]
    if len(datasets) != len(set(datasets)):
        fail("bundle contains duplicate sender datasets")
    expected = {
        "schema": BUNDLE_SCHEMA,
        "cutoff": units[0]["cutoff"],
        "units": sorted(units, key=lambda plan: plan["sender"]["dataset"]),
    }
    if (
        any(plan["cutoff"] != expected["cutoff"] for plan in units)
        or bundle != expected
    ):
        fail("bundle content or ordering is not canonical")
    return bundle


def load_bundle(path):
    try:
        bundle = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        raise ReconcileError(f"cannot load bundle: {exc}") from exc
    return validate_bundle(bundle)


def bundle_approval(digest):
    return f"GO bundle SHA-256 {digest} for Debord-to-Mali reconciliation."


def command_bundle(args):
    units = [load_plan(Path(name)) for name in args.manifests]
    cutoffs = {plan["cutoff"] for plan in units}
    if len(cutoffs) != 1:
        fail("all bundle manifests must use one cutoff")
    bundle = {
        "schema": BUNDLE_SCHEMA,
        "cutoff": next(iter(cutoffs)),
        "units": sorted(units, key=lambda plan: plan["sender"]["dataset"]),
    }
    validate_bundle(bundle)
    output = Path(args.output)
    output.write_text(json.dumps(bundle, sort_keys=True, indent=2) + "\n")
    digest = manifest_digest(output)
    print(f"bundle={output}")
    print(f"sha256={digest}")
    print(f"units={len(units)}")
    print(f"sender_delete={sum(len(plan['sender_delete']) for plan in units)}")
    print(f"receiver_delete={sum(len(plan['receiver_delete']) for plan in units)}")
    print(f"approval={bundle_approval(digest)}")


def command_check_bundle(args):
    path = Path(args.bundle)
    if manifest_digest(path) != args.sha256:
        fail("bundle SHA-256 mismatch")
    bundle = load_bundle(path)
    require_pair_idle()
    for plan in bundle["units"]:
        _, completed = check_side(plan, args.side)
        require_common(plan)
        print(
            f"CHECK_OK unit={plan['unit_id']} side={args.side} "
            f"completed={completed} remaining={len(target_names(plan, args.side)) - completed}"
        )


def command_apply_bundle(args):
    path = Path(args.bundle)
    if manifest_digest(path) != args.sha256:
        fail("bundle SHA-256 mismatch")
    bundle = load_bundle(path)
    expected = bundle_approval(args.sha256)
    if args.approval != expected:
        fail(f"exact approval required: {expected}")
    require_pair_idle()
    for plan in bundle["units"]:
        print(f"UNIT_BEGIN {plan['unit_id']} side={args.side}", flush=True)
        apply_side(plan, args.side)
        print(f"UNIT_COMPLETE {plan['unit_id']} side={args.side}", flush=True)


def parser():
    result = argparse.ArgumentParser(description=__doc__)
    sub = result.add_subparsers(dest="command", required=True)

    plan = sub.add_parser(
        "plan", help="inventory live hosts and write a non-executing manifest"
    )
    plan.add_argument("--sender-dataset", required=True)
    plan.add_argument("--receiver-dataset", required=True)
    plan.add_argument(
        "--cutoff", required=True, help="UTC timestamp, e.g. 2026-06-21T00:00:00Z"
    )
    plan.add_argument("--output", required=True)
    plan.set_defaults(func=command_plan)

    check = sub.add_parser(
        "check", help="verify an unchanged manifest and stopped host"
    )
    check.add_argument("--manifest", required=True)
    check.add_argument("--sha256", required=True)
    check.add_argument("--side", choices=("sender", "receiver"), required=True)
    check.set_defaults(func=command_check)

    apply = sub.add_parser(
        "apply", help="delete the manifest's exact snapshot list on one side"
    )
    apply.add_argument("--manifest", required=True)
    apply.add_argument("--sha256", required=True)
    apply.add_argument("--side", choices=("sender", "receiver"), required=True)
    apply.add_argument("--approval", required=True)
    apply.set_defaults(func=command_apply)
    bundle = sub.add_parser(
        "bundle", help="combine exact unit manifests under one digest"
    )
    bundle.add_argument("--output", required=True)
    bundle.add_argument("manifests", nargs="+")
    bundle.set_defaults(func=command_bundle)

    check_bundle = sub.add_parser(
        "check-bundle", help="verify every unit in an unchanged bundle"
    )
    check_bundle.add_argument("--bundle", required=True)
    check_bundle.add_argument("--sha256", required=True)
    check_bundle.add_argument("--side", choices=("sender", "receiver"), required=True)
    check_bundle.set_defaults(func=command_check_bundle)

    apply_bundle = sub.add_parser(
        "apply-bundle", help="delete one bundle side, validating each unit"
    )
    apply_bundle.add_argument("--bundle", required=True)
    apply_bundle.add_argument("--sha256", required=True)
    apply_bundle.add_argument("--side", choices=("sender", "receiver"), required=True)
    apply_bundle.add_argument("--approval", required=True)
    apply_bundle.set_defaults(func=command_apply_bundle)
    return result


def main(argv=None):
    args = parser().parse_args(argv)
    try:
        args.func(args)
    except ReconcileError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
