"""Thinkpad1's deployment boundary; no keys are handled by this process."""

import fcntl
import hashlib
import json
import os
import struct
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from uuid import UUID

ESP = Path("/boot")
PROFILE = Path("/nix/var/nix/profiles/system")
EFI = Path("/sys/firmware/efi/efivars")
GLOBAL_GUID = "8be4df61-93ca-11d2-aa0d-00e098032b8c"
LOADER_GUID = "4a67b082-0a4c-41cf-b6c7-440b29bb8c4f"
CACHE = Path("/run/thinkpad1-tpm-deploy/measurements.json")
TIMINGS = {}


def run(*args, env=None, timing=None):
    started = time.monotonic()
    try:
        return subprocess.check_output(
            [str(arg) for arg in args], text=True, stdin=subprocess.DEVNULL, env=env
        )
    finally:
        if timing:
            TIMINGS[timing] = TIMINGS.get(timing, 0.0) + time.monotonic() - started


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def sha(data):
    return hashlib.sha256(data).digest()


def file_hash(path):
    with open(path, "rb") as stream:
        return hashlib.file_digest(stream, "sha256").digest()


def extend(digests):
    result = bytes(32)
    for digest in digests:
        result = sha(result + digest)
    return result.hex()


def policy_hash(pcrs):
    # TPM2 PolicyPCR wire encoding, matching systemd's tpm2_calculate_policy_pcr:
    # SHA256 bank, PCR selection 4+9, no PIN or additional policy.
    return sha(
        bytes(32)
        + bytes.fromhex("0000017f00000001000b03100200")
        + sha(bytes.fromhex(pcrs[4] + pcrs[9]))
    ).hex()


def firmware_target(option):
    # UEFI EFI_LOAD_OPTION, restricted to systemd/efibootmgr's GPT HD/File/End
    # short-form path. Full hardware paths, optional arguments, and extra nodes
    # are deliberately unsupported rather than guessed.
    error = "unsupported firmware boot option layout"
    require(len(option) >= 8, error)
    attributes, path_length = struct.unpack_from("<IH", option)
    require(attributes & 1, "firmware boot entry is inactive")
    require(attributes == 1, error)
    path_start = next(
        (i + 2 for i in range(6, len(option) - 1, 2) if option[i : i + 2] == b"\0\0"),
        len(option),
    )
    require(path_start + path_length == len(option), error)
    path = option[path_start:]
    require(len(path) >= 52 and path[:4] == b"\x04\x01\x2a\x00", error)
    number, start, size, signature, mbr_type, signature_type = struct.unpack_from(
        "<IQQ16sBB", path, 4
    )
    require(mbr_type == 2 and signature_type == 2, error)
    file_node = path[42:]
    require(file_node[:2] == b"\x04\x04", error)
    length = int.from_bytes(file_node[2:4], "little")
    require(
        length >= 6
        and length % 2 == 0
        and file_node[length:] == b"\x7f\xff\x04\x00"
        and file_node[length - 2 : length] == b"\0\0",
        error,
    )
    filename = file_node[4 : length - 2].decode("utf-16-le")
    return UUID(bytes_le=signature), filename, (number, start, size)


def firmware_check():
    def variable(name, guid=GLOBAL_GUID):
        return (EFI / f"{name}-{guid}").read_bytes()[4:]

    require(
        not (EFI / f"BootNext-{GLOBAL_GUID}").exists(),
        "BootNext overrides permanent boot order",
    )
    current = variable("BootCurrent")
    order = variable("BootOrder")
    require(
        len(current) == 2
        and len(order) >= 2
        and len(order) % 2 == 0
        and current == order[:2],
        "current boot is not first in BootOrder",
    )
    option = variable(f"Boot{int.from_bytes(current, 'little'):04X}")
    partition_id, target, geometry = firmware_target(option)
    image = (
        variable("LoaderImageIdentifier", LOADER_GUID).decode("utf-16-le").rstrip("\0")
    )
    require(
        image.replace("/", "\\").lower() == "\\efi\\systemd\\systemd-bootx64.efi",
        f"only direct Linux firmware boot is supported (observed {image!r})",
    )
    observed_partition = UUID(
        variable("LoaderDevicePartUUID", LOADER_GUID).decode("utf-16-le").rstrip("\0")
    )
    require(
        partition_id == observed_partition
        and target.lower() == image.replace("/", "\\").lower(),
        "firmware boot target differs from observed direct boot",
    )
    device = (Path("/dev/disk/by-partuuid") / str(partition_id)).stat().st_rdev
    require(
        device == ESP.stat().st_dev,
        "mounted ESP differs from firmware boot partition",
    )
    mounts = [
        line.split()
        for line in Path("/proc/self/mountinfo").read_text().splitlines()
        if line.split()[4] == str(ESP)
    ]
    require(
        len(mounts) == 1
        and mounts[0][3] == "/"
        and mounts[0][mounts[0].index("-") + 1] == "vfat",
        "mounted ESP must expose a FAT partition root without stacked mounts",
    )
    partition = Path(f"/sys/dev/block/{os.major(device)}:{os.minor(device)}").resolve(
        strict=True
    )
    sector_size = int((partition.parent / "queue/logical_block_size").read_text())
    actual_geometry = (
        int((partition / "partition").read_text()),
        int((partition / "start").read_text()) * 512 // sector_size,
        int((partition / "size").read_text()) * 512 // sector_size,
    )
    require(geometry == actual_geometry, "firmware partition geometry differs from ESP")
    for name in (
        "LoaderEntryDefault",
        "LoaderEntryPreferred",
        "LoaderEntryOneShot",
        "LoaderEntrySysFail",
    ):
        require(
            not (EFI / f"{name}-{LOADER_GUID}").exists(),
            f"{name} overrides loader.conf",
        )
    loader_config()


def bootspec(system):
    document = json.loads((system / "boot.json").read_text())
    spec = document["org.nixos.bootspec.v1"]
    require(spec["system"] == "x86_64-linux", "unsupported architecture")
    require(not spec.get("initrdSecrets"), "mutable initrd secrets are unsupported")
    require(
        not document.get("org.nixos.extra-initrd.v1", {}).get("paths"),
        "multiple initrds are unsupported",
    )
    require(
        not document.get("org.nixos.systemd-boot", {}).get("devicetree"),
        "device trees are unsupported",
    )
    require(
        spec["init"] == str(system / "init"),
        "bootspec init does not identify candidate",
    )
    require(
        spec["kernelParams"]
        and all(
            param.isprintable() and param == param.strip()
            for param in spec["kernelParams"]
        ),
        "unsupported kernel parameter encoding",
    )
    return spec


def esp_path(source):
    source = Path(source).resolve(strict=True)
    root = source.relative_to("/nix/store").parts[0]
    name = source.name if root == source.name else f"{root}-{source.name}"
    return Path("EFI/nixos") / f"{name}.efi"


def command_line(spec):
    initrd = "\\" + str(esp_path(spec["initrd"])).replace("/", "\\")
    return f"initrd={initrd} init={spec['init']} " + " ".join(spec["kernelParams"])


def pcrlock(*args):
    return json.loads(
        run(
            os.environ["TPM_DEPLOY_PCRLOCK"],
            *args,
            env=os.environ | {"SYSTEMD_LOG_LEVEL": "err"},
        )
    )


def pe_hash(path):
    records = pcrlock("--pcrlock=-", "lock-pe", path)["records"]
    require(len(records) == 1 and records[0]["pcr"] == 4, "unexpected PE measurement")
    return next(d["digest"] for d in records[0]["digests"] if d["hashAlg"] == "sha256")


def measurement_cache(pcrs):
    identity = [
        1,
        Path("/proc/sys/kernel/random/boot_id").read_text(),
        pcrs[4],
        pcrs[9],
        os.environ["TPM_DEPLOY_PCRLOCK"],
    ]
    CACHE.parent.mkdir(mode=0o700, exist_ok=True)
    require(
        CACHE.parent.stat().st_uid == 0 and CACHE.parent.stat().st_mode & 0o077 == 0,
        "measurement cache directory must be root-only",
    )
    if CACHE.exists():
        cache = json.loads(CACHE.read_text())
        if cache.get("identity") == identity:
            return cache
    return {
        "identity": identity,
        "digests": {},
        "events": [
            e for e in pcrlock("--json=short", "log")["log"] if e["pcr"] in (4, 9)
        ],
    }


def immutable_digest(path, cache, *, pe=False):
    path = Path(path).resolve(strict=True)
    require(
        path.is_relative_to("/nix/store"),
        "only immutable store measurements may be cached",
    )
    key = ("pe:" if pe else "sha256:") + str(path)
    if key not in cache["digests"]:
        cache["digests"][key] = pe_hash(path) if pe else file_hash(path).hex()
    return cache["digests"][key]


def matching_tokens(metadata, policy):
    return [
        key
        for key, token in metadata["tokens"].items()
        if token.get("type") == "systemd-tpm2"
        and token.get("tpm2-pcrs") == [4, 9]
        and token.get("tpm2-pcr-bank") == "sha256"
        and token.get("tpm2-policy-hash") == policy
        and not token.get("tpm2-pin", False)
        and len(token.get("keyslots", [])) == 1
        and token["keyslots"][0] in metadata["keyslots"]
    ]


def metadata(device):
    return json.loads(run("cryptsetup", "luksDump", "--dump-json-metadata", device))


def check_capacity(luks, token_id):
    token = luks["tokens"][token_id]
    slot = luks["keyslots"][token["keyslots"][0]]

    def compact(value):
        return json.dumps(value, separators=(",", ":"))

    # Reserve a comparable token/keyslot plus serialization headroom. The native
    # enrollment operation remains the final authority on allocation success.
    json_size = int(luks["config"]["json_size"])
    needed_json = len(compact([token, slot])) + 1024
    require(
        len(luks["keyslots"]) < 32
        and len(luks["tokens"]) < 32
        and json_size - len(compact(luks)) >= needed_json,
        "insufficient LUKS enrollment capacity: no safe header headroom; existing credentials retained",
    )
    # The two LUKS2 metadata copies precede the keyslot area. Check contiguous
    # space as well as JSON space; free slot numbers alone are not sufficient.
    cursor = 2 * (4096 + json_size)
    limit = cursor + int(luks["config"]["keyslots_size"])
    gaps = []
    areas = sorted(
        (int(s["area"]["offset"]), int(s["area"]["size"]))
        for s in luks["keyslots"].values()
    )
    for offset, size in areas:
        require(
            cursor <= offset and offset + size <= limit,
            "unsupported LUKS keyslot layout",
        )
        gaps.append(offset - cursor)
        cursor = offset + size
    gaps.append(limit - cursor)
    require(
        max(gaps) >= int(slot["area"]["size"]),
        "insufficient LUKS enrollment capacity: keyslot area full; existing credentials retained",
    )


def bls_fields(path):
    text = path.read_text()
    require(
        all(char.isprintable() or char in "\t\r\n" for char in text),
        "unsupported BLS text encoding",
    )
    fields = {}
    for line in text.splitlines():
        line = line.strip(" \t")
        if not line or line.startswith("#"):
            continue
        parts = line.split(maxsplit=1)
        if len(parts) != 2:
            continue
        key, value = parts
        # Match systemd-boot's outer-quote removal, preserving argument quotes.
        if value[0] in "\"'" and value[-1] == value[0]:
            value = value[1:-1]
        fields.setdefault(key, []).append(value)
    return fields


def loader_config():
    fields = bls_fields(ESP / "loader/loader.conf")
    allowed = {
        "default",
        "timeout",
        "console-mode",
        "editor",
        "auto-entries",
        "auto-firmware",
        "auto-poweroff",
        "auto-reboot",
        "beep",
        "random-seed-mode",
        "reboot-for-bitlocker",
    }
    for key in fields:
        require(key in allowed, f"unsupported loader directive: {key}")
    require(
        fields.get("reboot-for-bitlocker", ["no"]) == ["no"],
        "BitLocker reboot handling is unsupported",
    )
    defaults = fields.get("default", [])
    require(
        len(defaults) == 1 and len(defaults[0].split()) == 1,
        "no explicit boot default",
    )
    return fields


def entry_for(spec, *, default=False):
    options = f"init={spec['init']} " + " ".join(spec["kernelParams"])
    entries = ESP / "loader/entries"
    if default:
        name = loader_config()["default"][0]
        require(
            Path(name).name == name and name.endswith(".conf"),
            "unsupported boot default",
        )
        paths = [entries / name]
    else:
        paths = sorted(entries.glob("*.conf"))
    for path in paths:
        fields = bls_fields(path)
        if default or options in fields.get("options", []):
            allowed = {
                "title",
                "version",
                "sort-key",
                "machine-id",
                "architecture",
                "options",
                "linux",
                "initrd",
            }
            for key in fields:
                require(key in allowed, f"unsupported boot entry directive: {key}")
            require(
                fields.get("architecture", ["x64"]) == ["x64"],
                "unsupported boot entry architecture",
            )
            require(
                fields.get("options") == [options],
                "ambiguous boot options or unexpected arguments",
            )
            require(
                fields.get("linux") == [f"/{esp_path(spec['kernel'])}"],
                "installed kernel path mismatch",
            )
            require(
                fields.get("initrd") == [f"/{esp_path(spec['initrd'])}"],
                "installed initrd mismatch",
            )
            return path
    raise RuntimeError("installed boot entry missing")


def atomic_write(path, data):
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as stream:
        temporary = Path(stream.name)
        stream.write(data)
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(temporary, path)
    descriptor = os.open(path.parent, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


STATE = Path("/var/lib/thinkpad1-tpm-deploy/state.json")


def fingerprint(value):
    return sha(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hex()


def save_state(state):
    atomic_write(STATE, (json.dumps(state, sort_keys=True) + "\n").encode())


def load_state(device, luks):
    STATE.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    require(not STATE.parent.is_symlink(), "state directory must not be a symlink")
    identity = run("cryptsetup", "luksUUID", device).strip()
    recovery = fingerprint(luks["keyslots"]["0"])
    if STATE.exists():
        state = json.loads(STATE.read_text())
        require(
            state["version"] == 1 and state["uuid"] == identity,
            "enrollment state belongs to another disk or version",
        )
        require(
            state["recovery"] == recovery,
            "recovery slot changed; manual reconciliation required",
        )
    else:
        state = {
            "version": 1,
            "uuid": identity,
            "recovery": recovery,
            "owned": {},
            "confirmed": [],
            "systems": {},
            "intent": None,
        }
    # Enrollment can finish before the process records its token. Only adopt the
    # unique matching token absent from the durable pre-enrollment snapshot.
    intent = state["intent"]
    if intent:
        created = [
            key
            for key in matching_tokens(luks, intent["policy"])
            if key not in intent["tokens_before"]
        ]
        require(len(created) <= 1, "ambiguous interrupted enrollment")
        if created:
            key = created[0]
            token = luks["tokens"][key]
            slot = token["keyslots"][0]
            require(
                slot != "0" and slot not in intent["slots_before"],
                "new token reused a pre-existing slot",
            )
            state["owned"][intent["policy"]] = {
                "token": key,
                "slot": slot,
                "system": intent["system"],
                "token_hash": fingerprint(token),
                "slot_hash": fingerprint(luks["keyslots"][slot]),
            }
        else:
            require(
                not (set(luks["keyslots"]) - set(intent["slots_before"])),
                "interrupted enrollment left unclassified keyslots; manual reconciliation required",
            )
        state["intent"] = None
    save_state(state)
    return state


def retain_systems(state, keep):
    # Native system-profiles are GC roots and are included by the boot installer.
    # Keep one generation per policy; never modify the human's main profile here.
    directory = Path("/nix/var/nix/profiles/system-profiles")
    directory.mkdir(parents=True, exist_ok=True)
    for policy, system in list(state["systems"].items()):
        require(
            len(policy) == 64 and all(c in "0123456789abcdef" for c in policy),
            "invalid policy name",
        )
        profile = directory / f"thinkpad1-tpm-{policy}"
        generation = directory / f"thinkpad1-tpm-{policy}-1-link"
        if policy in keep:
            if not profile.exists():
                run("nix-env", "--profile", profile, "--set", system)
            require(
                profile.resolve(strict=True) == Path(system),
                "managed rollback profile changed",
            )
        else:
            for path in (profile, generation):
                if path.is_symlink():
                    require(
                        path.resolve() == Path(system),
                        "managed rollback profile changed; refusing removal",
                    )
                    path.unlink()
                else:
                    require(not path.exists(), "unexpected managed profile file")
            del state["systems"][policy]
    save_state(state)


def retire_pending(device, state, keep):
    for policy, record in list(state["owned"].items()):
        if policy in keep:
            continue
        luks = metadata(device)
        key, slot = record["token"], record["slot"]
        if key not in luks["tokens"] and slot not in luks["keyslots"]:
            del state["owned"][policy]
            save_state(state)
            continue
        require(slot != "0", "refusing to retire recovery slot")
        require(
            fingerprint(luks["tokens"].get(key)) == record["token_hash"],
            "owned token changed; refusing retirement",
        )
        require(
            fingerprint(luks["keyslots"].get(slot)) == record["slot_hash"],
            "owned slot changed; refusing retirement",
        )
        require(
            all(
                other == key or slot not in token.get("keyslots", [])
                for other, token in luks["tokens"].items()
            ),
            "owned slot is shared with another token",
        )
        run("systemd-cryptenroll", f"--wipe-slot={slot}", device)
        after = metadata(device)
        require(
            slot not in after["keyslots"] and key not in after["tokens"],
            "retirement incomplete",
        )
        require(
            fingerprint(after["keyslots"]["0"]) == state["recovery"],
            "recovery slot changed during retirement",
        )
        del state["owned"][policy]
        save_state(state)


def deploy(action, candidate):
    firmware_check()
    booted = Path("/run/booted-system").resolve(strict=True)
    old, new = bootspec(booted), bootspec(candidate)
    pcrs = {
        p["nr"]: p["sha256"]
        for p in json.loads(
            run("systemd-analyze", "pcrs", "4", "9", "--json=short", timing="pcr-read")
        )
    }
    artifact_started = time.monotonic()
    TIMINGS["enrollment"] = 0.0
    cache = measurement_cache(pcrs)
    for key in ("kernel", "initrd"):
        require(
            Path(old[key]).resolve() == Path(new[key]).resolve(),
            f"changed {key} not supported yet; default unchanged",
        )
        require(
            immutable_digest(old[key], cache)
            == file_hash(ESP / esp_path(old[key])).hex(),
            f"installed {key} differs from immutable source",
        )
    loader = booted / "systemd/lib/systemd/boot/efi/systemd-bootx64.efi"
    require(
        immutable_digest(loader, cache)
        == immutable_digest(
            candidate / "systemd/lib/systemd/boot/efi/systemd-bootx64.efi", cache
        ),
        "changed bootloader not supported yet",
    )
    require(
        immutable_digest(loader, cache)
        == file_hash(ESP / "EFI/systemd/systemd-bootx64.efi").hex(),
        "installed bootloader differs from booted source",
    )
    require(
        command_line(old) == Path("/proc/cmdline").read_text().strip(),
        "unsupported boot argument encoding",
    )
    events = cache["events"]
    events4 = [e for e in events if e["pcr"] == 4]
    events9 = [e for e in events if e["pcr"] == 9]
    require(
        extend(bytes.fromhex(e["sha256"]) for e in events4) == pcrs[4],
        "PCR4 event replay mismatch",
    )
    images = [
        e["sha256"] for e in events4 if e["event"] == "efi-boot-services-application"
    ]
    require(
        images[-2:]
        == [
            immutable_digest(loader, cache, pe=True),
            immutable_digest(old["kernel"], cache, pe=True),
        ],
        "booted EFI images do not match sources",
    )
    old_digests = [
        sha((command_line(old) + "\0").encode("utf-16-le")),
        bytes.fromhex(immutable_digest(old["initrd"], cache)),
    ]
    require(
        [e["sha256"] for e in events9] == [d.hex() for d in old_digests],
        "unsupported PCR9 event sequence",
    )
    require(
        extend(old_digests) == pcrs[9], "PCR9 prediction does not match current boot"
    )
    cached_bytes = json.dumps(cache, sort_keys=True).encode()
    if not CACHE.exists() or CACHE.read_bytes() != cached_bytes:
        atomic_write(CACHE, cached_bytes)
    TIMINGS["artifact-and-event-validation"] = time.monotonic() - artifact_started
    device = os.environ["TPM_DEPLOY_DEVICE"]
    before = metadata(device)
    require("0" in before["keyslots"], "original recovery slot missing")
    current_tokens = matching_tokens(before, policy_hash(pcrs))
    require(
        current_tokens,
        "no bound TPM enrollment covers current boot; explicit recovery required",
    )
    run(
        "cryptsetup",
        "open",
        "--test-passphrase",
        "--token-only",
        "--token-id",
        current_tokens[0],
        device,
        timing="authorization",
    )
    state = load_state(device, before)
    current_policy = policy_hash(pcrs)
    state["confirmed"] = (
        [current_policy] + [p for p in state["confirmed"] if p != current_policy]
    )[:2]
    state["systems"][current_policy] = str(booted)
    save_state(state)
    future = {
        4: pcrs[4],
        9: extend(
            [sha((command_line(new) + "\0").encode("utf-16-le")), old_digests[1]]
        ),
    }
    expected = policy_hash(future)
    if not matching_tokens(before, expected):
        require(
            not any(
                token.get("type") == "systemd-tpm2"
                and token.get("tpm2-pin")
                and token.get("keyslots")
                for token in before["tokens"].values()
            ),
            "PIN-protected disk tokens are unsupported for unattended enrollment; no PIN will be requested",
        )
        check_capacity(before, current_tokens[0])
        state["intent"] = {
            "policy": expected,
            "system": str(candidate),
            "tokens_before": list(before["tokens"]),
            "slots_before": list(before["keyslots"]),
        }
        save_state(state)
        print("Preparing next-boot TPM enrollment", flush=True)
        run(
            "systemd-cryptenroll",
            "--unlock-tpm2-device=auto",
            "--tpm2-device=auto",
            "--tpm2-with-pin=no",
            f"--tpm2-pcrs=4:sha256={future[4]}+9:sha256={future[9]}",
            device,
            timing="enrollment",
        )
    after = metadata(device)
    require(
        after["keyslots"]["0"] == before["keyslots"]["0"],
        "recovery slot metadata changed",
    )
    require(
        matching_tokens(after, expected),
        "candidate enrollment absent after preparation",
    )
    state = load_state(device, after)
    state["systems"][expected] = str(candidate)
    keep = set(state["confirmed"]) | {expected}
    retain_systems(state, set(state["systems"]))
    loader_conf = ESP / "loader/loader.conf"
    previous_conf = loader_conf.read_bytes()
    previous_profile = PROFILE.resolve(strict=True)
    rollback_entry = entry_for(old)
    rollback_contents = rollback_entry.read_bytes()
    try:
        run("nix-env", "--profile", PROFILE, "--set", candidate)
        print(
            run(
                candidate / "bin/switch-to-configuration",
                action,
                timing="boot-installation",
            ),
            end="",
        )
        entry_for(new, default=True)
        for key in ("kernel", "initrd"):
            require(
                immutable_digest(new[key], cache)
                == file_hash(ESP / esp_path(new[key])).hex(),
                f"installed {key} failed verification",
            )
        require(
            immutable_digest(loader, cache)
            == file_hash(ESP / "EFI/systemd/systemd-bootx64.efi").hex(),
            "bootloader changed during installation",
        )
        firmware_check()
    except BaseException:
        atomic_write(loader_conf, previous_conf)
        atomic_write(rollback_entry, rollback_contents)
        run("nix-env", "--profile", PROFILE, "--set", previous_profile)
        raise
    if not rollback_entry.exists():
        atomic_write(rollback_entry, rollback_contents)
    retire_pending(device, state, keep)
    retain_systems(state, keep)
    print("TPM enrollment covers installed next boot; physical boot validation pending")


def main():
    require(os.geteuid() == 0, "must run as root")
    require(len(sys.argv) == 3, "usage: thinkpad1-tpm-deploy ACTION CANDIDATE")
    action, argument = sys.argv[1:]
    require(action in ("boot", "switch", "test", "dry-activate"), "unsupported action")
    candidate = Path(argument).resolve(strict=True)
    require(
        candidate.parent == Path("/nix/store"),
        "candidate must be an immutable system closure",
    )
    with open("/run/thinkpad1-tpm-deploy.lock", "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if action in ("test", "dry-activate"):
            print(run(candidate / "bin/switch-to-configuration", action), end="")
            print(f"TPM enrollment skipped for {action}; boot profile unchanged")
        else:
            deploy(action, candidate)


if __name__ == "__main__":
    try:
        main()
    except (
        RuntimeError,
        OSError,
        subprocess.CalledProcessError,
        ValueError,
        KeyError,
    ) as error:
        print(f"TPM deployment failed: {error}", file=sys.stderr)
        sys.exit(1)
    finally:
        if TIMINGS:
            print(
                "TPM timings (seconds): "
                + json.dumps(
                    {key: round(value, 4) for key, value in TIMINGS.items()},
                    sort_keys=True,
                )
            )
