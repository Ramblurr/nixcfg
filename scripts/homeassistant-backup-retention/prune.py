import argparse
import collections
import datetime
import json
import os
import tarfile
import time

RECENT_SECONDS = 3600

# Each tier keeps the newest backup in each of its `count` most recent
# non-empty buckets. A count of None keeps every bucket. Buckets use the
# host's local time.
TIERS = (
    ("daily", lambda t: t.date(), 7),
    ("weekly", lambda t: t.isocalendar()[:2], 4),
    ("monthly", lambda t: (t.year, t.month), 12),
    ("yearly", lambda t: t.year, None),
)

Backup = collections.namedtuple("Backup", "mtime name size")


def backups(directory):
    for entry in os.scandir(directory):
        if entry.name.endswith(".tar") and entry.is_file(
            follow_symlinks=False
        ):
            st = entry.stat(follow_symlinks=False)
            yield Backup(st.st_mtime, entry.name, st.st_size)


def is_system_backup(path):
    """A complete backup that carries Home Assistant core.

    Add-on update backups have no core. A truncated upload lacks the
    end-of-archive marker, two zero blocks after the last member, even when
    it was cut exactly at a member boundary.
    """
    try:
        with tarfile.open(path, "r:") as tar:
            members = tar.getmembers()
            end = tar.offset
            names = {os.path.normpath(m.name): m for m in members}
            meta = json.load(tar.extractfile(names["backup.json"]))
        with open(path, "rb") as f:
            f.seek(end)
            marker = f.read(2 * tarfile.BLOCKSIZE)
        return (
            meta.get("homeassistant") is not None
            and any(n.startswith("homeassistant.tar") for n in names)
            and marker == tarfile.NUL * (2 * tarfile.BLOCKSIZE)
        )
    except (
        OSError,
        tarfile.TarError,
        ValueError,
        AttributeError,
        KeyError,
        IndexError,
    ):
        return False


def keepers(newest_first):
    kept = collections.defaultdict(list)
    for tier, bucket_of, count in TIERS:
        newest = {}
        for b in newest_first:
            local = datetime.datetime.fromtimestamp(b.mtime)
            newest.setdefault(bucket_of(local), b)
        for b in list(newest.values())[:count]:
            kept[b.name].append(tier)
    return kept


def stamp(b):
    return datetime.datetime.fromtimestamp(b.mtime).isoformat(
        timespec="seconds"
    )


def main():
    parser = argparse.ArgumentParser(
        description="Prune Home Assistant backup tarballs to a "
        "daily/weekly/monthly/yearly set."
    )
    parser.add_argument("directory")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--now",
        type=datetime.datetime.fromisoformat,
        help="local ISO 8601 time to treat as now (for tests)",
    )
    args = parser.parse_args()
    now = args.now.timestamp() if args.now else time.time()

    files = sorted(backups(args.directory), reverse=True)
    recent = [b for b in files if b.mtime > now - RECENT_SECONDS]
    older = [b for b in files if b.mtime <= now - RECENT_SECONDS]
    settled = [
        b
        for b in older
        if is_system_backup(os.path.join(args.directory, b.name))
    ]
    other = [b for b in older if b not in settled]
    kept = keepers(settled)
    verb = "would-delete" if args.dry_run else "delete"
    totals = collections.Counter()

    for b in recent:
        print(f"skip {b.name} {stamp(b)} modified within the last hour")
        totals["skip"] += 1
    for b in other:
        print(f"skip {b.name} {stamp(b)} not a Home Assistant system backup")
        totals["skip_other"] += 1
    for b in settled:
        if b.name in kept:
            print(f"keep {b.name} {stamp(b)} {','.join(kept[b.name])}")
            totals["keep"] += 1
            totals["keep_bytes"] += b.size
        else:
            print(f"{verb} {b.name} {stamp(b)}")
            if not args.dry_run:
                os.unlink(os.path.join(args.directory, b.name))
            totals["delete"] += 1
            totals["delete_bytes"] += b.size

    print(
        f"summary kept={totals['keep']} kept_bytes={totals['keep_bytes']} "
        f"{verb}={totals['delete']} {verb}_bytes={totals['delete_bytes']} "
        f"skipped_recent={totals['skip']} skipped_other={totals['skip_other']}"
    )


if __name__ == "__main__":
    main()
