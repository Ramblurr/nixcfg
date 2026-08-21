import importlib.util
import json
import subprocess
import tempfile
import unittest
from pathlib import Path

MODULE_PATH = Path(__file__).parents[1] / "fix_zrepl.py"
SPEC = importlib.util.spec_from_file_location("fix_zrepl", MODULE_PATH)
fix_zrepl = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(fix_zrepl)

SENDER = "rpool/encrypted/safe/extra/atuin"
RECEIVER = "tank2/replication/debord/" + SENDER


def version(dataset, suffix, kind, guid, creation):
    separator = "@" if kind == "snapshot" else "#"
    return {
        "name": f"{dataset}{separator}{suffix}",
        "type": kind,
        "guid": guid,
        "creation": creation,
    }


def inventory(host, dataset, versions):
    snapshots = [item["name"] for item in versions if item["type"] == "snapshot"]
    return {
        "host": host,
        "dataset": dataset,
        "exists": True,
        "guid": "dataset-guid",
        "receive_resume_token": "absent",
        "versions": versions,
        "userrefs": {name: 0 for name in snapshots},
        "holds": {name: [] for name in snapshots},
        "abstractions": [],
    }


class ReconcilePlanTest(unittest.TestCase):
    def setUp(self):
        self.sender_versions = [
            version(SENDER, "zrepl_common", "snapshot", "common", 1),
            version(SENDER, "manual-old", "snapshot", "manual", 2),
            version(SENDER, "zrepl_old", "snapshot", "old", 3),
            version(SENDER, "zrepl_cutoff", "snapshot", "cutoff", 60),
            version(SENDER, "zrepl_new", "snapshot", "new", 90),
        ]
        self.receiver_versions = [
            version(RECEIVER, "zrepl_common", "snapshot", "common", 1),
            version(RECEIVER, "zrepl_diverged_1", "snapshot", "diverged-1", 4),
            version(RECEIVER, "zrepl_diverged_2", "snapshot", "diverged-2", 5),
            version(RECEIVER, "zrepl_cursor", "bookmark", "diverged-2", 5),
        ]
        self.sender = inventory("debord", SENDER, self.sender_versions)
        self.receiver = inventory("mali", RECEIVER, self.receiver_versions)

    def plan(self):
        return fix_zrepl.derive_plan(self.sender, self.receiver, "1970-01-01T00:01:00Z")

    def test_derives_exact_sender_retention_and_receiver_tail(self):
        plan = self.plan()
        self.assertEqual(
            {
                "mode": "proven-common",
                "common": self.receiver_versions[0],
                "common_sender": self.sender_versions[0],
                "sender_delete": [self.sender_versions[2]],
                "receiver_delete": self.receiver_versions[1:3],
                "preserved_receiver_bookmarks": [self.receiver_versions[3]],
            },
            {
                key: plan[key]
                for key in (
                    "mode",
                    "common",
                    "common_sender",
                    "sender_delete",
                    "receiver_delete",
                    "preserved_receiver_bookmarks",
                )
            },
        )

    def test_initial_receive_prunes_old_sender_without_receiver_mutation(self):
        receiver = {"host": "mali", "dataset": RECEIVER, "exists": False}
        plan = fix_zrepl.derive_plan(self.sender, receiver, "1970-01-01T00:01:00Z")
        self.assertEqual(
            {
                "mode": "initial-receive",
                "common": None,
                "sender_delete": [self.sender_versions[0], self.sender_versions[2]],
                "receiver_delete": [],
            },
            {
                key: plan[key]
                for key in ("mode", "common", "sender_delete", "receiver_delete")
            },
        )

    def test_held_receiver_tail_fails_closed(self):
        target = self.receiver_versions[1]["name"]
        self.receiver["userrefs"][target] = 1
        self.receiver["holds"][target] = ["zrepl_step"]
        with self.assertRaisesRegex(
            fix_zrepl.ReconcileError, "receiver-only snapshot is held"
        ):
            self.plan()

    def test_held_sender_prune_target_fails_closed(self):
        target = self.sender_versions[2]["name"]
        self.sender["userrefs"][target] = 1
        self.sender["holds"][target] = ["keep"]
        with self.assertRaisesRegex(
            fix_zrepl.ReconcileError, "old sender snapshot is held"
        ):
            self.plan()

    def test_existing_receiver_without_common_guid_fails_closed(self):
        self.receiver["versions"][0]["guid"] = "not-common"
        with self.assertRaisesRegex(fix_zrepl.ReconcileError, "no common GUID"):
            self.plan()

    def test_wrong_mapping_fails_closed(self):
        self.receiver["dataset"] += "-wrong"
        with self.assertRaisesRegex(
            fix_zrepl.ReconcileError, "receiver mapping mismatch"
        ):
            self.plan()

    def test_present_resume_token_fails_closed(self):
        self.receiver["receive_resume_token"] = "present-redacted"
        with self.assertRaisesRegex(
            fix_zrepl.ReconcileError, "receive token must be absent"
        ):
            self.plan()

    def test_deleted_targets_must_be_an_exact_prefix(self):
        plan = self.plan()
        current = fix_zrepl.inventory_without_targets(
            self.receiver, [plan["receiver_delete"][1]["name"]]
        )
        with self.assertRaisesRegex(fix_zrepl.ReconcileError, "not an exact prefix"):
            fix_zrepl.deleted_prefix(plan, current, "receiver")

    def test_manifest_rederivation_rejects_injected_target(self):
        plan = self.plan()
        plan["sender_delete"].append(self.sender_versions[-1])
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "plan.json"
            path.write_text(json.dumps(plan))
            with self.assertRaisesRegex(
                fix_zrepl.ReconcileError, "targets do not match"
            ):
                fix_zrepl.load_plan(path)

    def test_manifest_rederivation_accepts_generated_plan(self):
        plan = self.plan()
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "plan.json"
            path.write_text(json.dumps(plan))
            self.assertEqual(plan, fix_zrepl.load_plan(path))

    def test_generated_batch_script_is_valid_shell(self):
        plan = self.plan()
        target = dict(plan["sender_delete"][0])
        target["guid"] = "123"
        script = fix_zrepl.build_destroy_script(
            plan["sender"], [target], plan["common_sender"]
        )
        result = subprocess.run(
            ["/bin/sh", "-n"], input=script, text=True, capture_output=True, check=False
        )
        self.assertEqual(
            {"returncode": 0, "stderr": "", "destroy_count": 1},
            {
                "returncode": result.returncode,
                "stderr": result.stderr,
                "destroy_count": script.count("sudo zfs destroy"),
            },
        )

    def test_generated_batch_rejects_out_of_scope_target(self):
        plan = self.plan()
        target = dict(plan["sender_delete"][0])
        target["name"] = "rpool/encrypted/safe/persist@zrepl_wrong"
        with self.assertRaisesRegex(fix_zrepl.ReconcileError, "unsafe batch target"):
            fix_zrepl.build_destroy_script(
                plan["sender"], [target], plan["common_sender"]
            )

    def test_bundle_validates_embedded_units(self):
        plan = self.plan()
        bundle = {
            "schema": fix_zrepl.BUNDLE_SCHEMA,
            "cutoff": plan["cutoff"],
            "units": [plan],
        }
        self.assertEqual(bundle, fix_zrepl.validate_bundle(bundle))

    def test_bundle_rejects_duplicate_units(self):
        plan = self.plan()
        bundle = {
            "schema": fix_zrepl.BUNDLE_SCHEMA,
            "cutoff": plan["cutoff"],
            "units": [plan, plan],
        }
        with self.assertRaisesRegex(
            fix_zrepl.ReconcileError, "duplicate sender datasets"
        ):
            fix_zrepl.validate_bundle(bundle)


if __name__ == "__main__":
    unittest.main()
