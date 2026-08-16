{ pkgs }:
pkgs.testers.runNixOSTest {
  name = "zrepl-outage-continuity";

  nodes = {
    source =
      { nodes, pkgs, ... }:
      {
        virtualisation.emptyDiskImages = [ 1024 ];
        virtualisation.memorySize = 1536;
        boot.supportedFilesystems = [ "zfs" ];
        networking.hostId = "03600001";
        networking.firewall.allowedTCPPorts = [ 8888 ];
        environment.systemPackages = [
          pkgs.parted
          pkgs.zrepl
        ];

        services.zrepl = {
          enable = true;
          settings.jobs = [
            {
              name = "mali_snap";
              type = "snap";
              filesystems."tank/data" = true;
              snapshotting = {
                type = "periodic";
                prefix = "zrepl_";
                interval = "5s";
              };
              pruning.keep = [
                {
                  type = "regex";
                  regex = ".*";
                }
              ];
            }
            {
              name = "mali_rsyncnet";
              type = "source";
              serve = {
                type = "tcp";
                listen = ":8888";
                clients = {
                  "${nodes.target.networking.primaryIPAddress}" = "target";
                  "${nodes.target.networking.primaryIPv6Address}" = "target";
                };
              };
              filesystems."tank/data" = true;
              snapshotting.type = "manual";
            }
          ];
        };
      };

    target =
      { pkgs, ... }:
      {
        virtualisation.emptyDiskImages = [ 1024 ];
        virtualisation.memorySize = 1536;
        boot.supportedFilesystems = [ "zfs" ];
        networking.hostId = "03600002";
        environment.systemPackages = [
          pkgs.parted
          pkgs.zrepl
        ];

        services.zrepl = {
          enable = true;
          settings.jobs = [
            {
              name = "pull_mali";
              type = "pull";
              connect = {
                type = "tcp";
                address = "source:8888";
              };
              root_fs = "tank/zrepl/source";
              interval = "1h";
              recv.placeholder.encryption = "off";
              pruning = {
                keep_sender = [ { type = "not_replicated"; } ];
                keep_receiver = [
                  {
                    type = "regex";
                    regex = ".*";
                  }
                ];
              };
            }
          ];
        };
      };
  };

  testScript = ''
    import json
    start_all()

    with subtest("create isolated source and receiver pools"):
        for machine in source, target:
            machine.wait_for_unit("multi-user.target")
            machine.systemctl("stop zrepl.service")
            machine.succeed("parted --script /dev/vdb mklabel gpt")
            machine.succeed("zpool create tank /dev/vdb")

        source.succeed("zfs create tank/data")
        target.succeed("zfs create -p tank/zrepl/source")
        source.systemctl("start zrepl.service")
        target.systemctl("start zrepl.service")
        source.wait_for_open_port(8888)
        target.wait_for_unit("zrepl.service")

    with subtest("establish the initial cursor and held receiver snapshot"):
        source.succeed("printf baseline > /tank/data/payload")
        source.succeed("zfs snapshot tank/data@zrepl_baseline")
        baseline_guid = source.succeed(
            "zfs list -H -p -o guid tank/data@zrepl_baseline"
        ).strip()
        target.succeed("zrepl signal wakeup pull_mali")
        target.wait_until_succeeds(
            "zfs list -H -t snapshot tank/zrepl/source/tank/data@zrepl_baseline"
        )
        source.wait_until_succeeds(
            "zfs list -H -t bookmark -o name tank/data | grep -q '#zrepl_CURSOR_.*_J_mali_rsyncnet'"
        )
        baseline_cursor = source.succeed(
            "zfs list -H -d 1 -t bookmark -o guid tank/data | tail -n 1"
        ).strip()
        receiver_dataset_guid = target.succeed(
            "zfs get -H -o value guid tank/zrepl/source/tank/data"
        ).strip()
        assert baseline_cursor == baseline_guid
        receiver_baseline_guid = target.succeed(
            "zfs list -H -p -o guid tank/zrepl/source/tank/data@zrepl_baseline"
        ).strip()
        assert receiver_baseline_guid == baseline_guid
        target.succeed(
            "zfs holds -H tank/zrepl/source/tank/data@zrepl_baseline | grep -q 'zrepl_last_received_J_pull_mali'"
        )

    with subtest("receiver absence leaves every queued recovery point intact"):
        target.systemctl("stop zrepl.service")
        target.fail("systemctl is-active --quiet zrepl.service")

        periodic_before = int(source.succeed(
            "zfs list -H -d 1 -t snapshot -o name tank/data | grep -c '@zrepl_'"
        ).strip())
        for index in range(1, 4):
            source.succeed(f"printf outage-{index} >> /tank/data/payload")
            source.succeed(f"zfs snapshot tank/data@zrepl_outage_{index}")

        source.wait_until_succeeds(
            "test $(zfs list -H -d 1 -t snapshot -o name tank/data | grep -c '@zrepl_') -ge "
            + str(periodic_before + 5),
            timeout=30,
        )
        source.sleep(6)
        for index in range(1, 4):
            source.succeed(f"zfs list -H -t snapshot tank/data@zrepl_outage_{index}")
        source.succeed(
            "zfs list -H -t bookmark -o guid tank/data | grep -qx " + baseline_cursor
        )

    with subtest("capacity pressure stops writes without emergency deletion"):
        source.succeed("zfs set quota=4M tank/data")
        source.fail("dd if=/dev/urandom of=/tank/data/capacity-pressure bs=1M count=16 conv=fsync")
        source.sleep(6)
        for index in range(1, 4):
            source.succeed(f"zfs list -H -t snapshot tank/data@zrepl_outage_{index}")
        source.succeed(
            "zfs list -H -t bookmark -o guid tank/data | grep -qx " + baseline_cursor
        )
        source.succeed("zfs set quota=none tank/data")
        retained_version_guids = dict(
            line.split("\t")
            for line in source.succeed(
                "zfs list -H -p -d 1 -t snapshot,bookmark -o name,guid tank/data"
            ).splitlines()
        )
        assert baseline_guid in retained_version_guids.values()

    with subtest("receiver return resumes the existing lineage incrementally"):
        target.systemctl("start zrepl.service")
        target.wait_for_unit("zrepl.service")
        target.succeed("zrepl signal wakeup pull_mali")
        for index in range(1, 4):
            target.wait_until_succeeds(
                f"zfs list -H -t snapshot tank/zrepl/source/tank/data@zrepl_outage_{index}",
                timeout=60,
            )

        target.wait_until_succeeds(
            "! zrepl status --mode dump | grep -E 'PLANNING-ERROR|REPLICATION-ERROR'",
            timeout=60,
        )
        target.wait_until_succeeds(
            "zrepl status --mode dump --job pull_mali | grep -q 'Status: done'",
            timeout=60,
        )
        status = json.loads(target.succeed("zrepl status --mode raw"))
        latest_attempt = status["Jobs"]["pull_mali"]["pull"]["Replication"]["Attempts"][-1]
        assert latest_attempt["State"] == "done"
        filesystem = next(
            fs for fs in latest_attempt["Filesystems"] if fs["Info"]["Name"] == "tank/data"
        )
        planned_steps = [step["Info"] for step in filesystem["Steps"]]
        assert planned_steps
        assert all(step["From"] for step in planned_steps), planned_steps
        assert (
            retained_version_guids["tank/data" + planned_steps[0]["From"]] == baseline_guid
        )

        source.wait_until_succeeds(
            "! zfs list -H -d 1 -t bookmark -o guid tank/data | grep -qx "
            + baseline_cursor,
            timeout=60,
        )
        resumed_cursors = set(source.succeed(
            "zfs list -H -d 1 -t bookmark -o guid tank/data"
        ).split())
        receiver_guids = set(target.succeed(
            "zfs list -H -d 1 -t snapshot -o guid tank/zrepl/source/tank/data"
        ).split())
        assert resumed_cursors
        assert resumed_cursors <= receiver_guids
        assert target.succeed(
            "zfs get -H -o value guid tank/zrepl/source/tank/data"
        ).strip() == receiver_dataset_guid
        target.succeed(
            "zfs list -H -t snapshot tank/zrepl/source/tank/data@zrepl_baseline"
        )
  '';
}
