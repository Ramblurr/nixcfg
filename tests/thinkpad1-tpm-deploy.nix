{ pkgs, alternateSystemd }:
pkgs.testers.runNixOSTest {
  name = "thinkpad1-tpm-deploy";

  nodes.machine =
    { lib, pkgs, ... }:
    let
      encryptedConfig = {
        imports = [ ../hosts/thinkpad1/tpm-deploy.nix ];
        boot.initrd.luks.devices = lib.mkVMOverride {
          cryptroot = {
            device = "/dev/vdb";
            crypttabExtraOpts = [ "tpm2-device=auto" ];
          };
        };
        virtualisation.rootDevice = "/dev/mapper/cryptroot";
      };
      entryOverride = line: {
        imports = [ encryptedConfig ];
        boot.loader.systemd-boot.extraInstallCommands = ''
          entry=$(awk '$1 == "default" {print $2}' /boot/loader/loader.conf)
          printf '%s\n' ${lib.escapeShellArg line} >> "/boot/loader/entries/$entry"
        '';
      };
      loaderOverride = line: {
        imports = [ encryptedConfig ];
        boot.loader.systemd-boot.extraInstallCommands = ''
          printf '%s\n' ${lib.escapeShellArg line} >> /boot/loader/loader.conf
        '';
      };
    in
    {
      virtualisation = {
        emptyDiskImages = [ 1024 ];
        useBootLoader = true;
        useEFIBoot = true;
        efi.OVMF = pkgs.OVMFFull;
        tpm.enable = true;
        mountHostNixStore = true;
        memorySize = 2048;
        # Let OVMF honor BootOrder rather than QEMU forcing the fallback path.
        qemu.drives = lib.mkForce [
          {
            name = "root";
            file = ''"$NIX_DISK_IMAGE"'';
            deviceExtraOpts.serial = "root";
            driveExtraOpts.werror = "report";
          }
          {
            file = "$(pwd)/empty0.qcow2";
            driveExtraOpts.werror = "report";
          }
        ];
      };
      boot.loader.systemd-boot.enable = true;
      boot.initrd.systemd.enable = true;
      boot.initrd.systemd.tpm2.enable = true;
      boot.initrd.availableKernelModules = [ "tpm_tis" ];
      # Keep the VM control connection alive across systemd package switches.
      systemd.services.backdoor.restartIfChanged = false;
      environment.systemPackages = [
        pkgs.cryptsetup
        pkgs.efibootmgr
        pkgs.strace
      ];

      specialisation.encrypted.configuration = encryptedConfig;
      specialisation.settings.configuration = {
        imports = [ encryptedConfig ];
        environment.etc."tpm-deploy-example".text = "settings changed";
      };
      specialisation.settings2.configuration = {
        imports = [ encryptedConfig ];
        environment.etc."tpm-deploy-example".text = "second settings change";
      };
      specialisation.settings3.configuration = {
        imports = [ encryptedConfig ];
        environment.etc."tpm-deploy-example".text = "third settings change";
      };
      specialisation.failedInstall.configuration = {
        imports = [ encryptedConfig ];
        boot.loader.systemd-boot.extraInstallCommands = ''
          echo "deliberate boot installation failure" >&2
          exit 42
        '';
      };
      specialisation.failedMenu.configuration = {
        imports = [ encryptedConfig ];
        boot.loader.systemd-boot.extraInstallCommands = ''
          printf broken > /boot/EFI/systemd/systemd-bootx64.efi
        '';
      };
      specialisation.duplicateOptions.configuration = entryOverride "options unexpected=1";
      specialisation.duplicateLinux.configuration = entryOverride "linux /EFI/BOOT/other.efi";
      specialisation.alternateEntry.configuration = entryOverride "efi /EFI/BOOT/other.efi";
      specialisation.preferredEntry.configuration = loaderOverride "preferred nixos-generation-1.conf";
      specialisation.duplicateDefault.configuration = loaderOverride " default\tmissing.conf";
      specialisation.changedInitrd.configuration = {
        imports = [ encryptedConfig ];
        boot.initrd.systemd.storePaths = [ pkgs.hello ];
      };
      specialisation.changedMicrocode.configuration = {
        imports = [ encryptedConfig ];
        hardware.cpu.intel.updateMicrocode = true;
      };
      specialisation.changedKernel.configuration = {
        imports = [ encryptedConfig ];
        boot.kernelPackages = pkgs.linuxPackages_6_12;
      };
      specialisation.changedBootloader.configuration = {
        imports = [ encryptedConfig ];
        systemd.package = alternateSystemd;
      };
      specialisation.failedLoader.configuration = {
        imports = [ encryptedConfig ];
        systemd.package = alternateSystemd;
        boot.loader.systemd-boot.extraInstallCommands = ''
          echo "deliberate failure after loader replacement" >&2
          exit 42
        '';
      };
      specialisation.invalidArguments.configuration = {
        imports = [ encryptedConfig ];
        boot.kernelParams = lib.mkAfter [ "example=\"quiet\nunexpected\"" ];
      };
      specialisation.emptyArguments.configuration = {
        imports = [ encryptedConfig ];
        boot.kernelParams = lib.mkForce [ ];
      };
    };

  testScript =
    { nodes, ... }:
    let
      encrypted = nodes.machine.specialisation.encrypted.configuration.system.build.toplevel;
      settings = nodes.machine.specialisation.settings.configuration.system.build.toplevel;
      settings2 = nodes.machine.specialisation.settings2.configuration.system.build.toplevel;
      settings3 = nodes.machine.specialisation.settings3.configuration.system.build.toplevel;
      failedInstall = nodes.machine.specialisation.failedInstall.configuration.system.build.toplevel;
      failedLoader = nodes.machine.specialisation.failedLoader.configuration.system.build.toplevel;
      failedMenu = nodes.machine.specialisation.failedMenu.configuration.system.build.toplevel;
      duplicateOptions =
        nodes.machine.specialisation.duplicateOptions.configuration.system.build.toplevel;
      duplicateLinux = nodes.machine.specialisation.duplicateLinux.configuration.system.build.toplevel;
      alternateEntry = nodes.machine.specialisation.alternateEntry.configuration.system.build.toplevel;
      preferredEntry = nodes.machine.specialisation.preferredEntry.configuration.system.build.toplevel;
      duplicateDefault =
        nodes.machine.specialisation.duplicateDefault.configuration.system.build.toplevel;
      changedInitrd = nodes.machine.specialisation.changedInitrd.configuration.system.build.toplevel;
      changedMicrocode =
        nodes.machine.specialisation.changedMicrocode.configuration.system.build.toplevel;
      changedKernel = nodes.machine.specialisation.changedKernel.configuration.system.build.toplevel;
      changedBootloader =
        nodes.machine.specialisation.changedBootloader.configuration.system.build.toplevel;
      invalidArguments =
        nodes.machine.specialisation.invalidArguments.configuration.system.build.toplevel;
      emptyArguments = nodes.machine.specialisation.emptyArguments.configuration.system.build.toplevel;
      registration = pkgs.closureInfo {
        rootPaths = [
          encrypted
          settings
          settings2
          settings3
          failedInstall
          failedLoader
          failedMenu
          duplicateOptions
          duplicateLinux
          alternateEntry
          preferredEntry
          duplicateDefault
          changedInitrd
          changedMicrocode
          changedKernel
          changedBootloader
          invalidArguments
          emptyArguments
        ];
      };
    in
    ''
      import json
      import shlex
      import time

      def snapshot():
          return {
              "luks": json.loads(machine.succeed("cryptsetup luksDump --dump-json-metadata /dev/vdb")),
              "profile": machine.succeed("readlink -f /nix/var/nix/profiles/system"),
              "loader": machine.succeed("cat /boot/loader/loader.conf"),
              "efi": machine.succeed("sha256sum /boot/EFI/systemd/systemd-bootx64.efi /boot/EFI/BOOT/BOOTX64.EFI"),
          }

      def check_boot_menu():
          entries = [entry for entry in json.loads(machine.succeed("bootctl list --json=short"))
                     if entry.get("options", "").startswith("init=")
                     and "-specialisation-" not in entry["id"]
                     and (entry["id"].startswith("nixos-") or entry["id"] == "thinkpad1-tpm-rollback.conf")]
          default = next(line.split()[1] for line in snapshot()["loader"].splitlines()
                         if line.startswith("default "))
          assert entries[0]["id"] == default, "default is not the first NixOS menu entry"
          assert "(default)" in entries[0]["title"], "default label is unclear"
          assert len({entry["options"] for entry in entries}) == len(entries), f"duplicate NixOS menu entries: {entries}"
          generations = [int(entry["version"].split()[1]) for entry in entries[1:]]
          assert generations == sorted(generations, reverse=True), "rollback entries are not newest-first"
          assert any(entry["id"] == "thinkpad1-tpm-rollback.conf" for entry in entries), "protected bridge missing"
          return entries

      def refused(message, candidate="${settings3}"):
          before = snapshot()
          firmware = "sha256sum /sys/firmware/efi/efivars/Boot*-8be4df61-93ca-11d2-aa0d-00e098032b8c"
          before_firmware = machine.succeed(firmware)
          status, output = machine.execute(f"{candidate}/thinkpad1-tpm-deploy boot {candidate} 2>&1")
          assert status != 0 and message in output, output
          assert snapshot() == before, "refused deployment changed enrollment/profile/default"
          assert machine.succeed(firmware) == before_firmware, "refused deployment changed firmware selection"

      start_all()
      machine.wait_for_unit("multi-user.target")
      # Disposable test credential; never used on a real host.
      machine.succeed("printf test-recovery | cryptsetup luksFormat --batch-mode --pbkdf pbkdf2 --pbkdf-force-iterations 1000 /dev/vdb -")
      machine.succeed("printf test-recovery | cryptsetup open --key-file=- /dev/vdb cryptroot")
      machine.succeed("mkfs.ext4 /dev/mapper/cryptroot")
      # Bootstrap only: obtain the first measured encrypted boot, then replace this token.
      machine.succeed("printf test-recovery | systemd-cryptenroll --unlock-key-file=/dev/stdin --tpm2-device=auto --tpm2-pcrs= /dev/vdb")
      machine.succeed("nix-env --profile /nix/var/nix/profiles/system --set ${encrypted}")
      machine.succeed("${encrypted}/bin/switch-to-configuration boot")
      machine.succeed("bootctl install")
      machine.succeed("efibootmgr --bootorder $(efibootmgr | awk '/Linux Boot Manager/ {print substr($1,5,4); exit}')")
      machine.succeed("sync")
      machine.crash()
      machine.wait_for_unit("multi-user.target")
      machine.succeed("findmnt -n -o SOURCE / | grep /dev/mapper/cryptroot")
      machine.succeed("nix-store --load-db < ${registration}/registration")
      machine.succeed("nix-env --profile /nix/var/nix/profiles/system --set ${encrypted}")
      machine.succeed("systemd-cryptenroll --unlock-tpm2-device=auto --tpm2-device=auto --tpm2-pcrs=4:sha256+9:sha256 /dev/vdb")
      machine.succeed("cryptsetup luksKillSlot --batch-mode /dev/vdb 1")
      # Keep a separate unmanaged credential: automatic cleanup must never
      # reclaim it, even after several successful boots and pending updates.
      machine.succeed("umask 077; printf test-recovery > /tmp/recovery.key")
      machine.succeed("cryptsetup luksAddKey --batch-mode --key-file /tmp/recovery.key --pbkdf pbkdf2 --pbkdf-force-iterations 1000 /dev/vdb /tmp/recovery.key")
      print(machine.succeed("efibootmgr -v; bootctl status --no-pager"))
      with subtest("preferred EFI selection is refused without clearing it"):
          machine.succeed("bootctl set-preferred @current")
          preferred = "/sys/firmware/efi/efivars/LoaderEntryPreferred-4a67b082-0a4c-41cf-b6c7-440b29bb8c4f"
          before_preferred = machine.succeed(f"sha256sum {preferred}")
          try:
              refused("LoaderEntryPreferred overrides")
              assert machine.succeed(f"sha256sum {preferred}") == before_preferred
          finally:
              machine.succeed('bootctl set-preferred ""')
      with subtest("configuration preference is refused before enrollment"):
          machine.succeed("cp /boot/loader/loader.conf /tmp/saved-loader.conf; printf ' preferred\\tnixos-generation-1.conf\\n' >> /boot/loader/loader.conf")
          try:
              refused("unsupported loader directive: preferred")
          finally:
              machine.succeed("cp /tmp/saved-loader.conf /boot/loader/loader.conf")
      boot_id = machine.succeed("efibootmgr | awk '/BootCurrent:/ {print $2}'").strip()
      with subtest("inactive firmware selection is rejected before enrollment"):
          machine.succeed(f"efibootmgr -b {boot_id} -A")
          try:
              refused("firmware boot entry is inactive")
          finally:
              machine.succeed(f"efibootmgr -b {boot_id} -a")
      with subtest("retargeted firmware selection is rejected before enrollment"):
          for partition, loader in [(1, r"\EFI\BOOT\other.efi"), (2, r"\EFI\systemd\systemd-bootx64.efi")]:
              machine.succeed(f"efibootmgr -b {boot_id} -B; efibootmgr -c -b {boot_id} -d /dev/vda -p {partition} -L 'Linux Boot Manager' -l '{loader}'")
              try:
                  refused("firmware boot target differs")
              finally:
                  machine.succeed(f"efibootmgr -b {boot_id} -B; efibootmgr -c -b {boot_id} -d /dev/vda -p 1 -L 'Linux Boot Manager' -l '\\EFI\\systemd\\systemd-bootx64.efi'")
      with subtest("a different mounted ESP is rejected even with identical files"):
          machine.succeed("mkdir /tmp/other-esp; mount -t tmpfs none /tmp/other-esp; cp -a /boot/. /tmp/other-esp/; mount --bind /tmp/other-esp /boot")
          try:
              refused("mounted ESP differs from firmware boot partition")
          finally:
              machine.succeed("umount /boot; umount /tmp/other-esp")
      with subtest("non-canonical boot arguments are rejected before enrollment"):
          refused("unsupported kernel parameter encoding", "${invalidArguments}")
          refused("unsupported kernel parameter encoding", "${emptyArguments}")
      with subtest("preview and test activation do not change enrollment or boot default"):
          before_luks = machine.succeed("cryptsetup luksDump --dump-json-metadata /dev/vdb")
          before_profile = machine.succeed("readlink -f /nix/var/nix/profiles/system")
          before_loader = machine.succeed("cat /boot/loader/loader.conf")
          for action in ["dry-activate", "test"]:
              output = machine.succeed(f"${settings2}/thinkpad1-tpm-deploy {action} ${settings2}")
              assert "TPM enrollment skipped" in output
              assert machine.succeed("cryptsetup luksDump --dump-json-metadata /dev/vdb") == before_luks
              assert machine.succeed("readlink -f /nix/var/nix/profiles/system") == before_profile
              assert machine.succeed("cat /boot/loader/loader.conf") == before_loader
      with subtest("repeat deployment preserves encrypted boot"):
          machine.succeed("cp /boot/loader/entries/$(awk '$1 == \"default\" {print $2}' /boot/loader/loader.conf) /boot/loader/entries/zzz-custom.conf")
          custom_entry = machine.succeed("sha256sum /boot/loader/entries/zzz-custom.conf")
          before = snapshot()
          started = time.monotonic()
          output = machine.succeed("${encrypted}/thinkpad1-tpm-deploy boot ${encrypted}")
          print(f"No-op preparation and installation: {time.monotonic() - started:.2f}s")
          assert "Preparing next-boot TPM enrollment" not in output
          after = snapshot()
          menu_before = machine.succeed("sha256sum /boot/loader/entries/*.conf")
          assert after["luks"] == before["luks"], "no-op changed LUKS metadata"
          assert after["profile"] == before["profile"], "no-op changed profile target"
          # The first install reconciles the bootstrap ESP's generation number
          # with the newly initialized profile on the encrypted root.
          started = time.monotonic()
          output = machine.succeed("${encrypted}/thinkpad1-tpm-deploy boot ${encrypted}")
          print(f"Warm no-op preparation and installation: {time.monotonic() - started:.2f}s")
          assert "Preparing next-boot TPM enrollment" not in output
          assert snapshot() == after, "warm no-op changed enrollment/profile/default"
          assert machine.succeed("sha256sum /boot/loader/entries/*.conf") == menu_before, "warm no-op changed menu"
          assert machine.succeed("sha256sum /boot/loader/entries/zzz-custom.conf") == custom_entry, "custom entry changed"
          machine.succeed("rm /boot/loader/entries/zzz-custom.conf")
          stats = json.loads(next(line.split(": ", 1)[1] for line in output.splitlines() if line.startswith("TPM timings (seconds): ")))
          assert stats["enrollment"] == 0 and stats["authorization"] > 0
          print(f"Warm no-op phases: {stats}")
          original_menu_version = check_boot_menu()[0]["version"]
          started = time.monotonic()
          machine.succeed("nix-env --profile /nix/var/nix/profiles/system --set ${encrypted}; ${encrypted}/bin/switch-to-configuration boot")
          print(f"Ordinary no-op profile/boot installation baseline: {time.monotonic() - started:.2f}s")
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("findmnt -n -o SOURCE / | grep /dev/mapper/cryptroot")
      with subtest("settings-changing deployment prepares unattended unlock"):
          machine.succeed("${settings}/thinkpad1-tpm-deploy switch ${settings}")
          check_boot_menu()
          machine.succeed("grep 'settings changed' /etc/tpm-deploy-example")
          for candidate in ["${settings2}", "${settings}"] * 4:
              started = time.monotonic()
              output = machine.succeed(f"{candidate}/thinkpad1-tpm-deploy boot {candidate}")
              elapsed = time.monotonic() - started
              check_boot_menu()
              stats = json.loads(next(line.split(": ", 1)[1] for line in output.splitlines() if line.startswith("TPM timings (seconds): ")))
              assert stats["enrollment"] > 0
              # Reinstall the same, already-covered candidate for a safe baseline.
              started = time.monotonic()
              machine.succeed(f"nix-env --profile /nix/var/nix/profiles/system --set {candidate}; {candidate}/bin/switch-to-configuration boot")
              baseline = time.monotonic() - started
              print(f"Settings deploy: total={elapsed:.2f}s baseline={baseline:.2f}s difference={elapsed - baseline:.2f}s phases={stats}")
              luks = json.loads(machine.succeed("cryptsetup luksDump --dump-json-metadata /dev/vdb"))
              assert set(luks["keyslots"]) >= {"0", "1", "2"}, "recovery, unmanaged, or original bound slot was removed"
              assert len(luks["keyslots"]) == 4, "superseded pending enrollment accumulated"
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep 'settings changed' /etc/tpm-deploy-example")
          machine.succeed("findmnt -n -o SOURCE / | grep /dev/mapper/cryptroot")
      with subtest("known-good rollback survives ordinary generation pruning"):
          machine.succeed("nix-env --profile /nix/var/nix/profiles/system --delete-generations old")
          machine.succeed("${settings2}/thinkpad1-tpm-deploy boot ${settings2}")
          entries = check_boot_menu()
          rollback = [entry for entry in entries if "init=${encrypted}/init" in entry.get("options", "").split()]
          assert rollback, "known-good rollback entry was pruned"
          assert rollback[0]["version"] == original_menu_version, "pruning lost the system generation label"
          machine.succeed("bootctl set-oneshot " + rollback[0]["id"])
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep -F 'init=${encrypted}/init' /proc/cmdline")
      with subtest("menu cleanup failure preserves rollback when the previous default is the bridge"):
          machine.succeed("${encrypted}/thinkpad1-tpm-deploy boot ${encrypted}")
          entries = check_boot_menu()
          assert entries[0]["id"] == "thinkpad1-tpm-rollback.conf"
          other = next(entry for entry in entries if "init=${settings}/init" in entry.get("options", "").split())
          machine.succeed("bootctl set-oneshot " + other["id"])
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep -F 'init=${settings}/init' /proc/cmdline")
          before = snapshot()
          status, output = machine.execute("${failedMenu}/thinkpad1-tpm-deploy boot ${failedMenu} 2>&1")
          assert status != 0 and "bootloader changed during installation" in output, output
          after = snapshot()
          assert all(after[key] == before[key] for key in ["profile", "loader", "efi"]), "previous boot default was not restored"
          for section in ["keyslots", "tokens"]:
              assert all(after["luks"][section].get(key) == value for key, value in before["luks"][section].items()), "existing credentials changed"
          entries = json.loads(machine.succeed("bootctl list --json=short"))
          rollback = [entry for entry in entries if "init=${settings}/init" in entry.get("options", "").split()]
          assert rollback, "restoring the previous default stranded the booted rollback"
          machine.succeed("bootctl set-oneshot " + rollback[0]["id"])
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep -F 'init=${settings}/init' /proc/cmdline")
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep -F 'init=${encrypted}/init' /proc/cmdline")
      with subtest("firmware overrides are refused without clearing them"):
          machine.succeed("efibootmgr -n $(efibootmgr | awk '/BootCurrent:/ {print $2}')")
          refused("BootNext overrides")
          machine.succeed("efibootmgr | grep BootNext")
          machine.succeed("efibootmgr -N")
      with subtest("cached source measurements never hide ESP changes"):
          machine.succeed("${settings2}/thinkpad1-tpm-deploy boot ${settings2}")
          check_boot_menu()
          entry = machine.succeed("cat /boot/loader/entries/thinkpad1-tpm-rollback.conf")
          initrd = "/boot" + next(line.split()[1] for line in entry.splitlines() if line.startswith("initrd "))
          machine.succeed(f"cp {initrd} /tmp/saved-initrd; printf changed >> {initrd}")
          refused("installed initrd differs from immutable source")
          machine.succeed(f"cp /tmp/saved-initrd {initrd}; rm /tmp/saved-initrd")
      with subtest("missing current enrollment never falls back to a password prompt"):
          luks = snapshot()["luks"]
          token = next(key for key, value in luks["tokens"].items() if value.get("keyslots") == ["2"])
          machine.succeed(f"umask 077; cryptsetup token export --token-id {token} /dev/vdb > /tmp/bound-token.json")
          machine.succeed(f"cryptsetup token remove --token-id {token} /dev/vdb")
          refused("no bound TPM enrollment")
          machine.succeed(f"cryptsetup token import --token-id {token} --json-file /tmp/bound-token.json /dev/vdb")
      with subtest("slot exhaustion preserves all existing credentials"):
          original = set(snapshot()["luks"]["keyslots"])
          machine.succeed("umask 077; printf test-recovery > /tmp/recovery.key")
          for _ in range(32 - len(original)):
              status, _ = machine.execute("cryptsetup luksAddKey --batch-mode --key-file /tmp/recovery.key --pbkdf pbkdf2 --pbkdf-force-iterations 1000 /dev/vdb /tmp/recovery.key 2>&1")
              if status != 0:
                  break
          refused("insufficient LUKS enrollment capacity")
          added = set(snapshot()["luks"]["keyslots"]) - original
          for slot in added:
              machine.succeed(f"cryptsetup luksKillSlot --batch-mode /dev/vdb {slot}")
      with subtest("interrupted enrollment can be retried without losing recovery"):
          before = snapshot()
          machine.succeed(r"""
              setsid ${settings3}/thinkpad1-tpm-deploy boot ${settings3} >/tmp/interrupted.log 2>&1 &
              pid=$!
              until grep -q 'Preparing next-boot TPM enrollment' /tmp/interrupted.log; do
                  kill -0 "$pid" || { cat /tmp/interrupted.log; exit 1; }
                  sleep 0.01
              done
              kill -KILL -- -"$pid"
              wait "$pid" || true
          """)
          after = snapshot()
          assert after["profile"] == before["profile"] and after["loader"] == before["loader"]
          assert after["luks"]["keyslots"]["0"] == before["luks"]["keyslots"]["0"]
          # An interrupted command does not establish ownership of a new slot
          # that has no matching token. Preserve it and the enrollment journal.
          slots = set(after["luks"]["keyslots"])
          machine.succeed("cryptsetup luksAddKey --batch-mode --key-file /tmp/recovery.key --pbkdf pbkdf2 --pbkdf-force-iterations 1000 /dev/vdb /tmp/recovery.key")
          unknown = set(snapshot()["luks"]["keyslots"]) - slots
          assert len(unknown) == 1
          journal = machine.succeed("sha256sum /var/lib/thinkpad1-tpm-deploy/state.json")
          refused("unclassified keyslots")
          assert machine.succeed("sha256sum /var/lib/thinkpad1-tpm-deploy/state.json") == journal
          machine.succeed("cryptsetup luksKillSlot --batch-mode /dev/vdb " + unknown.pop())
          machine.succeed("${settings3}/thinkpad1-tpm-deploy boot ${settings3}")
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep 'third settings change' /etc/tpm-deploy-example")
          # Exercise retention after another successful boot, not only repeated
          # deployments from the original enrollment.
          for candidate in ["${settings2}", "${settings}"] * 2:
              machine.succeed(f"{candidate}/thinkpad1-tpm-deploy boot {candidate}")
          assert len(snapshot()["luks"]["keyslots"]) <= 6
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep 'settings changed' /etc/tpm-deploy-example")
          machine.succeed("${settings2}/thinkpad1-tpm-deploy boot ${settings2}")
          luks = snapshot()["luks"]
          assert set(luks["keyslots"]) >= {"0", "1", "2"}
          assert len(luks["keyslots"]) <= 6
      with subtest("failed boot installation restores the selected default and profile"):
          for candidate, message in [
              ("${failedInstall}", "deliberate boot installation failure"),
              ("${duplicateOptions}", "ambiguous boot options"),
              ("${duplicateLinux}", "installed kernel path mismatch"),
              ("${alternateEntry}", "unsupported boot entry directive: efi"),
              ("${preferredEntry}", "unsupported loader directive: preferred"),
              ("${duplicateDefault}", "no explicit boot default"),
          ]:
              before = snapshot()
              status, output = machine.execute(f"{candidate}/thinkpad1-tpm-deploy boot {candidate} 2>&1")
              assert status != 0 and message in output, output
              after = snapshot()
              assert after["profile"] == before["profile"] and after["loader"] == before["loader"]
              for slot, value in before["luks"]["keyslots"].items():
                  assert after["luks"]["keyslots"][slot] == value
              for token, value in before["luks"]["tokens"].items():
                  assert after["luks"]["tokens"][token] == value
              assert len(after["luks"]["keyslots"]) == len(before["luks"]["keyslots"]) + 1
              machine.succeed("${encrypted}/thinkpad1-tpm-deploy boot ${encrypted}")
              assert len(snapshot()["luks"]["keyslots"]) <= 6
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep -F 'init=${encrypted}/init' /proc/cmdline")
      with subtest("unattended enrollment does not attempt PIN-protected disk tokens"):
          # Disposable VM PIN, unrelated to the laptop's login PIN.
          machine.succeed("NEWPIN=test-pin systemd-cryptenroll --unlock-tpm2-device=auto --tpm2-device=auto --tpm2-pcrs=4:sha256+9:sha256 --tpm2-with-pin=yes /dev/vdb")
          refused("PIN-protected disk tokens", "${failedInstall}")
          pin_token = next(token for token in snapshot()["luks"]["tokens"].values() if token.get("tpm2-pin"))
          machine.succeed("systemd-cryptenroll --wipe-slot=" + pin_token["keyslots"][0] + " /dev/vdb")
      with subtest("boot artifact updates preserve unattended unlock and rollback"):
          machine.succeed("cmp -n $(stat -c %s ${pkgs.microcode-intel}/intel-ucode.img) ${pkgs.microcode-intel}/intel-ucode.img ${changedMicrocode}/initrd")
          original_token = next(key for key, token in snapshot()["luks"]["tokens"].items() if token.get("keyslots") == ["2"])
          original_unlock = f"LD_LIBRARY_PATH=${encrypted}/systemd/lib/cryptsetup cryptsetup open --test-passphrase --token-only --token-id {original_token} /dev/vdb"
          machine.succeed(original_unlock)
          for candidate in ["${changedInitrd}", "${changedMicrocode}", "${changedKernel}", "${changedBootloader}", "${encrypted}"]:
              started = time.monotonic()
              action = "switch" if candidate == "${changedBootloader}" else "boot"
              output = machine.succeed(f"{candidate}/thinkpad1-tpm-deploy {action} {candidate}")
              print(f"Artifact deployment {candidate}: {time.monotonic() - started:.2f}s")
              if action == "switch":
                  machine.succeed(f"test $(readlink -f /run/current-system) = {candidate}")
              assert len(snapshot()["luks"]["keyslots"]) <= 8
              contexts = json.loads(machine.succeed("cat /var/lib/thinkpad1-tpm-deploy/state.json"))["contexts"]
              for policy, context in contexts.items():
                  loader_root = "/".join(context["loader"].split("/")[:4])
                  machine.succeed(f"nix-store --query --roots {loader_root} | grep -F thinkpad1-tpm-loader-{policy}-1-link")
              before = snapshot()
              again = machine.succeed(f"{candidate}/thinkpad1-tpm-deploy {action} {candidate}")
              assert "Preparing next-boot TPM enrollment" not in again
              assert snapshot() == before, "repeated artifact deployment changed state"
              machine.succeed("sync")
              machine.crash()
              machine.wait_for_unit("multi-user.target")
              machine.succeed(f"grep -F 'init={candidate}/init' /proc/cmdline")
              machine.succeed("findmnt -n -o SOURCE / | grep /dev/mapper/cryptroot")
              if candidate == "${encrypted}":
                  machine.succeed(original_unlock)
              else:
                  assert snapshot()["luks"]["tokens"][original_token]["keyslots"] == ["2"]
                  machine.fail(original_unlock)
              if candidate == "${changedBootloader}":
                  machine.succeed(f"{candidate}/thinkpad1-tpm-deploy boot {candidate}")
                  entries = json.loads(machine.succeed("bootctl list --json=short"))
                  old_kernel = next(entry for entry in entries if "init=${changedKernel}/init" in entry.get("options", "").split())
                  machine.succeed("bootctl set-oneshot " + old_kernel["id"])
                  machine.succeed("sync")
                  machine.crash()
                  machine.wait_for_unit("multi-user.target")
                  machine.succeed("grep -F 'init=${changedKernel}/init' /proc/cmdline")
                  # The booted closure has the old systemd package, but firmware
                  # used the newly installed shared bootloader.
                  machine.succeed(f"{candidate}/thinkpad1-tpm-deploy boot {candidate}")
      with subtest("unexpected cached boot sequence cannot authorize preparation"):
          machine.succeed("${encrypted}/thinkpad1-tpm-deploy boot ${encrypted}")
          cache_path = "/run/thinkpad1-tpm-deploy/measurements.json"
          saved_cache = machine.succeed(f"cat {cache_path}")
          corrupted = json.loads(saved_cache)
          event = next(event for event in corrupted["events"] if event["pcr"] == 4 and event["event"] != "separator")
          event["event"] = "separator"
          try:
              machine.succeed("printf %s " + shlex.quote(json.dumps(corrupted)) + " > " + cache_path)
              refused("unsupported PCR4 boot sequence")
          finally:
              machine.succeed("printf %s " + shlex.quote(saved_cache) + " > " + cache_path)
      with subtest("live EFI bytes are checked despite cached source measurements"):
          for path, message in [
              ("/boot/EFI/systemd/systemd-bootx64.efi", "installed bootloader differs from known sources"),
              ("/boot/EFI/BOOT/BOOTX64.EFI", "unrecognized fallback bootloader"),
          ]:
              machine.succeed(f"cp {path} /tmp/saved-efi; printf changed >> {path}")
              try:
                  refused(message)
              finally:
                  machine.succeed(f"cp /tmp/saved-efi {path}")
      with subtest("whole loader-policy bundle capacity is checked before enrollment"):
          tokens = snapshot()["luks"]["tokens"]
          fillers = [token for token in range(32) if str(token) not in tokens][:-1]
          machine.succeed("printf %s " + shlex.quote(json.dumps({"type": "test-capacity", "keyslots": []})) + " > /tmp/capacity-token.json")
          try:
              for token in fillers:
                  machine.succeed(f"cryptsetup token import --token-id {token} --json-file /tmp/capacity-token.json /dev/vdb")
              refused("insufficient LUKS enrollment capacity", "${failedLoader}")
          finally:
              for token in fillers:
                  machine.succeed(f"cryptsetup token remove --token-id {token} /dev/vdb")
      with subtest("failed loader replacement restores both EFI images and the prior default"):
          before = snapshot()
          status, output = machine.execute("${failedLoader}/thinkpad1-tpm-deploy boot ${failedLoader} 2>&1")
          assert status != 0 and "deliberate failure after loader replacement" in output, output
          after = snapshot()
          for key in ["profile", "loader", "efi"]:
              assert after[key] == before[key], f"failed loader update changed {key}"
          for section in ["keyslots", "tokens"]:
              for key, value in before["luks"][section].items():
                  assert after["luks"][section][key] == value
          machine.succeed("sync")
          machine.crash()
          machine.wait_for_unit("multi-user.target")
          machine.succeed("grep -F 'init=${encrypted}/init' /proc/cmdline")
      with subtest("power loss on both sides of loader replacement boots the prepared bridge"):
          for phase in ["entry-pruning", "before", "after"]:
              machine.succeed("${encrypted}/thinkpad1-tpm-deploy boot ${encrypted}")
              generation = machine.succeed("basename $(readlink /nix/var/nix/profiles/system)").strip().split("-")[1]
              machine.succeed("sync")
              machine.crash()
              machine.wait_for_unit("multi-user.target")
              machine.succeed("grep -F 'init=${encrypted}/init' /proc/cmdline")
              # Leave a prepared but unbooted default different from the bridge.
              machine.succeed("${settings3}/thinkpad1-tpm-deploy boot ${settings3}")
              primary = "/boot/EFI/systemd/systemd-bootx64.efi"
              original = machine.succeed(f"sha256sum {primary} | cut -d ' ' -f1").strip()
              fallback = machine.succeed("sha256sum /boot/EFI/BOOT/BOOTX64.EFI | cut -d ' ' -f1").strip()
              desired = machine.succeed("sha256sum ${changedBootloader}/systemd/lib/systemd/boot/efi/systemd-bootx64.efi | cut -d ' ' -f1").strip()
              if phase == "entry-pruning":
                  # Successful cleanup removed native duplicates of the bridge.
                  # Recreate one orphan generation to exercise native pruning.
                  old_entry = f"/boot/loader/entries/nixos-generation-{generation}.conf"
                  machine.succeed(f"test ! -e {old_entry} && cp /boot/loader/entries/thinkpad1-tpm-rollback.conf {old_entry}")
                  machine.succeed("nix-env --profile /nix/var/nix/profiles/system --delete-generations old")
                  trace = f"-e trace=unlink -e inject=unlink:signal=SIGSTOP:when=1+ -P {old_entry}"
              else:
                  injection = "delay_enter=120s" if phase == "before" else "signal=SIGSTOP"
                  trace = f"-e trace=renameat -e inject=renameat:{injection}:when=1 -P /boot/EFI/systemd"
              machine.succeed(f"setsid strace -f -o /tmp/loader-trace {trace} ${changedBootloader}/thinkpad1-tpm-deploy boot ${changedBootloader} > /tmp/interrupted-loader.log 2>&1 &")
              try:
                  if phase == "entry-pruning":
                      machine.wait_until_succeeds(f"test ! -e {old_entry}", timeout=90)
                  if phase != "before":
                      machine.wait_until_succeeds(f"test $(sha256sum {primary} | cut -d ' ' -f1) = {desired}", timeout=90)
                  machine.wait_until_succeeds("ps -eo stat,args | grep -E '^[Tt].*(bootctl|python)'", timeout=90)
                  machine.succeed(f"test $(sha256sum {primary} | cut -d ' ' -f1) = {original if phase == 'before' else desired}")
                  machine.succeed(f"test $(sha256sum /boot/EFI/BOOT/BOOTX64.EFI | cut -d ' ' -f1) = {desired if phase == 'entry-pruning' else fallback}")
                  if phase == "entry-pruning":
                      print(machine.succeed("cat /boot/loader/loader.conf /tmp/loader-trace"))
                  machine.succeed("entry=$(awk '$1 == \"default\" {print $2}' /boot/loader/loader.conf); grep -F 'init=${encrypted}/init' /boot/loader/entries/$entry")
              except Exception:
                  print(machine.execute("cat /tmp/interrupted-loader.log /tmp/loader-trace"))
                  raise
              machine.succeed("sync")
              machine.crash()
              machine.wait_for_unit("multi-user.target")
              machine.succeed("grep -F 'init=${encrypted}/init' /proc/cmdline")
              machine.succeed("${changedBootloader}/thinkpad1-tpm-deploy boot ${changedBootloader}")
              machine.succeed(f"test $(sha256sum /boot/EFI/BOOT/BOOTX64.EFI | cut -d ' ' -f1) = {desired}")
              assert len(snapshot()["luks"]["keyslots"]) <= 8
              machine.succeed("sync")
              machine.crash()
              machine.wait_for_unit("multi-user.target")
              machine.succeed("grep -F 'init=${changedBootloader}/init' /proc/cmdline")
    '';
}
