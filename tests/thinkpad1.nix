# Read-only policy regression check against the real private host configurations.
nixosConfigurations:
let
  c = nixosConfigurations.thinkpad1.config;
  lib = nixosConfigurations.thinkpad1.pkgs.lib;
  family = c.modules.users.primaryUser.username;
  user = c.users.users.${family};
  admin = c.users.users.ramblurr;
  hm = c.home-manager.users.${family};
  existing =
    name:
    let
      old = nixosConfigurations.${name}.config;
    in
    old.modules.users.primaryUser.username == "ramblurr"
    && old.systemd.user.services.flatpak-auto-update.enable
    && old.systemd.user.timers.flatpak-auto-update.enable
    && old.systemd.user.timers.flatpak-auto-update.timerConfig.OnCalendar == "daily"
    && old.services.openssh.openFirewall;
  checks = {
    vmIsolation =
      let
        v = c.virtualisation.vmVariant;
      in
      v.networking.hostName == "thinkpad1-vm"
      && v.sops.secrets == { }
      && !v.services.tailscale.enable
      && !v.services.openssh.enable
      && v.users.users.viki.password == "test"
      && v.users.users.viki.hashedPasswordFile == null
      && v.users.users.ramblurr.hashedPasswordFile == null
      && !(v.fileSystems ? "/boot")
      && c.users.users.viki.password == null
      && c.users.users.viki.hashedPasswordFile != null;
    stable = c.system.nixos.release == "26.05";
    pinLogin =
      c.security.pinpam.enable
      &&
        c.security.pinpam.auth.services == [
          "login"
          "kde"
        ]
      && c.security.pam.services.login.rules.auth.pinpam.control == "sufficient"
      && c.security.pam.services.login.rules.auth.pinpam.args == [ "use_first_pass" ]
      &&
        c.security.pam.services.login.rules.auth.pinpam.order
        > c.security.pam.services.login.rules.auth.unix.order
      &&
        c.security.pam.services.login.rules.auth.pinpam.order
        < c.security.pam.services.login.rules.auth.deny.order
      && !(c.security.pam.services.sudo.rules.auth ? pinpam)
      && !(c.security.pam.services.sshd.rules.auth ? pinpam)
      && !builtins.elem "tss" c.users.users.viki.extraGroups
      && !c.security.pinpam.masterKey.enable;
    slowUnlock = builtins.elem "x-systemd.device-timeout=0" c.fileSystems."/".options;
    bootPrompt = c.boot.plymouth.theme == "catppuccin-mocha";
    ordinarySsh = builtins.elem "--ssh=false" c.services.tailscale.extraSetFlags;
    diskLayout =
      c.fileSystems."/".fsType == "btrfs"
      && c.fileSystems."/home".fsType == "btrfs"
      && c.disko.devices.disk.main.device == c.repo.secrets.local.systemDisk
      && c.disko.devices.disk.main.content.partitions.encrypted.content.type == "luks"
      && c.disko.devices.disk.main.content.partitions.encrypted.content.content.vg == "thinkpad1"
      && c.disko.devices.lvm_vg.thinkpad1.lvs.swap.size == "24G";
    hibernation =
      c.boot.initrd.systemd.enable
      && c.boot.initrd.systemd.tpm2.enable
      && c.boot.resumeDevice == "/dev/thinkpad1/swap"
      && map (s: s.device) c.swapDevices == [ "/dev/thinkpad1/swap" ]
      && !c.disko.devices.lvm_vg.thinkpad1.lvs.swap.content.randomEncryption
      && !builtins.elem "nohibernate" c.boot.kernelParams;
    homeSnapshots =
      c.services.snapper.configs.home.SUBVOLUME == "/home"
      && c.services.snapper.configs.home.TIMELINE_CREATE
      && c.services.snapper.configs.home.TIMELINE_CLEANUP
      && c.services.snapper.configs.home.TIMELINE_LIMIT_HOURLY == 24
      && builtins.elem "sh:home/.snapshots" c.modules.services.borgmatic.exclude-patterns
      && c.virtualisation.vmVariant.services.snapper.configs == { };
    syncthing =
      c.services.syncthing.enable
      && c.services.syncthing.user == family
      && c.services.syncthing.guiAddress == "127.0.0.1:8384"
      && !c.services.syncthing.overrideDevices
      && !c.services.syncthing.overrideFolders
      && c.services.syncthing.key == null
      && c.services.syncthing.cert == null
      && c.systemd.services.syncthing.environment.STNODEFAULTFOLDER == "true";
    backupPlan =
      c.modules.services.borgmatic.name == "thinkpad1"
      &&
        c.services.borgmatic.settings.source_directories == [
          "/home"
          "/etc"
        ]
      && builtins.length c.modules.services.borgmatic.repositories == 2
      && !c.virtualisation.vmVariant.services.borgmatic.enable;
    locale =
      c.time.timeZone == "Europe/Vienna"
      && c.i18n.defaultLocale == "en_US.UTF-8"
      && c.console.keyMap == "de"
      && c.services.xserver.xkb.layout == "at";
    accounts =
      user.uid == 1000
      && user.extraGroups == [ "networkmanager" ]
      && admin.uid == 1001
      && admin.extraGroups == [ "wheel" ];
    passwords = user.hashedPasswordFile != null && admin.hashedPasswordFile != null;
    secretWiring =
      c.modules.users.primaryUser.name == c.repo.secrets.local.primaryUserName
      && builtins.pathExists (c.node.secretsDir + "/local.nix")
      && lib.hasSuffix "/hosts/thinkpad1/secrets.sops.yaml" (toString c.sops.defaultSopsFile);
    desktop =
      c.services.desktopManager.plasma6.enable
      && !c.services.displayManager.sddm.enable
      && c.services.displayManager.plasma-login-manager.enable
      && c.services.displayManager.plasma-login-manager.settings.Greeter.PreselectedUser == family
      && !c.modules.desktop.kde.krohnkite.enable;
    flatpak =
      c.services.flatpak.enable
      && lib.any (
        cmd: lib.hasInfix "remote-add --user --if-not-exists flathub" cmd
      ) hm.systemd.user.services.flathub-user.Service.ExecStart;
    noAutomaticUpdates =
      !c.system.autoUpgrade.enable
      && !(c.systemd.user.services ? flatpak-auto-update)
      && !(c.systemd.user.timers ? flatpak-auto-update);
    remoteAdmin =
      c.services.tailscale.enable
      && c.services.openssh.enable
      && !c.services.openssh.openFirewall
      && !c.services.openssh.settings.PasswordAuthentication
      && !c.services.openssh.settings.KbdInteractiveAuthentication
      && !(builtins.elem 22 c.networking.firewall.allowedTCPPorts)
      && builtins.elem "tailscale0" c.networking.firewall.trustedInterfaces
      && admin.openssh.authorizedKeys.keys != [ ];
    noWorkstationServices =
      !c.virtualisation.docker.enable
      && !c.virtualisation.podman.enable
      && !c.virtualisation.libvirtd.enable
      && !c.modules.impermanence.enable;
    userOwnsPlasma =
      !(hm.xdg.configFile ? "kdeglobals")
      && !(hm.xdg.configFile ? "plasma-org.kde.plasma.desktop-appletsrc");
    witt =
      existing "witt"
      && nixosConfigurations.witt.config.services.displayManager.sddm.settings.Users.MinimumUid == 99999;
    quine = existing "quine";
  };
in
assert lib.assertMsg (lib.all (x: x) (
  builtins.attrValues checks
)) "Family desktop policy failed: ${builtins.toJSON checks}";
checks
