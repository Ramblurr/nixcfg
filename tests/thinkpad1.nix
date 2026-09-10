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
    stable = c.system.nixos.release == "26.05";
    locale =
      c.time.timeZone == "Europe/Vienna"
      && c.i18n.defaultLocale == "de_AT.UTF-8"
      && c.console.keyMap == "de"
      && c.services.xserver.xkb.layout == "de";
    installer =
      let
        i = nixosConfigurations.thinkpad1-installer.config;
      in
      i.isoImage.makeEfiBootable
      && i.isoImage.makeUsbBootable
      && i.fileSystems."/".fsType == "tmpfs"
      && !i.services.openssh.settings.PasswordAuthentication
      && !i.services.openssh.settings.KbdInteractiveAuthentication
      && !(builtins.hasAttr family i.users.users)
      && !(i ? sops);
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
