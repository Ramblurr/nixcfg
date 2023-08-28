{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "quine";
in {
  imports = [
    ../../../home.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    ./hardware-configuration.nix
    ./wireplumber.nix
    ./syncthing.nix
  ];
  system.stateVersion = "23.05";
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  sops.defaultSopsFile = ./secrets.sops.yaml;
  environment.etc."machine-id".text = "76913090587c40c8a3207202dfe86fc2";
  services.udev.extraRules = ''
    KERNEL=="ttyACM0", MODE:="666"
  '';
  modules = {
    desktop = {
      enable = true;
      setupCommands = ''
        xrandr \
          --output DP-1 --mode 5120x1440 --auto
        xrandr \
        --output HDMI-A-1 --mode 3840x2160 --auto
      '';
      hyprland.enable = true;
      kde.enable = true;
      xdg.enable = true;
      random-apps.enable = true;
      browsers = {
        firefox.enable = true;
        chromium.enable = true;
      };
      gaming.enable = true;
      programs = {
        onepassword.enable = true;
        calibre.enable = true;
        element.enable = true;
        kdeconnect.enable = true;
        discord.enable = true;
        junction.enable = true;
        kitty.enable = true;
        logseq.enable = true;
        nextcloud.enable = true;
        nheko.enable = true;
        obs.enable = true;
        signal.enable = true;
        slack.enable = true;
        thunderbird.enable = true;
      };
    };
    shell = {
      aria2.enable = true;
      atuin.enable = true;
      direnv.enable = true;
      git.enable = true;
      gpg-agent.enable = true;
      htop.enable = true;
      ssh.enable = true;
      tmux.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
      zsh.starship.enable = true;
      random.enable = true;
    };
    services = {
      docker.enable = true;
      docker.enableOnBoot = false;
      podman.enable = true;
      microsocks.enable = true;
      printing.enable = true;
      printing.drivers = [pkgs.cups-brother-mfcl2750dw];
      sshd.enable = true;
      flatpak.enable = true;
    };
    dev = {
      clojure.enable = true;
      python.enable = true;
      node.enable = true;
      k8s.enable = true;
      jetbrains.enable = true;
      random.enable = true;
    };
    editors = {
      emacs.enable = true;
      vim.enable = true;
      vscode.enable = true;
    };
    impermanence.enable = true;
    boot.zfs.enable = true;
    boot.zfs.pools = ["rpool"];
    vpn.mullvad.enable = true;
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    networking.default.enable = true;
    networking.default.hostName = "quine";
    hardware.ryzen.enable = true;
    hardware.nvidia.enable = true;
    hardware.pipewire.enable = true;
    hardware.pipewire.denoise.enable = true;
    users.primaryUser = {
      username = "ramblurr";
      name = "Casey Link";
      homeDirectory = "/home/ramblurr";
      signingKey = "978C4D08058BA26EB97CB51820782DBCACFAACDA";
      email = "unnamedrambler@gmail.com";
      passwordSecretKey = "ramblurr-password";
      extraGroups = [
        "wheel"
        "audio"
        "flatpak"
        "input"
        "plugdev"
      ];
    };
    services.borgmatic = {
      enable = true;
      name = "aquinas.socozy.casa-mali";
      repositories = [
        "ssh://aquinas@borgbackup-host.int.socozy.casa/mnt/backup/borg_repos/aquinas/home"
        "\${OFFSITE_REPOSITORY}"
      ];
      exclude-patterns = [
        "etc/ssl"
        "var/home/*/.cache"
        "var/home/*/.var"
        "var/home/*/.local/lib"
        "var/home/*/.local/share/containers"
        "var/home/*/.local/share/JetBrains"
        "var/home/*/.local/share/volta"
        "var/home/*/.local/share/lein"
        "var/home/*/.local/share/Trash"
        "var/home/*/.local/share/virtualenv"
        "var/home/*/.local/share/yarn"
        "var/home/*/.local/share/nvm"
        "var/home/*/.local/state"
        "var/home/*/.npm"
        "var/home/*/.yarn"
        "var/home/*/.vagrant.d/boxes"
        "\'*.pyc\'"
        "'*/.vim*.tmp'"
        "'*/.DS_Store'"
        "'*/node_modules'"
        "'*/build'"
        "'*/target'"
        "'*/dist'"
        "'*/tmp'"
        "'*/bower_components'"
        "'*.idea'"
        "'*/.*~'"
        "'*/out'"
        "'*/.vagrant'"
        "'*/securedir'"
        "'*/encrypted'"
        "'*/ram'"
        "'*/cache'"
        "'*/.cache'"
        "'*/_cacache'"
        "'*/_lock'"
        "'*/*.tmp'"
        "'*/*.swp'"
        "'*/*~'"
        "'*/*.lock'"
        "'*/*-nas'"
        "'*/.Trash'"
        "'*/.terraform'"
        "'*/venv'"
        "'*/emacs-doom.d'"
        "'*/.gradle'"
        "'*/.*sync*.db'"
        "'*/.ansible'"
      ];
    };
  };
}
