{
  config,
  nixpkgs,
  pkgs,
  lib,
  ...

}:
{

  assertions = [
    {
      assertion = config.system.replaceDependencies.replacements == [ ];
      message = "system.replaceDependencies.replacements causes hydra to build the system at evaluation time. It must be removed!";
    }
    {
      assertion = config.networking.nftables.enable || config.networking.firewall.extraInputRules == "";
      message = "\"config.networking.firewall.extraInputRules\" is defined but nftables is not activated";
    }
  ];

  users.users.root = {
    initialHashedPassword = lib.mkForce null;
    openssh.authorizedKeys.keys = config.site.sshPubKeys;
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true;

  boot = {
    enableContainers = false; # should be enabled explicitly
    loader.systemd-boot = {
      configurationLimit = lib.mkDefault 10;
      editor = false;
      graceful = true;
    };
    kernel.sysctl = {
      "kernel.panic" = 60; # reset 60 seconds after a kernel panic
      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.ipv6.conf.all.disable_ipv6" = lib.mkIf (!config.networking.enableIPv6) "1";
    };
    tmp.cleanOnBoot = true;
  };

  documentation.enable = false;

  programs.command-not-found.enable = false;
  environment = {
    interactiveShellInit = # sh
      ''
        # raise some awareness towards failed services
        systemctl --no-pager --failed || true
      '';
    systemPackages =
      with pkgs;
      [
        bmon
        curl
        nvd
        ncdu
        dig
        ethtool
        fd
        iproute2
        jq
        lsof
        nmap
        pv
        ripgrep
        rsync
        strace
        tree
        wget
      ]
      ++ (map (x: x.terminfo) (
        with pkgs.pkgsBuildBuild;
        [
          ghostty
          kitty
          tmux
          wezterm
        ]
      ));
  };

  time.timeZone = lib.mkDefault "Europe/Berlin";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
    ];
  };
  networking = {
    firewall = {
      logRefusedConnections = false;
    };
    useHostResolvConf = lib.mkIf (!config.services.resolved.enable) true;
  };

  sops.environment.SOPS_GPG_EXEC = lib.getExe (pkgs.gnupg.override { enableMinimal = true; });
  nix = {
    gc = {
      automatic = lib.mkDefault true;
      dates = "06:00";
      options = "--delete-older-than 30d";
      randomizedDelaySec = "6h";
    };
    nixPath = [
      "nixpkgs=${builtins.unsafeDiscardStringContext nixpkgs}"
      "nixos=${builtins.unsafeDiscardStringContext nixpkgs}"
      "nixos-config=/you/shall/deploy/from/the/flake"
    ];
    registry.nixpkgs.flake = nixpkgs;
    settings = {
      extra-experimental-features = "ca-derivations";
      # if a download from hydra fails, we want to stop and retry it, instead of building it
      #fallback = false;
      #trusted-public-keys = [
      #  "hydra.hq.c3d2.de:KZRGGnwOYzys6pxgM8jlur36RmkJQ/y8y62e52fj1ps="
      #];
      #stalled-download-timeout = 30; # in case hydra is not reachable fail faster
      # don't self feed hydra
      #substituters = lib.mkIf (config.networking.hostName != "hydra") (
      #  lib.mkBefore [ "https://hydra.hq.c3d2.de" ]
      #);
    };
  };

  #nixpkgs.config = {
  #  allowAliases = false;
  #  allowUnfreePredicate =
  #    pkg:
  #    builtins.elem (pkgs.lib.getName pkg) [
  #      "drone.io"
  #      "drone-runner-ssh"
  #      "elasticsearch" # mastodon
  #    ];
  #};
  programs = {
    fzf.keybindings = true;
    git = {
      enable = true;
    };
    htop = {
      enable = true;
      settings = {
        cpu_count_from_one = true;
        hide_kernel_threads = true;
        hide_userland_threads = true;
        highlight_base_name = true;
      };
    };
    iotop.enable = true;
    mtr.enable = true;
    tcpdump.enable = true;
    tmux = {
      enable = true;
      historyLimit = 50000;
      extraConfig = # tmux
        ''
          # mouse control
          set -g mouse on

          # don't clear selection on copy
          bind-key -Tcopy-mode-vi MouseDragEnd1Pane send -X copy-selection-no-clear
          bind-key -Tcopy-mode-vi y send -X copy-selection-no-clear
        '';
    };
    traceroute.enable = true;
    vim = {
      enable = true;
      defaultEditor = true;
    };
  };

  modules.services.sshd.enable = true;

  security.acme = {
    acceptTerms = true;
    defaults = {
      email = config.site.data.contact;
      # letsencrypt staging server with way higher rate limits
      server = "https://acme-staging-v02.api.letsencrypt.org/directory";
      reloadServices = lib.optional config.services.nginx.enable "nginx";
    };
  };

  system.activationScripts.deleteOldSystemProfiles = lib.mkIf config.nix.gc.automatic ''
    echo "Deleting old system profiles..."
    ${config.nix.package}/bin/nix-env --profile /nix/var/nix/profiles/system --delete-generations +10 || true
  '';

  systemd = {
    enableEmergencyMode = false;
    services = {
      nginx.serviceConfig.OOMScoreAdjust = "-100";
      nix-daemon.serviceConfig = {
        KillMode = "control-group"; # kill all worker thread when restarting
        Restart = "on-failure"; # restart if killed eg oom killed
      };
    };

    watchdog = lib.mkIf (!config.boot.isContainer) {
      # when the system hangs reboot fast
      runtimeTime = "15s";
      rebootTime = "15s";
    };
  };
}
