{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.default
    ./core.nix

    ../mixins/sops.nix
    ../mixins/aria2.nix
    ../mixins/git.nix
    ../mixins/gpg-agent.nix
    ../mixins/ssh.nix
    ../mixins/xdg.nix
    ../mixins/zsh.nix
    ../mixins/tmux.nix
    ../mixins/htop.nix
    ../mixins/vim.nix
    ../mixins/backup.nix
  ];

  config = {
    #users.users.ramblurr.linger = true;
    users.users.ramblurr.shell = pkgs.zsh;

    services.dbus.packages = with pkgs; [pkgs.dconf];
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    environment.systemPackages = with pkgs; [
      killall
      vim
      inotify-tools
      bandwhich
      tmux
      curl
      wget
    ];
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      users.ramblurr = {pkgs, ...} @ hm: {
        imports = [
          inputs.impermanence.nixosModules.home-manager.impermanence
          inputs.sops-nix.homeManagerModule
        ];
        systemd.user.startServices = true;
        home.extraOutputsToInstall = ["info" "man" "share" "icons" "doc"];
        home.stateVersion = "21.11";
        home.homeDirectory = "/home/ramblurr";
        home.sessionVariables = {
          EDITOR = "vim";
          CARGO_HOME = "${hm.config.xdg.dataHome}/cargo";
        };
        sops = {
          defaultSopsFile = ../secrets/secrets.sops.yaml;
          gnupg.home = "${hm.config.xdg.configHome}/.gnupg";
        };

        home.persistence."/persist/home/ramblurr" = {
          allowOther = true;
          directories = [
            "docs"
            "downloads"
            "sync"
            "src"
            "work"
            "vendor"
            "nixcfg"
            ".config/gnupg"
            ".config/gh"
            ".config/Morgen"
            ".config/OpenSCAD"
            ".config/PrusaSlicer-alpha"
            ".config/qobuz-dl"
          ];
        };
        manual = {manpages.enable = true;};
        news.display = "silent";
        programs = {
          home-manager.enable = true;
        };
        home.packages = lib.mkMerge [
          (lib.mkIf (pkgs.hostPlatform.system == "x86_64-linux") (with pkgs; [
            # x86_64-linux only
            zenith # uh oh, no aarch64 support? noooooo
          ]))
          (lib.mkIf (pkgs.hostPlatform.system == "aarch_64-linux") (with pkgs; [
            # aarch64-linux only
          ]))
          (with pkgs; [
            # <rust pkgs>
            bat
            du-dust
            exa
            fd
            gitui
            gex
            httpie
            dogdns
            jless
            sd
            procs
            ripgrep
            # </rust pkgs>
            nix-du
            nix-tree
            nix-prefetch
            nix-output-monitor
            ncdu
            binwalk
            usbutils
            rclone
            pciutils
            dmidecode
            lshw
            nvme-cli
            efibootmgr
            mokutil
            cryptsetup
            wipe
            file
            lsof
            unar
            p7zip
            sops
            age
            step-cli
            gptfdisk
            parted
            iotop
            which
            binutils.bintools
            unzip
            xz
            zip
            asciinema
            findutils
            rsync
            wget
            jq
            openssh
            watchman
            watchexec
            wireguard-tools
            difftastic
            just
            go-task
            fzf
            jdk
            powertop
            linuxPackages.cpupower
            # linuxPackages.usbip
            yt-dlp
            imgurbash2
            mosh
          ])
        ];
      };
    };
  };
}
