{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.random;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.random = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    services.pcscd.enable = true;
    myhm =
      { ... }@hm:
      {
        home.packages = with pkgs; [
          treefmt2
          repomix
          glow
          graphviz
          #(openai-whisper-cpp.override { cudaSupport = true; })
          #openai-whisper
          smem
          #pkgs.my.mysql-backup
          # <rust pkgs>
          rink
          bandwhich
          bat
          restic
          du-dust
          fd
          gitui
          gitu
          gex
          httpie
          dogdns
          jless
          sd
          procs
          # </rust pkgs>
          proxychains-ng
          #mat2 # https://github.com/NixOS/nixpkgs/issues/348081
          waypipe
          nix-du
          nix-tree
          nix-prefetch
          nix-output-monitor
          ncdu
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
          gptfdisk
          parted
          iotop
          which
          unzip
          #xz
          zip
          # asciinema py12 nose
          findutils
          # https://github.com/NixOS/nixpkgs/issues/265014
          #(pkgs.rsync.overrideAttrs (_: _: { hardeningDisable = [ "fortify" ]; }))
          rsync
          wget
          jet
          jq
          openssh
          watchman
          watchexec
          tree
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
          vifm
          smartmontools
          pwgen-secure
          alejandra
          nixfmt-rfc-style
          cloudflare-utils
        ];

        persistence = mkIf withImpermanence { directories = [ "${hm.config.xdg.configHome}/.gnupg" ]; };
      };
  };
}
