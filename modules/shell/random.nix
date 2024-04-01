{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.random;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.random = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.pcscd.enable = true;
    myhm =
      { ... }@hm:
      {
        home.packages = with pkgs; [
          #pkgs.my.mysql-backup
          # <rust pkgs>
          bat
          restic
          du-dust
          fd
          gitui
          gex
          httpie
          dogdns
          jless
          sd
          procs
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
          #binutils.bintools # this conflicts with the gcc package
          unzip
          xz
          zip
          asciinema
          findutils
          # https://github.com/NixOS/nixpkgs/issues/265014
          (pkgs.rsync.overrideAttrs (_: _: { hardeningDisable = [ "fortify" ]; }))
          wget
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
          inputs.fast-flake-update.packages.${system}.fast-flake-update
          #inputs.nixfmt.packages.${pkgs.hostPlatform.system}.nixfmt
        ];

        persistence = mkIf withImpermanence { directories = [ "${hm.config.xdg.configHome}/.gnupg" ]; };
      };
  };
}
