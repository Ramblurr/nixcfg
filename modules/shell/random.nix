{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.random;
in
{
  options.modules.shell.random = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    services.pcscd.enable = true;
    myhm = _hm: {
      home.packages = with pkgs; [
        fh
        internetarchive
        (warp-terminal.override { waylandSupport = true; })
        gum
        xcp
        ghorg
        treefmt
        repomix
        glow
        graphviz
        magic-wormhole-rs
        nix-update
        psmisc
        #(openai-whisper-cpp.override { cudaSupport = true; })
        #openai-whisper
        smem
        #pkgs.my.mysql-backup
        # <rust pkgs>
        rink
        bandwhich
        bat
        restic
        dust
        fd
        #gitui
        #gitu
        #gex
        httpie
        doggo
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
        powertop
        ttyd
        linuxPackages.cpupower
        # linuxPackages.usbip
        yt-dlp
        imgurbash2
        mosh
        vifm
        smartmontools
        pwgen-secure
        nixfmt
        gost
        deploy-rs
      ];

    };
  };
}
