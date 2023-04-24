{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
# includes ci devshell nativeBuildInputs - see bottom
{
  imports = [
    inputs.home-manager.nixosModules.default # "home-manager"
    ./core.nix

    ../mixins/sops.nix
    ../mixins/aria2.nix
    ../mixins/git.nix
    ../mixins/gpg-agent.nix
    ../mixins/ssh.nix
    ../mixins/xdg.nix
    ../mixins/zsh.nix
  ];

  config = {
    # I don't think my user dbus socket is here without this?????
    users.users.ramblurr.linger = true;
    users.users.ramblurr.shell = pkgs.zsh;

    services.dbus.packages = with pkgs; [pkgs.dconf];
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;

      users.ramblurr = {pkgs, ...} @ hm: {
        home.extraOutputsToInstall = ["info" "man" "share" "icons" "doc"];
        home.stateVersion = "21.11";
        home.homeDirectory = "/home/ramblurr";
        home.sessionVariables = {
          EDITOR = "vim";
          CARGO_HOME = "${hm.config.xdg.dataHome}/cargo";
        };
        manual = {manpages.enable = false;};
        news.display = "silent";
        programs = {
          home-manager.enable = true;
          gpg.enable = true;
        };
        programs = {
          git.enable = true;
          neovim.enable = true;
        };
        home.packages = lib.mkMerge [
          (lib.mkIf (pkgs.hostPlatform.system == "x86_64-linux") (with pkgs; [
            # x86_64-linux only
            zenith # uh oh, no aarch64 support? noooooo
          ]))
          (lib.mkIf (pkgs.hostPlatform.system == "aarch_64-linux") (with pkgs; [
            # aarch64-linux only
          ]))
          # ++ inputs.self.devShells.${pkgs.stdenv.hostPlatform.system}.ci.nativeBuildInputs
          (with pkgs; [
            # <rust pkgs>
            bat
            tealdeer
            du-dust
            exa
            fd
            gitui
            gex
            grex
            hexyl
            xh
            dogdns
            ripgrep
            gnupg
            jless
            sd
            procs
            prs
            bandwhich
            pipes-rs
            rustscan
            # </rust pkgs>

            # nix-related (TODO move to devtools shell that gets pulled in)
            # nix-tree nix-du ncdu nix-prefetch nixpkgs-review
            nix-output-monitor

            htop
            ncdu
            binwalk
            usbutils
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
            wget
            curl
            findutils
            rsync
            wget
            curl
            jq
            openssh
            watchman
            watchexec
            wireguard-tools
            difftastic
            just
            go-task
            aspell
            aspellDicts.en
            aspellDicts.de
            curl
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
