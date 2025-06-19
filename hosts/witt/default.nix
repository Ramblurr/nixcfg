{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.repo.secrets.global) domain;
  inherit (config.modules.users.primaryUser) username;
  machine-id = "798016ab36504bd0a5397317013bedba";
  defaultSopsFile = ./secrets.sops.yaml;
in
{
  imports = [
    ./disk-config.nix
    ./hardware.nix
    ./home.nix
    ./syncthing.nix
    ../../config
    ../../config/workstation-impermanence.nix
  ];
  system.stateVersion = "24.05";
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  environment.etc."machine-id".text = machine-id;

  time.timeZone = "Europe/Berlin";

  #### TEMPORARY
  # Workaround kernel bug in kernel >= 6.6.57
  # ref: https://github.com/NixOS/nixpkgs/issues/353709
  # see my last-known-good.nix overlay for corresponding overlay pkg
  boot.kernelPackages = pkgs.linuxPackages_6_6;
  ### END

  networking = {
    networkmanager.enable = true;
    hostId = pkgs.lib.concatStringsSep "" (
      pkgs.lib.take 8 (
        pkgs.lib.stringToCharacters (builtins.hashString "sha256" config.networking.hostName)
      )
    );
    # oh boy, https://discourse.nixos.org/t/connected-to-mullvadvpn-but-no-internet-connection/35803/11
    resolvconf.enable = false;
  };
  services.resolved.enable = true;
  services.timesyncd.enable = true;
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
    settings.General.Experimental = true;
  };
  home.attic.enable = false;
  nix = {
    settings = {
      substituters = [
        "https://nixpkgs-wayland.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
    };
  };

  modules = {
    impermanence.enable = true;
    boot.zfs.enable = true;
    boot.zfs.usePlymouth = false;
    boot.zfs.scrubPools = [ "rpool" ];
    #vpn.mullvad.enable = true;
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    hardware.pipewire.enable = true;
    hardware.pipewire.denoise.enable = true;
    users.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
      "kvm"
      "audio"
      "flatpak"
      "input"
      "plugdev"
      "libvirtd"
    ];

    desktop = {
      kde.enable = true;
      kde.sddm.hideUsers = [ username ];
      xdg.enable = true;
      browsers = {
        firefox.enable = true;
        chromium.enable = true;
      };

      fonts =
        let
          # Pragmata Pro Style
          iosevka-ss08 = (pkgs.iosevka-bin.override { variant = "SS08"; });
          # Source Code Pro Style
          iosevka-ss09 = (pkgs.iosevka-bin.override { variant = "SS09"; });
          # IBM Plex Mono Style
          iosevka-ss15 = (pkgs.iosevka-bin.override { variant = "SS15"; });
        in
        {
          enable = true;
          mono.name = "Iosevka SS15";
          mono.package = iosevka-ss15;
          terminal.name = "Iosevka Term SS15";
          terminal.package = iosevka-ss15;
          symbols.fonts = [
            "Iosevka Term SS15"
          ];
          packages = with pkgs; [
            (iosevka-bin.override { variant = "Aile"; })
            (iosevka-bin.override { variant = "Etoile"; })
            iosevka-ss08
            iosevka-ss09
            iosevka-ss15
            iosevka-bin
          ];
        };
      programs = {
        cad.enable = true;
        junction.enable = true;
        #kdeconnect.enable = true;
        kitty.enable = true;
        logseq.enable = true;
        chrysalis.enable = true;
        element.enable = true;
        #nextcloud.enable = true;
        #nheko.enable = true;
        onepassword.enable = true;
        #owncloud.enable = true;
        signal.enable = true;
        thunderbird.enable = true;
        musescore.enable = true;
        yubico.enable = true;
      };
    };
    shell = {
      aria2.enable = true;
      attic.enable = true;
      atuin.enable = true;
      atuin.sync.enable = true;
      direnv.enable = true;
      git.enable = true;
      gpg-agent.enable = true;
      htop.enable = true;
      random.enable = true;
      ssh.enable = true;
      tmux.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
      zsh.powerlevel10k.enable = false;
      zsh.starship.enable = true;
    };

    editors = {
      emacs.enable = true;
      emacs.package = pkgs.emacs-pgtk;
      vim = {
        enable = true;
        extraPlugins = with pkgs.vimPlugins; [
          vim-surround
          vim-airline
          tabular
          vim-nix
          gruvbox-community
        ];
      };
      vscode.enable = true;
    };

    services = {
      attic-watch-store.enable = true;
      docker.enable = true;
      docker.enableOnBoot = false;
      flatpak.enable = true;
      microsocks.enable = true;
      podman.enable = true;
      printing.enable = true;
      printing.drivers = [
        pkgs.hplip
        pkgs.hplipWithPlugin
        pkgs.cups-brother-mfcl2750dw
      ];
      sshd.enable = true;
    };
    dev = {
      clojure.enable = true;
      fennel.enable = false;
      jetbrains.enable = true;
      k8s.enable = false;
      node.enable = true;
      python.enable = true;
      radicle.enable = false;
      random.enable = true;
    };
  };

}
