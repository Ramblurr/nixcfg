{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.repo.secrets.global) domain;
  hn = "witt";
  machine-id = "798016ab36504bd0a5397317013bedba";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../ramblurr.nix {
    inherit
      config
      lib
      pkgs
      inputs
      ;
  };
in
{
  imports = [
    ./disk-config.nix
    ./hardware.nix
    ./home.nix
    ./syncthing.nix
    ../../config/secrets.nix
    ../../config/attic.nix
    ../../config/workstation-impermanence.nix
  ];
  system.stateVersion = "24.05";
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  environment.etc."machine-id".text = machine-id;

  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";

  #### TEMPORARY
  # Workaround kernel bug in kernel >= 6.6.57
  # ref: https://github.com/NixOS/nixpkgs/issues/353709
  # see my last-known-good.nix overlay for corresponding overlay pkg
  boot.kernelPackages = pkgs.linuxPackages_6_6;
  ### END

  networking = {
    networkmanager.enable = true;
    hostName = hn;
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
  home.attic.enable = true;
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
    users.primaryUser = {
      username = "ramblurr";
      name = "Me";
      homeDirectory = "/home/ramblurr";
      signingKey = "978C4D08058BA26EB97CB51820782DBCACFAACDA";
      email = "unnamedrambler@gmail.com";
      passwordSecretKey = "ramblurr-password";
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
        "kvm"
        "audio"
        "flatpak"
        "input"
        "plugdev"
        "libvirtd"
      ];
    };

    desktop = {
      kde.enable = true;
      kde.sddm.hideUsers = [ config.modules.users.primaryUser.username ];
      xdg.enable = true;
      browsers = {
        firefox.enable = true;
        chromium.enable = true;
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
