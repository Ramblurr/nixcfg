{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  imports =
    [
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
    ]
    ++ (mapModulesRec' (toString ./modules) import);

  # https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module
  # "use the global pkgs that is configured via the system level nixpkgs options"
  # "This saves an extra Nixpkgs evaluation, adds consistency, and removes the dependency on NIX_PATH,
  #  which is otherwise used for importing Nixpkgs."
  home-manager.useGlobalPkgs = true;

  # Configure nix and nixpkgs
  nixpkgs.config.allowUnfree = true;
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    extraOptions = "experimental-features = nix-command flakes";

    settings = {
      substituters = [
        "https://hyprland.cachix.org"
        "https://cache.nixos.org"
        "https://nixpkgs-wayland.cachix.org"
        #"https://unmatched.cachix.org"
        "https://nix-community.cachix.org"
        "https://nix-gaming.cachix.org"
        #"https://arm.cachix.org/"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        #"unmatched.cachix.org-1:F8TWIP/hA2808FDABsayBCFjrmrz296+5CQaysosTTc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        #"arm.cachix.org-1:5BZ2kjoL1q6nWhlnrbAl+G7ThY7+HaBRD9PZzqZkbnM="
      ];
      trusted-users = ["root" "@wheel"];
      auto-optimise-store = true;
    };

    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };

    registry.nixpkgs.flake = inputs.nixpkgs;

    nixPath = [
      "nixpkgs=/etc/nixpkgs/channels/nixpkgs"
    ];
  };

  systemd.tmpfiles.rules = [
    "L+ /etc/nixpkgs/channels/nixpkgs - - - - ${inputs.nixpkgs}"
  ];

  programs.command-not-found.enable = false;
  #home-manager.users.huantian.programs.nix-index.enable = true;

  system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;

  # Just the bare necessities...
  environment.systemPackages = with pkgs; [
    kitty.terminfo
    cached-nix-shell
    dig
    jq
    curl
    vim
    wget
    unzip
    killall
    nvd
    lsof

    # These are used by the flake tooling
    git
    git-crypt
    sops
    gnupg
  ];
}
