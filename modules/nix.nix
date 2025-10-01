{
  inputs,
  config,
  lib,
  pkgs,
  actual-nixpkgs,
  ...
}:

let
  inherit (config.repo.secrets.global) ciSigningPublicKey;
in
with lib;
{
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    extraOptions = ''
      # Quicker timeout for inaccessible binary caches
      connect-timeout = 5
      # Enable flakes
      experimental-features = nix-command flakes
      # Do not warn on dirty git repo
      warn-dirty = false
    '';
    settings = {
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ciSigningPublicKey
      ];
      trusted-users = [
        "root"
        "@wheel"
      ];
      auto-optimise-store = true;
    };

    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };

    registry.nixpkgs.flake = actual-nixpkgs;
    registry.stable.flake = inputs.nixpkgs-stable;
    registry.unstable.flake = inputs.nixpkgs;

    nixPath = [ "nixpkgs=/etc/nixpkgs/channels/nixpkgs" ];
  };

  systemd.tmpfiles.rules = [ "L+ /etc/nixpkgs/channels/nixpkgs - - - - ${actual-nixpkgs}" ];

  programs.command-not-found.enable = false;

  system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;

  # Just the bare necessities...
  environment.systemPackages = with pkgs; [
    kitty.terminfo
    cached-nix-shell
    dig
    jq
    curlHTTP3
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
  system = {
    # Enable printing changes on nix build etc with nvd
    activationScripts.report-changes = ''
      PATH=$PATH:${
        lib.makeBinPath [
          pkgs.nvd
          pkgs.nix
        ]
      }
      nvd diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2) || true
    '';
  };

  documentation.nixos.enable = false;
}
