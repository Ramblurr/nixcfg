{
  inputs,
  config,
  lib,
  pkgs,
  actual-nixpkgs,
  ...
}:
with lib;
with lib.my;
{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.sops-nix.nixosModules.sops
  ] ++ (mapModulesRec' (toString ./modules) import);

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
      extra-substituters = [ "https://attic.mgmt.${config.repo.secrets.global.domain.home}/socozy" ];
      extra-trusted-public-keys = [
        "socozy:6DGMWTIQnpp/tsHzx45lX1lUOn4oiDwg7WX1/pJASwE= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
      substituters = [ "https://cache.nixos.org" ];
      trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
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

    nixPath = [ "nixpkgs=/etc/nixpkgs/channels/nixpkgs" ];
  };

  systemd.tmpfiles.rules = [ "L+ /etc/nixpkgs/channels/nixpkgs - - - - ${actual-nixpkgs}" ];

  programs.command-not-found.enable = false;
  #home-manager.users.huantian.programs.nix-index.enable = true;

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
}
