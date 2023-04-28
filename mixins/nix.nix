{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  _nix = pkgs.nixVersions.unstable;
in {
  config = {
    environment.systemPackages = [
      _nix
      pkgs.git
    ];
    nixpkgs.config = {
      allowAliases = false;
      allowUnfree = true;
    };
    nix = {
      package = _nix;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
      nixPath = lib.mkForce []; # i doth protest
      settings = {
        experimental-features = ["nix-command" "flakes"];
        builders-use-substitutes = true;
        cores = 0;
        max-jobs = "auto";
        auto-optimise-store = true;
        use-xdg-base-directories = true;
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
          "unmatched.cachix.org-1:F8TWIP/hA2808FDABsayBCFjrmrz296+5CQaysosTTc="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
          "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        ];
        substituters = [
          "https://hyprland.cachix.org"
          "https://cache.nixos.org"
          "https://nixpkgs-wayland.cachix.org"
          "https://unmatched.cachix.org"
          "https://nix-community.cachix.org"
          "https://nix-gaming.cachix.org"
        ];
        trusted-users = ["@wheel" "ramblurr" "root"];
      };
    };
  };
}
