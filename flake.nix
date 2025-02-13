{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    home-manager-stable.url = "github:nix-community/home-manager/release-24.11";
    home-manager-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    nixpkgs-mine.url = "github:ramblurr/nixpkgs/consolidated";
    #nixpkgs-mine.url = "path:/home/ramblurr/src/nixpkgs";

    nix-std.url = "github:chessai/nix-std";

    nixfmt.url = "github:serokell/nixfmt";

    microvm.url = "github:astro/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";

    cadquery.url = "github:vinszent/cq-flake/main";
    cadquery.inputs.nixpkgs.follows = "nixpkgs";

    firefox-gnome-theme.url = "github:rafaelmardojai/firefox-gnome-theme";
    firefox-gnome-theme.flake = false;

    anyrun.url = "github:anyrun-org/anyrun";
    anyrun.inputs.nixpkgs.follows = "nixpkgs";

    matugen.url = "github:InioX/matugen";
    matugen.inputs.nixpkgs.follows = "nixpkgs";

    #radicle.url = "git+https://seed.radicle.xyz/z3gqcJUoA1n9HaHKufZs5FCSGazv5.git?rev=54aacc96197a48b79fcc260f94312d824f5e0a34";
    #radicle.inputs.nixpkgs.follows = "nixpkgs";
    #radicle.inputs.flake-utils.follows = "flake-utils";

    #quadlet-nix.url = "github:Ramblurr/nixos-quadlet/feat-home-manager";
    quadlet-nix.url = "github:SEIAROTg/quadlet-nix/main";
    quadlet-nix.inputs.nixpkgs.follows = "nixpkgs";

    #nix-writers.url = "git+https://cgit.krebsco.de/nix-writers";
    #nix-writers.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    #clj-nix.url = "github:jlesquembre/clj-nix";
    #clj-nix.inputs.nixpkgs.follows = "nixpkgs";

    #clojure-lsp.url = "github:clojure-lsp/clojure-lsp";
    #clojure-lsp.inputs.nixpkgs.follows = "nixpkgs";
    #clojure-lsp.inputs.flake-utils.follows = "flake-utils";
    #clojure-lsp.inputs.clj-nix.follows = "clj-nix";

    #nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi";
    #nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs";
    #nixos-raspberrypi.inputs.nixos-hardware.follows = "nixos-hardware";

    nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi/dev";
    nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs";
    nixos-raspberrypi.inputs.nixos-hardware.follows = "nixos-hardware";

    nixos-ovos.url = "github:ramblurr/ovos-rpi-nixos/dev";
    nixos-ovos.inputs.nixpkgs.follows = "nixpkgs";
    nixos-ovos.inputs.nixos-raspberrypi.follows = "nixos-raspberrypi";

    flake-utils.url = "github:numtide/flake-utils";

    disko-unstable.url = "github:nix-community/disko";
    disko-unstable.inputs.nixpkgs.follows = "nixpkgs";

    disko-stable.url = "github:nix-community/disko";
    disko-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    #hyprland = {
    #  type = "git";
    #  url = "https://github.com/hyprwm/Hyprland";
    #  ref = "refs/tags/v0.47.1";
    #  submodules = true;
    #};

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs-stable";

    firefox-nightly.url = "github:colemickens/flake-firefox-nightly";
    firefox-nightly.inputs.nixpkgs.follows = "nixpkgs";

    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur.url = "github:nix-community/NUR";

    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    nixos-extra-modules = {
      url = "github:oddlama/nixos-extra-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    nixos-nftables-firewall = {
      url = "github:thelegy/nixos-nftables-firewall";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/hosts.nix
        ./nix/pkgs.nix
        ./nix/iso-test.nix
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    };

}
