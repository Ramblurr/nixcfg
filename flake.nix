{
  nixConfig = {
    extra-substituters = [
      "https://numtide.cachix.org"
      "https://nixos-raspberrypi.cachix.org"
      "https://cache.flox.dev"
    ];
    extra-trusted-public-keys = [
      "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
      "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
    ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager-stable.url = "github:nix-community/home-manager/release-25.11";
    home-manager-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    nixpkgs-mine.url = "git+https://github.com/ramblurr/nixpkgs?shallow=1&ref=consolidated";
    # for development
    #nixpkgs-mine.url = "path:/home/ramblurr/src/nixpkgs";

    nix-std.url = "github:chessai/nix-std";
    spdx-util.url = "https://flakehub.com/f/ramblurr/spdx-util/0.1.4";
    spdx-util.inputs.nixpkgs.follows = "nixpkgs";
    #boxai.url = "path:/home/ramblurr/src/llm/vms";
    #boxai.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";

    microvm.url = "github:astro/microvm.nix";
    #microvm.url = "path:/home/ramblurr/src/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";

    #cadquery.url = "github:vinszent/cq-flake/main";
    #cadquery.inputs.nixpkgs.follows = "nixpkgs";

    #firefox-gnome-theme.url = "github:rafaelmardojai/firefox-gnome-theme";
    #firefox-gnome-theme.flake = false;

    automatic-ripping-machine.url = "github:xieve/automatic-ripping-machine/main?dir=nixos";
    automatic-ripping-machine.inputs.nixpkgs.follows = "nixpkgs";

    #matugen.url = "github:InioX/matugen";
    #matugen.inputs.nixpkgs.follows = "nixpkgs";

    #radicle.url = "git+https://seed.radicle.xyz/z3gqcJUoA1n9HaHKufZs5FCSGazv5.git?rev=54aacc96197a48b79fcc260f94312d824f5e0a34";
    #radicle.inputs.nixpkgs.follows = "nixpkgs";
    #radicle.inputs.flake-utils.follows = "flake-utils";

    quadlet-nix2.url = "github:mirkolenz/quadlet-nix/main";

    quadlet-nix2.inputs.nixpkgs.follows = "nixpkgs";

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

    #nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi/dev";
    #nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs";
    #nixos-raspberrypi.inputs.nixos-hardware.follows = "nixos-hardware";

    #nixos-ovos.url = "github:ramblurr/ovos-rpi-nixos/dev";
    #nixos-ovos.inputs.nixpkgs.follows = "nixpkgs";
    #nixos-ovos.inputs.nixos-raspberrypi.follows = "nixos-raspberrypi";

    flake-utils.url = "github:numtide/flake-utils";

    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

    disko-unstable.url = "github:nix-community/disko";
    disko-unstable.inputs.nixpkgs.follows = "nixpkgs";

    disko-stable.url = "github:nix-community/disko";
    disko-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    crowdsec.url = "git+https://codeberg.org/kampka/nix-flake-crowdsec.git";
    crowdsec.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs-stable";

    firefox-nightly.url = "github:nix-community/flake-firefox-nightly";
    firefox-nightly.inputs.nixpkgs.follows = "nixpkgs";

    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    #sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.url = "github:ramblurr/sops-nix/age-plugin";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    #plasma-manager = {
    #  url = "github:nix-community/plasma-manager";
    #  inputs.nixpkgs.follows = "nixpkgs";
    #  inputs.home-manager.follows = "home-manager";
    #};

    nixos-extra-modules = {
      url = "github:oddlama/nixos-extra-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    nixos-nftables-firewall = {
      url = "github:thelegy/nixos-nftables-firewall";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Pin niri on commit where wtype  isn't broken
    # ref: https://github.com/YaLTeR/niri/issues/3394
    niri.url = "github:YaLTeR/niri?rev=3ccb06f5644c4bcdf74ad2e4d388a13ac65207af";
    niri.inputs.nixpkgs.follows = "nixpkgs";

    llm-agents.url = "github:numtide/llm-agents.nix";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs-stable";
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi";
    nad-api.url = "https://flakehub.com/f/ramblurr/nad-api/*";
    nad-api.inputs.nixpkgs.follows = "nixpkgs";
    tmux-buddy.url = "https://flakehub.com/f/ramblurr/tmux-buddy/*";
    tmux-buddy.inputs.nixpkgs.follows = "nixpkgs";
    #zmx.url = "github:neurosnap/zmx";
    #zmx.inputs.nixpkgs.follows = "nixpkgs";
    #brother_ql_web.url = "github:makefu/brother_ql_web";
    niri-scratchpad.url = "github:gvolpe/niri-scratchpad";
    niri-scratchpad.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./flake/hosts.nix
        ./flake/pkgs.nix
        ./flake/iso-test.nix
        ./flake/devshell.nix

      ];
      systems = [
        "x86_64-linux"
      ];
    };

}
