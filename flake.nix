{
  #nixConfig = {
  #  extra-substituters = [
  #    "https://numtide.cachix.org"
  #    "https://nixos-raspberrypi.cachix.org"
  #    "https://cache.flox.dev"
  #  ];
  #  extra-trusted-public-keys = [
  #    "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
  #    "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
  #    "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
  #  ];
  #};
  inputs = {
    automatic-ripping-machine.url = "github:xieve/automatic-ripping-machine/main?dir=nixos";
    automatic-ripping-machine.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    # do not follow nixpkgs: use determinate's pinned nixpkgs for nixd integration
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
    disko-stable.url = "github:nix-community/disko";
    disko-stable.inputs.nixpkgs.follows = "nixpkgs-stable";
    disko-unstable.url = "github:nix-community/disko";
    disko-unstable.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    firefox-nightly.url = "github:nix-community/flake-firefox-nightly";
    firefox-nightly.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    git-lines.url = "github:omegaice/git-lines";
    git-lines.inputs.advisory-db.follows = "";
    git-lines.inputs.flake-parts.follows = "flake-parts";
    git-lines.inputs.git-hooks-nix.follows = "pre-commit-hooks";
    git-lines.inputs.nixpkgs.follows = "nixpkgs";
    git-lines.inputs.treefmt-nix.follows = "treefmt-nix";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager-stable.url = "github:nix-community/home-manager/release-25.11";
    home-manager-stable.inputs.nixpkgs.follows = "nixpkgs-stable";
    impermanence.url = "github:nix-community/impermanence";
    # do not follow home-manager: impermanence does not need it when consumed as a module
    impermanence.inputs.home-manager.follows = "";
    # do not follow nixpkgs: impermanence does not need it when consumed as a module
    impermanence.inputs.nixpkgs.follows = "";
    # do not follow nixpkgs: use llm-agents' nixpkgs for their cache
    llm-agents.url = "github:numtide/llm-agents.nix";
    microvm.url = "github:microvm-nix/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";
    nad-api.url = "https://flakehub.com/f/ramblurr/nad-api/*";
    nad-api.inputs.nixpkgs.follows = "nixpkgs";
    # Pin niri on commit where wtype isn't broken; ref: https://github.com/YaLTeR/niri/issues/3394
    niri.url = "github:YaLTeR/niri?rev=3ccb06f5644c4bcdf74ad2e4d388a13ac65207af";
    niri.inputs.nixpkgs.follows = "nixpkgs";
    niri-scratchpad.url = "github:gvolpe/niri-scratchpad";
    niri-scratchpad.inputs.nixpkgs.follows = "nixpkgs";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";
    nix-std.url = "github:chessai/nix-std";
    nixos-extra-modules.url = "github:oddlama/nixos-extra-modules";
    nixos-extra-modules.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    # do not follow nixpkgs: keep nixos-generators on nixpkgs-stable for image builds
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs-stable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-hardware.inputs.nixpkgs.follows = "nixpkgs";
    nixos-nftables-firewall.url = "github:thelegy/nixos-nftables-firewall";
    nixos-nftables-firewall.inputs.nixpkgs.follows = "nixpkgs";
    # do not follow nixpkgs: use nixos-raspberrypi's nixpkgs for their cache
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-mine.url = "git+https://github.com/ramblurr/nixpkgs?shallow=1&ref=consolidated";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    quadlet-nix2.url = "github:mirkolenz/quadlet-nix/main";
    quadlet-nix2.inputs.nixpkgs.follows = "nixpkgs";
    # do not follow nixpkgs: use ramsevka's own cached font build
    ramsevka.url = "github:ramblurr/iosevka-custom";
    #sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.url = "github:ramblurr/sops-nix/age-plugin";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    spdx-util.url = "https://flakehub.com/f/ramblurr/spdx-util/0.1.4";
    spdx-util.inputs.nixpkgs.follows = "nixpkgs";
    tmux-buddy.url = "https://flakehub.com/f/ramblurr/tmux-buddy/*";
    tmux-buddy.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    #boxai.url = "path:/home/ramblurr/src/llm/vms";
    #boxai.inputs.nixpkgs.follows = "nixpkgs";
    #brother_ql_web.url = "github:makefu/brother_ql_web";
    #cadquery.url = "github:vinszent/cq-flake/main";
    #cadquery.inputs.nixpkgs.follows = "nixpkgs";
    #clj-nix.url = "github:jlesquembre/clj-nix";
    #clj-nix.inputs.nixpkgs.follows = "nixpkgs";
    #clojure-lsp.url = "github:clojure-lsp/clojure-lsp";
    #clojure-lsp.inputs.clj-nix.follows = "clj-nix";
    #clojure-lsp.inputs.flake-utils.follows = "flake-utils";
    #clojure-lsp.inputs.nixpkgs.follows = "nixpkgs";
    #firefox-gnome-theme.url = "github:rafaelmardojai/firefox-gnome-theme";
    #firefox-gnome-theme.flake = false;
    #nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi";
    #nixos-raspberrypi.inputs.nixos-hardware.follows = "nixos-hardware";
    #nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs";
    #nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi/dev";
    #nixos-raspberrypi.inputs.nixos-hardware.follows = "nixos-hardware";
    #nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs";
    #nixpkgs-mine.url = "path:/home/ramblurr/src/nixpkgs";
    #plasma-manager.url = "github:nix-community/plasma-manager";
    #plasma-manager.inputs.home-manager.follows = "home-manager";
    #plasma-manager.inputs.nixpkgs.follows = "nixpkgs";
    #radicle.url = "git+https://seed.radicle.xyz/z3gqcJUoA1n9HaHKufZs5FCSGazv5.git?rev=54aacc96197a48b79fcc260f94312d824f5e0a34";
    #radicle.inputs.flake-utils.follows = "flake-utils";
    #radicle.inputs.nixpkgs.follows = "nixpkgs";
    #sops-nix.url = "github:Mic92/sops-nix";
    #zmx.url = "github:neurosnap/zmx";
    #zmx.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./flake/hosts.nix
        ./flake/pkgs.nix
        #./flake/iso-test.nix
        ./flake/devshell.nix
      ];
      systems = [
        "x86_64-linux"
      ];
    };

}
