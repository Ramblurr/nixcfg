{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.editors.emacs;
  inherit (config.modules.users.primaryUser) homeDirectory;
  localIssues = pkgs.runCommandLocal "local-issues" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
    mkdir -p "$out/bin" "$out/lisp"
    install -m755 ${../../../configs/doom/bin/local-issues} "$out/bin/local-issues"
    install -m644 ${../../../configs/doom/lisp/local-issues-core.el} "$out/lisp/local-issues-core.el"
    wrapProgram "$out/bin/local-issues" \
      --suffix PATH : ${lib.makeBinPath [ cfg.package ]}
  '';
in
{
  options.modules.editors.emacs = {
    enable = lib.mkEnableOption "";
    package = lib.mkOption { type = lib.types.package; }; # $lib.mkPackageOption pkgs "emacs30-pgtk" { };
    localDoomConfigRepo = lib.mkOption {
      type = lib.types.str;
      default = "${homeDirectory}/nixcfg/configs/doom";
      description = "The WiFi interface to use";
    };

  };
  config = mkIf cfg.enable {
    fonts.packages = [
      pkgs.emacs-all-the-icons-fonts
      pkgs.symbola
    ];
    environment.wordlist.enable = true;
    myhm =
      { config, ... }:
      {
        imports = [ inputs.nix-doom-emacs-unstraightened.homeModule ];

        programs.doom-emacs = {
          enable = true;
          doomDir = ../../../configs/doom;
          doomLocalDir = "${config.xdg.dataHome}/nix-doom-ndeu";
          emacs = cfg.package;
          experimentalFetchTree = true;
          # NDEU provides emacs/emacsclient and backs the managed user daemon.
          provideEmacs = true;
          extraPackages = epkgs: [
            epkgs.eldev
            # epkgs.tramp-rpc # Disabled until its Nix sandbox tests are fixed.
            epkgs.vterm
            epkgs.treesit-grammars.with-all-grammars
          ];
        };

        services.emacs = {
          enable = true;
          client.enable = true;
          startWithUserSession = "graphical";
        };

        sops.secrets.authinfo = {
          mode = "0400";
          path = ".authinfo";
        };

        xdg.configFile.doom.source = config.lib.file.mkOutOfStoreSymlink cfg.localDoomConfigRepo;

        home.packages = with pkgs; [
          localIssues
          symbola
          ## Some emacs package dependencies
          ffmpegthumbnailer
          copilot-language-server
          mediainfo
          git
          zoxide
          (ripgrep.override { withPCRE2 = true; })
          gnutls # for TLS connectivity
          ## Optional dependencies
          openscad-lsp
          dockfmt # docker
          clang-tools # java format
          pandoc # markdown
          shfmt # shell format
          html-tidy # html format
          stylelint # css lint
          pkgs."js-beautify" # js/css/html format
          pkgs.prettier
          fd # faster projectile indexing
          imagemagick # for image-dired
          zstd # for undo-fu-session/undo-tree compression

          ## Module dependencies
          # :checkers spell
          (aspellWithDicts (
            ds: with ds; [
              en
              en-computers
              en-science
              de
            ]
          ))

          # :tools editorconfig
          editorconfig-core-c # per-project style config
          # :tools lookup & :lang org +roam
          sqlite
          # :lang latex & :lang org (latex previews)
          texlive.combined.scheme-medium
          # :lang nix
          nixfmt
        ];
      };
  };
}
