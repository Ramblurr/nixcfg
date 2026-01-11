{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.editors.emacs;
  inherit (config.modules.users.primaryUser) homeDirectory username;
  withImpermanence = config.modules.impermanence.enable;
  nixosConfig = config;
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
    fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
    environment.wordlist.enable = true;
    environment.persistence."/persist".users.${username}.directories = lib.mkIf withImpermanence [
      ".config/emacs"
    ];
    myhm =
      { config, ... }:
      {
        programs.emacs = {
          enable = true;
          inherit (cfg) package;
          extraPackages = epkgs: [
            epkgs.eldev
            epkgs.vterm
            #epkgs.pdf-tools
            epkgs.treesit-grammars.with-all-grammars
            #epkgs.mu4e
          ];
        };

        systemd.user.services.emacs.Service = lib.mkIf nixosConfig.modules.dev.llms.enable {
          EnvironmentFile = [ "%h/.llm-keys" ];
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
          nodePackages.js-beautify # js/css/html format
          nodePackages.prettier
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
          (python3.withPackages (
            ps: with ps; [
              virtualenv
              black
              #python-lsp-black
              setuptools
            ]
          ))
          # :lang nix
          nixfmt
        ];
      };
  };
}
