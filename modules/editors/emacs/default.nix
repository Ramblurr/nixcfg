{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.editors.emacs;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.editors.emacs = {
    enable = lib.mkEnableOption "";
    package = lib.mkPackageOption pkgs "emacs29-pgtk" { };
  };
  config = mkIf cfg.enable {
    fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
    environment.wordlist.enable = true;
    myhm =
      { ... }@hm:
      {
        programs.emacs = {
          enable = true;
          package = cfg.package;
          extraPackages = epkgs: [
            epkgs.vterm
            epkgs.pdf-tools
            epkgs.treesit-grammars.with-all-grammars
            epkgs.mu4e
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

        home.packages = with pkgs; [
          ## Some emacs package dependencies
          ffmpegthumbnailer
          mediainfo
          git
          zoxide
          (ripgrep.override { withPCRE2 = true; })
          gnutls # for TLS connectivity
          kitty

          ## Optional dependencies
          openscad-lsp # openscad
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
          # :lang beancount
          beancount
          (python3.withPackages (
            ps: with ps; [
              virtualenv
              black
              #python-lsp-black
              setuptools
            ]
          ))
          # :lang nix
          nixfmt-rfc-style
          #inputs.nixfmt.packages.${pkgs.hostPlatform.system}.nixfmt-rfc-style
        ];
        persistence = lib.mkIf withImpermanence {
          directories = [
            ".emacs.doom"
            ".emacs.corgi"
            ".emacs.d"
          ];
        };
        home.file.".doom.d" = {
          # Get Doom Emacs
          source = ./configs/doom.d; # Sets up symlink name ".doom.d" for file "doom.d"
          recursive = true; # symlink the whole dirj
          # onChange = builtins.readFile ./configs/doom.sh; # If an edit is detected, it will run this script. Pretty much the same as what is now in default.nix but actually stating the terminal and adding the disown flag to it won't time out
        };
      };
  };
}
