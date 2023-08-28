{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.editors.emacs;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.emacs.enable = true;
    fonts.packages = [pkgs.emacs-all-the-icons-fonts];

    system.userActivationScripts = {
      # Installation script every time nixos-rebuild is run. So not during initial install.
      doomEmacs = {
        text = ''
          source ${config.system.build.setEnvironment}
          EMACS="/persist${homeDirectory}/.emacs.d"

          if [ ! -d "$EMACS" ]; then
            ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git $EMACS
            yes | $EMACS/bin/doom install
            $EMACS/bin/doom sync
          else
            $EMACS/bin/doom sync
          fi
        ''; # It will always sync when rebuild is done. So changes will always be applied.
      };
    };
    myhm = {...} @ hm: {
      programs.emacs = {
        enable = true;
        package = pkgs.emacs;
      };
      home.packages = with pkgs; [
        ## Doom dependencies
        git
        (ripgrep.override {withPCRE2 = true;})
        gnutls # for TLS connectivity
        kitty

        ## Optional dependencies
        fd # faster projectile indexing
        imagemagick # for image-dired
        zstd # for undo-fu-session/undo-tree compression

        ## Module dependencies
        # :checkers spell
        (aspellWithDicts (ds: with ds; [en en-computers en-science de]))
        hunspell
        hunspellDicts.en_US
        hunspellDicts.de_AT

        # :tools editorconfig
        editorconfig-core-c # per-project style config
        # :tools lookup & :lang org +roam
        sqlite
        # :lang latex & :lang org (latex previews)
        texlive.combined.scheme-medium
        # :lang beancount
        beancount
        fava
      ];
      persistence = mkIf withImpermanence {
        directories = [
          ".emacs.d"
        ];
      };
      home.file.".doom.d" = {
        # Get Doom Emacs
        source = ./configs/doom.d; # Sets up symlink name ".doom.d" for file "doom.d"
        recursive = true; # symlink the whole dirj
        onChange = builtins.readFile ./configs/doom.sh; # If an edit is detected, it will run this script. Pretty much the same as what is now in default.nix but actually stating the terminal and adding the disown flag to it won't time out
      };

      home.file.".local/share/icons/doom.png" = {
        source = ./configs/icons/doom.png;
        recursive = true;
      };
      home.file.".local/share/applications/doom.desktop" = {
        text = ''
          [Desktop Entry]
          Name=Doom
          GenericName=Text Editor
          Comment=Edit text
          MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;text/x-markdown;text/html;application/xhtml+xml
          Exec=emacs %F
          Icon=${hm.config.home.homeDirectory}/.local/share/icons/doom.png
          Type=Application
          Terminal=false
          Categories=Development;TextEditor;
          StartupWMClass=Doom
          StartupNotify=true
          Keywords=Text;Editor;
        '';
      };
    };
  };
}
