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
    # TODO(upgrade) to fonts.packages once everything is on 23.11
    fonts.fonts = [pkgs.emacs-all-the-icons-fonts];

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
        package = pkgs.emacs29;
        extraPackages = epkgs: [epkgs.vterm];
      };

      services.emacs.enable = true;
      systemd.user.services.emacs.Unit = {
        requires = ["graphical-session.target"];
        partOf = ["graphical-session.target"];
        WantedBy = ["graphical-session.target"];
      };

      sops.secrets.authinfo = {
        mode = "0400";
        path = ".authinfo";
      };

      home.packages = with pkgs; [
        ## Doom dependencies
        git
        zoxide
        (ripgrep.override {withPCRE2 = true;})
        gnutls # for TLS connectivity
        kitty

        ## Optional dependencies
        dockfmt # docker
        clang-tools # java format
        pandoc # markdown
        shfmt # shell format
        html-tidy # html format
        stylelint # css lint
        nodePackages.js-beautify # js/css/html format
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
        (python311.withPackages (ps:
          with ps; [
            virtualenv
            black
            python-lsp-black
            setuptools
          ]))

        # :lang nix
        (stdenv.mkDerivation {
          name = "alejandra-posing-as-nixfmt";
          buildInputs = [alejandra];
          phases = ["installPhase"];
          installPhase = ''
            mkdir -p $out/bin
            cat <<EOF > $out/bin/nixfmt
            #!/bin/sh
            exec ${alejandra}/bin/alejandra --quiet "\$@"
            EOF
            chmod +x $out/bin/nixfmt
          '';
        })
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
          Exec=emacsclient -c %F
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
