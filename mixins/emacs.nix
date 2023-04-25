{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  services.emacs.enable = true;

  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
    };

    home.persistence = {
      "/persist/home/ramblurr".directories = ["${hm.config.xdg.configHome}/emacs"];
    };
    home.file.".local/share/icons/doom.png" = {
      source = ../configs/icons/doom.png;
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

    home.packages = with pkgs; [
      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls # for TLS connectivity

      ## Optional dependencies
      fd # faster projectile indexing
      imagemagick # for image-dired
      zstd # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [en en-computers en-science de]))
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
  };

  fonts.fonts = [pkgs.emacs-all-the-icons-fonts];

  system.userActivationScripts = {
    installDoomEmacs = ''
      if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
         git clone --depth=1 --single-branch "" "$XDG_CONFIG_HOME/emacs"
      fi
    '';
  };
}
