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
