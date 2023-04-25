{
  pkgs,
  inputs,
  ...
}: {
  config = {
    services.pcscd.enable = true;
    home-manager.users.ramblurr = {pkgs, ...} @ hm: {
      services.gpg-agent = {
        enable = true;
        enableSshSupport = true;
        enableZshIntegration = true;
        defaultCacheTtl = 60;
        maxCacheTtl = 120;
        enableExtraSocket = true;
        enableBashIntegration = true;
        pinentryFlavor = "qt";
        sshKeys = ["978C4D08058BA26EB97CB51820782DBCACFAACDA"];
      };
      programs.gpg = {
        enable = true;
        homedir = "${hm.config.xdg.configHome}/.gnupg";
        publicKeys = [
          {
            source = ../configs/casey-pub.asc;
            trust = "ultimate";
          }
        ];
        scdaemonSettings = {
          disable-ccid = true;
          card-timeout = "2";
          pcsc-shared = true;
          debug-level = "basic";
          log-file = "${hm.config.home.homeDirectory}/.cache/scdaemon.log";
        };
      };

      home.persistence."/persist/home/ramblurr" = {
        directories = [
          "${hm.config.xdg.configHome}/.gnupg"
        ];
      };
    };
  };
}
