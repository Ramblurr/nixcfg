{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.atuin;
  withImpermanence = config.modules.impermanence.enable;
  domain = config.repo.secrets.global.domain.home;
  # At the time of writing, `atuin login` only reads input from /dev/tty, so we can't just redirect stdin.
  #
  # The exact text matches specified in this expect script will be super brittle, but the advantage of that is we'll
  # get an early warning if `atuin login` is changed or improved.
  atuinLogin =
    username:
    pkgs.writeShellScript "atuin-login" ''
      ${pkgs.expect}/bin/expect -f ${pkgs.writeText "atuin-login.exp" ''
        set timeout 2
        set password [exec ${pkgs.coreutils}/bin/cat ${config.sops.secrets.atuin_password.path}]
        set key [exec ${pkgs.coreutils}/bin/cat ${config.sops.secrets.atuin_key.path}]

        log_user 0
        spawn ${pkgs.atuin}/bin/atuin login -u ${username}
        expect {
          "You are already logged in!" {
            puts "Already logged in!"
            exit 0
          }
          -exact "Please enter password: " {
            send "$password\n"
          }
          timeout {
            log_user 1
            puts "timeout!";
            exit 1
          }
        }
        expect {
          -exact {Please enter encryption key [blank to use existing key file]: } {
            log_user 0  # Disable logging just for sending the key
            send "$key\n"
            log_user 1  # Re-enable logging
          }
          timeout {
            log_user 1
            puts "timeout!";
            exit 1
          }
        }
        expect {
          eof {
            log_user 1
            puts "Login successful!"
          }
          timeout {
            log_user 1
            puts "timeout!";
            exit 1
          }
        }
      ''}
      echo "Done Expect script!"
    '';
in
{
  options.modules.shell.atuin = {
    enable = lib.mkEnableOption "";
    autoLogin.enable = lib.mkEnableOption "";
    syncing = {
      enable = lib.mkEnableOption "";
      address = lib.mkOption {
        type = lib.types.uniq lib.types.str;
        default = "https://atuin.${domain}";
      };
    };
  };
  config = lib.mkIf cfg.enable {

    sops.secrets.atuin_password = lib.mkIf cfg.autoLogin.enable {
      owner = "ramblurr";
    };
    sops.secrets.atuin_key = lib.mkIf cfg.autoLogin.enable {
      owner = "ramblurr";
    };

    myhm =
      { pkgs, config, ... }@hm:
      {
        programs.atuin = {
          enable = true;
          enableBashIntegration = true;
          settings = {
            style = "compact";
            update_check = false;
          }
          // lib.optionalAttrs cfg.syncing.enable {
            sync_address = cfg.syncing.address;
            auto_sync = true;
          };
        }
        // lib.optionalAttrs (builtins.hasAttr "daemon" hm.options.programs.atuin) {
          daemon.enable = true;
        };
        home.persistence."/persist" = mkIf withImpermanence {
          directories = [ ".config/atuin" ];
        };
        home.file = mkIf withImpermanence {
          ".local/share/atuin".source = config.lib.file.mkOutOfStoreSymlink "/persist/extra/atuin";
        };

        systemd.user.services.atuin-login = lib.mkIf cfg.autoLogin.enable {
          #Unit.Requires = [ "sops-nix.service" ];
          #Unit.After = [ "sops-nix.service" ];
          Service.Type = "oneshot";
          Service.RemainAfterExit = "true";
          Service.ExecStart = toString (atuinLogin hm.config.home.username);
          Install.WantedBy = [ "default.target" ];
        };

        systemd.user.timers.atuin-sync = lib.mkIf cfg.syncing.enable {
          Unit.Description = "Atuin auto sync";
          Timer.OnUnitActiveSec = "1h";
          Install.WantedBy = [ "timers.target" ];
        };

        systemd.user.services.atuin-sync = lib.mkIf cfg.syncing.enable {
          Unit.Description = "Atuin auto sync";
          Service = {
            Type = "oneshot";
            ExecStart = "${pkgs.atuin}/bin/atuin sync";
            IOSchedulingClass = "idle";
          };
        };
      };
  };
}
