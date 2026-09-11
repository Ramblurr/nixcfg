{ config, pkgs, ... }:
let
  loginOrder = config.security.pam.services.plasmalogin.rules.auth.login.order;
in
{
  security.pinpam.masterKey.enable = true;

  # PLM already contains authentication in its login substack. Keep the existing
  # password/PIN decision and Viki guard there; stamp the wallet key afterward.
  security.pam.services.plasmalogin.rules.auth = {
    wallet-viki-only = {
      order = loginOrder + 1;
      control = "[success=1 default=ignore]";
      modulePath = "${pkgs.linux-pam}/lib/security/pam_succeed_if.so";
      args = [
        "user"
        "!="
        "viki"
        "quiet"
      ];
    };
    wallet-master-key = {
      order = loginOrder + 2;
      control = "optional";
      modulePath = "${config.security.pinpam.package}/lib/security/libpinpam_master_key.so";
    };
    # pam_set_data replaces the earlier capture in login, before its session
    # stage opens the wallet. Other users retain their ordinary password token.
    wallet-capture = {
      inherit (config.security.pam.services.login.rules.auth.kwallet) modulePath args;
      order = loginOrder + 3;
      control = "optional";
    };
  };
}
