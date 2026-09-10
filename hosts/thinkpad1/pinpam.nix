{
  inputs,
  lib,
  pkgs,
  ...
}:
{
  imports = [ inputs.pinpam.nixosModules.default ];

  security.pinpam = {
    enable = true;
    # Upstream's custom phase list omits checks and ELF fixups.
    package = inputs.pinpam.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
      phases = [
        "unpackPhase"
        "patchPhase"
        "configurePhase"
        "buildPhase"
        "checkPhase"
        "installPhase"
        "fixupPhase"
      ];
      nativeCheckInputs = (old.nativeCheckInputs or [ ]) ++ [ pkgs.swtpm ];
    });
    pin.policy = {
      minLength = 6;
      maxLength = 6;
      maxAttempts = 5;
    };
    auth = {
      enable = true;
      # Plasma Login Manager uses the login substack; KDE uses kde for unlocking.
      services = [
        "login"
        "kde"
      ];
      preferOrderBeforeUnix = false;
      fallbackOrder = 13000;
      control = "sufficient";
    };
  };

  security.pam.services = lib.genAttrs [ "login" "kde" ] (_: {
    rules.auth = {
      # Password auth runs first. A PIN reuses the same field without another prompt.
      pinpam.args = [ "use_first_pass" ];
      pinpam-viki-only = {
        order = 12999;
        control = "[success=1 default=ignore]";
        modulePath = "${pkgs.linux-pam}/lib/security/pam_succeed_if.so";
        args = [
          "user"
          "!="
          "viki"
          "quiet"
        ];
      };
    };
  });
}
