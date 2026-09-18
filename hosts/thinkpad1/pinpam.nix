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
    polkit.enableAgentTpmAccess = true;
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
      # Login, KDE unlocking, and Polkit prompts accept Viki's PIN.
      services = [
        "login"
        "kde"
        "polkit-1"
      ];
      preferOrderBeforeUnix = false;
      fallbackOrder = 13000;
      control = "sufficient";
    };
  };

  security.pam.services = lib.genAttrs [ "login" "kde" "polkit-1" ] (service: {
    rules.auth = {
      # Password auth runs first. A PIN reuses the same field without another prompt.
      pinpam.args = [ "use_first_pass" ];
      # Polkit's deny rule precedes the login stacks' configured fallback order.
      pinpam.order = lib.mkIf (service == "polkit-1") (lib.mkForce 12000);
      pinpam-viki-only = {
        order = if service == "polkit-1" then 11999 else 12999;
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
