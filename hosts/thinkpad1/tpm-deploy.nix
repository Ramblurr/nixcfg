{
  config,
  lib,
  pkgs,
  ...
}:
let
  boot = config.boot.loader;
  helper = pkgs.writeShellApplication {
    name = "thinkpad1-tpm-deploy";
    runtimeInputs = [
      pkgs.python3
      pkgs.cryptsetup
      config.systemd.package
      config.nix.package
    ];
    text = ''
      export TPM_DEPLOY_DEVICE=${lib.escapeShellArg config.boot.initrd.luks.devices.cryptroot.device}
      export LD_LIBRARY_PATH=${config.systemd.package}/lib/cryptsetup
      export TPM_DEPLOY_PCRLOCK=${config.systemd.package}/lib/systemd/systemd-pcrlock
      exec python3 ${./tpm-deploy.py} "$@"
    '';
  };
in
{
  assertions = [
    {
      assertion =
        boot.systemd-boot.enable
        && boot.efi.efiSysMountPoint == "/boot"
        && boot.systemd-boot.xbootldrMountPoint == null
        && !(boot.systemd-boot.bootCounting.enable or false)
        && !boot.systemd-boot.rebootForBitlocker;
      message = "thinkpad1 TPM deployment requires systemd-boot on /boot without XBOOTLDR, boot counting, or BitLocker reboot handling.";
    }
    {
      assertion =
        config.boot.initrd.systemd.enable
        && config.boot.initrd.systemd.tpm2.enable
        && (config.boot.initrd.luks.devices ? cryptroot)
        && lib.elem "tpm2-device=auto" config.boot.initrd.luks.devices.cryptroot.crypttabExtraOpts;
      message = "thinkpad1 TPM deployment requires systemd initrd TPM support and automatic TPM unlock for cryptroot.";
    }
  ];

  # Host-local deployment entry point, also exercised by the encrypted VM test.
  system.systemBuilderCommands = lib.mkAfter ''
    ln -s ${helper}/bin/thinkpad1-tpm-deploy "$out/thinkpad1-tpm-deploy"
  '';
}
