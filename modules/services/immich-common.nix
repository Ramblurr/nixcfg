{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  cfg = config.services.immich;
  mediaMount = "${utils.escapeSystemdPath cfg.mediaLocation}.mount";
in
{
  assertions = [
    {
      assertion = cfg.settings == null;
      message = "This Immich deployment keeps application settings editable in the admin UI.";
    }
    {
      assertion = (config.fileSystems.${cfg.mediaLocation}.fsType or "") == "nfs";
      message = "Immich media must be a declared NFS mount.";
    }
  ];

  services.immich.settings = null;
  modules.mount-retry.mountPoints = [ cfg.mediaLocation ];
  systemd.services.immich-server = {
    wantedBy = lib.mkForce [ mediaMount ];
    after = [
      "network-online.target"
      mediaMount
    ];
    wants = [ "network-online.target" ];
    bindsTo = [ mediaMount ];
    unitConfig.RequiresMountsFor = [ cfg.mediaLocation ];
    preStart = lib.mkBefore ''
      ${lib.getExe' pkgs.util-linux "mountpoint"} -q ${lib.escapeShellArg cfg.mediaLocation}
    '';
    # Mali owns the NFS root. Client root is squashed and must not chown it.
    serviceConfig.StateDirectory = lib.mkForce "";
  };
  systemd.tmpfiles.settings.immich = lib.mkForce { };
}
