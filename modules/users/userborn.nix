{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.userborn;
  enabled = config.modules.users.enable;
  persistent = cfg.enable && config.modules.impermanence.enable;
  # The stable channel still pairs Userborn 0.5 with a module without subid support.
  # Native support: https://github.com/nikstur/userborn/pull/48
  nativeSubids = cfg.enable && lib.versionAtLeast cfg.package.version "1.0.0";
  rangeFor = name: uid: {
    # Preserve the deployed rootless-container mappings, including this older range.
    start = if name == "invoiceninja2" then 362144 else 100000 + uid * 65536;
    count = 65536;
  };
in
{
  options.users.users = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { config, name, ... }:
        let
          range = rangeFor name config.uid;
        in
        {
          config = lib.mkIf (enabled && nativeSubids && config.isNormalUser && config.uid != null) {
            subUidRanges = lib.mkDefault [
              {
                startUid = range.start;
                inherit (range) count;
              }
            ];
            subGidRanges = lib.mkDefault [
              {
                startGid = range.start;
                inherit (range) count;
              }
            ];
          };
        }
      )
    );
  };

  config = lib.mkIf enabled {
    # Userborn preserves identities in these files, not the old /var/lib/nixos maps.
    # https://github.com/nikstur/userborn/issues/35
    services.userborn.passwordFilesLocation = lib.mkIf persistent "/var/lib/userborn";
    # Use a native early bind: impermanence's fixed initrd path list omits this directory.
    # Existing hosts must seed the source before activation; see docs/userborn-persistence.md.
    fileSystems."/var/lib/userborn" = lib.mkIf persistent {
      device = "/persist/var/lib/userborn";
      fsType = "none";
      options = [ "bind" ];
      neededForBoot = true;
    };

    # An empty/partial migration must not silently allocate a new identity database.
    systemd.services.userborn.serviceConfig.ExecStartPre = lib.mkIf persistent (
      lib.mkBefore (
        map (file: "${pkgs.coreutils}/bin/test -s /var/lib/userborn/${file}") [
          "passwd"
          "group"
          "shadow"
        ]
        ++ lib.optionals nativeSubids (
          map (file: "${pkgs.coreutils}/bin/test -f /var/lib/userborn/${file}") [
            "subuid"
            "subgid"
          ]
        )
      )
    );

    # Keep the pre-1.0 workaround only where the NixOS module cannot manage subids.
    # https://github.com/nikstur/userborn/issues/7
    environment.etc = lib.mkIf (!nativeSubids) (
      let
        lines = lib.concatStrings (
          lib.mapAttrsToList (
            name: user:
            if user.isNormalUser && user.uid != null then
              let
                range = rangeFor name user.uid;
              in
              "${name}:${toString range.start}:${toString range.count}\n"
            else
              ""
          ) config.users.users
        );
      in
      {
        "subuid" = {
          text = lines;
          mode = "0444";
        };
        "subgid" = {
          text = lines;
          mode = "0444";
        };
      }
    );
  };
}
