{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkOption
    types
    ;
  inherit (config.networking) hostName;
  cfg = config.home-ops.microvm-guest;
  nets = builtins.attrNames (
    lib.filterAttrs (
      _:
      { hosts4, hosts6, ... }:
      hosts4 ? ${hostName} || lib.filterAttrs (_: hosts6: hosts6 ? ${hostName}) hosts6 != { }
    ) config.site.net
  );

  generateMacAddress = net: lib.my.generateMacAddress "1-${net}-${hostName}";
in
{
  options.home-ops.microvm-guest = {
    host = mkOption {
      type = types.enum [ "quine" ];
      default = null;
      description = ''
        Server that is supposed to host this MicroVM.
      '';
    };

    hostFQDN = mkOption {
      description = ''
        FQDN of the host that is supposed to host this MicroVM.
      '';
    };

    autoNetSetup = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Automatically configure MicroVM network interfaces and
        systemd-networkd according to our network data.
      '';
    };

    mounts = mkOption {
      description = "Persistent filesystems to create, without leading /.";
      type = types.listOf types.str;
    };

    mountBase = mkOption {
      description = "Location (ZFS dataset, ...) where all the shares live.";
      type = types.path;
      default = "/var/lib/microvms/${hostName}";
    };

    nixStoreBackend = mkOption {
      type = types.enum [
        "virtiofs"
        "blk"
      ];
      default = "virtiofs";
      description = ''
        /nix/store via virtiofs from the host, or create
        an erofs block image for it.
      '';
    };

  };
  imports = [ ./common.nix ];
  config = {
    home-ops.microvm-guest.mounts = [
      "etc"
      "home"
      "var"
    ];

    # make mounts like /etc /home /var available early so that they can be used in system.activationScripts
    fileSystems = lib.genAttrs (map (x: "/" + x) cfg.mounts) (_: {
      neededForBoot = true;
    });
    microvm = {
      hypervisor = lib.mkDefault "cloud-hypervisor";
      deflateOnOOM = false;
      mem = lib.mkDefault 512;
      vcpu = lib.mkDefault 4;

      interfaces = lib.mkIf cfg.autoNetSetup (
        map (net: {
          type = "tap";
          id = builtins.substring 0 15 "${net}-${hostName}";
          mac = generateMacAddress net;
        }) nets
      );

      shares =
        lib.optional (cfg.nixStoreBackend == "virtiofs") {
          source = "/nix/store";
          mountPoint = "/nix/.ro-store";
          tag = "store";
          proto = "virtiofs";
          socket = "${cfg.mountBase}/store.socket";
        }
        ++ map (
          dir:
          if lib.hasPrefix "/" dir then
            throw "${dir} starts with a leading /. Just don't!"
          else
            let
              tag = builtins.replaceStrings [ "/" ] [ "_" ] dir;
            in
            {
              source = "${cfg.mountBase}/${dir}";
              mountPoint = "/${dir}";
              inherit tag;
              proto = "virtiofs";
              socket = "${cfg.mountBase}/${tag}.socket";
            }
        ) cfg.mounts;

      socket = "${cfg.mountBase}/${hostName}.sock";

      storeDiskErofsFlags = [ "-zlz4hc,level=5" ];

      virtiofsd = {
        threadPoolSize = 4;
        extraArgs = [
          "--allow-mmap" # requires virtiofsd > 1.10.1
          "--cache=auto"
          "--inode-file-handles=mandatory"
        ];
      };
    };

    networking = lib.mkIf cfg.autoNetSetup {
      useDHCP = false;
      dhcpcd.enable = false;
      useNetworkd = true;
    };

    systemd.network = lib.mkIf cfg.autoNetSetup {
      links = builtins.foldl' (
        links: net:
        links
        // {
          "30-${net}" = {
            # enable = true;
            matchConfig.MACAddress = generateMacAddress net;
            # rename interface to net name
            linkConfig.Name = net;
          };
        }
      ) { } nets;

      networks = builtins.foldl' (
        networks: net:
        networks
        // {
          "30-${net}" =
            let
              netCfg = config.site.net.${net};
              addresses =
                lib.optional (
                  netCfg.hosts4 ? ${hostName}
                ) "${netCfg.hosts4.${hostName}}/${toString netCfg.subnet4Len}"
                ++ map (hosts6: "${hosts6.${hostName}}/64") (
                  builtins.filter (hosts6: hosts6 ? ${hostName}) (builtins.attrValues netCfg.hosts6)
                );
              defaultGateways = {
                "svc" = "svc-gw";
              };
            in
            {
              matchConfig.MACAddress = generateMacAddress net;
              addresses = map (Address: { inherit Address; }) addresses;
              gateway = lib.mkIf (defaultGateways ? ${net}) (
                let
                  gw = defaultGateways.${net};
                in
                [ netCfg.hosts4.${gw} ]
                ++ map (hosts6: hosts6.${gw}) (
                  builtins.filter (hosts6: hosts6 ? ${gw}) (builtins.attrValues netCfg.hosts6)
                )
              );
            };
        }
      ) { } nets;
    };

    system.build = {
      copyToServer = pkgs.writeShellScript "copy-to-${cfg.host}" ''
        nix copy --no-check-sigs --to ssh-ng://root@${cfg.hostFQDN} $@
      '';

      runOnServer = pkgs.writeShellScript "run-on-${cfg.host}" ''
        # we cannot execute any other commands here because it grabs away $@
        ssh root@${cfg.hostFQDN} -- $@
      '';
    };
  };
}
