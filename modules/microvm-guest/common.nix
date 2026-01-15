{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.networking) hostName;
  cfg = config.modules.microvm-guest;
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

  config = lib.mkIf cfg.enable {

    assertions = [
      {
        assertion = cfg.autoNetSetup.enable -> nets != [ ];
        message = "autoNetSetup is enabled, but no networks are configured. Please add at least one network to config.site.net.svc";
      }
    ];
    # make mounts like /etc /home /var available early so that they can be used in system.activationScripts
    fileSystems = {
      "/" = lib.mkDefault {
        fsType = "tmpfs";
      };
    }
    // lib.genAttrs (map (x: "/" + x) cfg.mounts) (_: {
      neededForBoot = true;
    });
    microvm = {
      writableStoreOverlay = lib.mkIf cfg.writableStoreOverlay.enable "/nix/.rw-store";
      hypervisor = lib.mkDefault "cloud-hypervisor";
      deflateOnOOM = false;
      mem = lib.mkDefault 1024;
      vcpu = lib.mkDefault 4;

      interfaces = lib.mkIf cfg.autoNetSetup.enable (
        map (net: {
          type = "macvtap";
          id = builtins.substring 0 15 "${net}-${hostName}";
          mac = generateMacAddress net;
          macvtap.link = "vlan-svc";
          macvtap.mode = "bridge";
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

    networking = lib.mkIf cfg.autoNetSetup.enable {
      useDHCP = false;
      dhcpcd.enable = false;
      useNetworkd = false;
    };
    # TODO: this should be user provided, but just use the first network's gateway for now
    services.resolved.fallbackDns = [
      (
        let
          net = lib.mori.first nets;
          netCfg = config.site.net.${net};
          gw = netCfg.dhcp.router;
        in
        lib.mori.first netCfg.hosts4.${gw}
      )
    ];

    systemd.network = lib.mkIf cfg.autoNetSetup.enable {
      enable = true;
      wait-online.enable = true;
      wait-online.anyInterface = true;
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
              ip4s = lib.optionals (netCfg.hosts4 ? ${hostName}) (
                map (addr: "${addr}/${toString netCfg.subnet4Len}") netCfg.hosts4.${hostName}
              );
              ip6s = map (hosts6: "${hosts6.${hostName}}/64") (
                builtins.filter (hosts6: hosts6 ? ${hostName}) (builtins.attrValues netCfg.hosts6)
              );
            in
            {
              matchConfig.MACAddress = generateMacAddress net;
              address = ip4s ++ ip6s;
              gateway = lib.mkIf netCfg.dhcp.enable (
                let
                  gw = netCfg.dhcp.router;
                in
                [
                  (lib.mori.first netCfg.hosts4.${gw})
                ]
                ++ (lib.mori.mapcat (hosts6: hosts6.${gw}) (
                  builtins.filter (hosts6: hosts6 ? ${gw}) (builtins.attrValues netCfg.hosts6)
                ))
              );
            };
        }
      ) { } nets;
    };

    services.userborn.enable = true;

    sops.age.sshKeyPaths = lib.mkIf cfg.bootstrapSops.enable (
      lib.mkDefault [ "/etc/ssh/ssh_host_ed25519_key" ]
    );
    microvm.credentialFiles = lib.mkIf cfg.bootstrapSops.enable {
      "SOPS_AGE_KEY" = cfg.bootstrapSops.credentialHostPath;
    };
    systemd.services.bootstrap-secrets = lib.mkIf cfg.bootstrapSops.enable {
      wantedBy = [ "sysinit.target" ];
      after = [ "systemd-sysusers.service" ];
      before = [
        "sops-install-secrets.service"
        "sshd.service"
      ];
      serviceConfig = {
        ImportCredential = "SOPS_AGE_KEY";
        Type = "oneshot";
      };
      script = ''
        mkdir -p /etc/ssh
        cat $CREDENTIALS_DIRECTORY/SOPS_AGE_KEY > /etc/ssh/ssh_host_ed25519_key
        chmod 0600 /etc/ssh/ssh_host_ed25519_key
      '';
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

    boot = {
      loader.grub.enable = false;
      initrd.systemd.enable = true;
      initrd.kernelModules = [
        # required for net.netfilter.nf_conntrack_max appearing in sysfs early at boot
        "nf_conntrack"
      ];
      kernel.sysctl =
        let
          inherit (config.microvm) mem;
        in
        lib.optionalAttrs (mem <= 2 * 1024) {
          # table overflow causing packets from nginx to the service to drop
          # nf_conntrack: nf_conntrack: table full, dropping packet
          "net.netfilter.nf_conntrack_max" = lib.mkDefault "65536";
        };
      kernelParams = [
        # mitigations which cost the most performance and are the least real world relevant
        # NOTE: keep in sync with baremetal.nix
        "retbleed=off"
        "gather_data_sampling=off" # Downfall
      ];
    };

    hardware.enableRedistributableFirmware = false;

    # nix store is mounted read only
    #nix = {
    #  enable = lib.mkDefault false;
    #  gc.automatic = false;
    #  optimise.automatic = false;
    #};

    system.build.installBootLoader = "${pkgs.coreutils}/bin/true";

    systemd.tmpfiles.rules = [
      "d /home/root 0700 root root -" # createHome does not create it
    ];

    systemd.user.extraConfig = ''
      DefaultEnvironment="PATH=/run/current-system/sw/bin:/run/wrappers/bin:${lib.makeBinPath [ pkgs.bash ]}"
    '';

    users = {
      mutableUsers = false;
      users."root" = {
        createHome = true;
        home = lib.mkForce "/home/root";
      };
    };
    # Add some basic utilities to path.
    # These are basically free because they're on the host anyways.
    environment.systemPackages =
      with pkgs;
      [
        bmon
        curl
        nvd
        ncdu
        dig
        ethtool
        fd
        iproute2
        jq
        lsof
        nmap
        pv
        ripgrep
        rsync
        strace
        tree
        wget
      ]
      ++ (map (x: x.terminfo) (
        with pkgs.pkgsBuildBuild;
        [
          ghostty
          kitty
          tmux
          wezterm
        ]
      ));
  };
}
