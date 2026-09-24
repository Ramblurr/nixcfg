{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.microvm-host;
  registry = import ../../config/microvm-ssh.nix;
  host = registry.${config.networking.hostName} or null;
  identity = "/var/lib/microvm-ssh/id_ed25519";
  upstream = pkgs.callPackage "${inputs.microvm}/pkgs/microvm-command.nix" {
    inherit (config.microvm) stateDir;
  };
  # Retain upstream routing, but let our strict host-key policy take effect.
  verifiedMicrovm = pkgs.runCommand "microvm-verified-ssh" { } ''
    mkdir -p "$out/bin"
    cp ${upstream}/bin/microvm "$out/bin/microvm"
    chmod u+w "$out/bin/microvm"
    substituteInPlace "$out/bin/microvm" --replace-fail \
      'ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null' 'ssh'
  '';
in
{
  options.modules.microvm-host = {
    enable = lib.mkEnableOption "Enable microvm host services (for imperative control!)";
    baseZfsDataset = lib.mkOption {
      type = lib.types.str;
      description = "Base ZFS dataset under which microvm shares are created.";
    };
  };
  config = lib.mkIf cfg.enable {
    microvm = {
      host.enable = true;
      # TODO autostart = [ ];
    };

    assertions = lib.optional (host != null) {
      assertion =
        let
          cids = map (guest: guest.cid) (builtins.attrValues host.guests);
        in
        builtins.length cids == builtins.length (lib.unique cids);
      message = "Host-owned MicroVM SSH requires unique VSOCK CIDs on each host.";
    };
    environment.systemPackages = lib.optional (host != null) (lib.hiPrio verifiedMicrovm);
    programs.ssh = lib.mkIf (host != null) {
      extraConfig = lib.mkBefore (
        lib.concatStringsSep "\n" (
          lib.mapAttrsToList (name: guest: ''
            Host vsock/${toString guest.cid} vsock-mux/${config.microvm.stateDir}/${name}/notify.vsock
              User root
              IdentityFile ${identity}
              IdentityAgent none
              IdentitiesOnly yes
              BatchMode yes
              HostKeyAlias microvm-${name}
              StrictHostKeyChecking yes
              UserKnownHostsFile /dev/null
          '') host.guests
        )
      );
      knownHosts = lib.mapAttrs' (
        name: guest:
        lib.nameValuePair "microvm-${name}" {
          hostNames = [ "microvm-${name}" ];
          publicKeyFile = guest.hostKeyFile;
        }
      ) host.guests;
    };

    # create the state directory for our microvms
    # this doesn't get its own zfs dataset, because the vm shares themselves will
    # be mounted under here
    environment.persistence."/persist".directories = [
      "/var/lib/microvms"
    ]
    ++ lib.optional (host != null) {
      directory = "/var/lib/microvm-ssh";
      user = "root";
      group = "root";
      mode = "0700";
    };
    systemd.tmpfiles.rules = [ "d /persist/var/lib/microvms 0770 microvm kvm" ];

    # allow microvm access to zvol
    users.users.microvm.extraGroups = [ "disk" ];

    # systemd services to ensure the ZFS datasets for the microvms are created
    systemd.services = {
      # MACVTAP parents must exist before guest interface setup runs.
      "microvm-macvtap-interfaces@" = {
        requires = [ "systemd-networkd-wait-online.service" ];
        after = [ "systemd-networkd-wait-online.service" ];
      };
      "microvm-virtiofsd@" = {
        requires = [ "microvm-zfs-datasets@%i.service" ];
      };
      "microvm-zfs-datasets@" = {
        description = "Create ZFS datasets for MicroVM '%i'";
        before = [ "microvm-virtiofsd@%i.service" ];
        after = [
          "local-fs.target"
          "zfs-datasets.service"
          # The installer supplies current/share/microvm/virtiofs on first boot.
          "install-microvm-%i.service"
        ];
        partOf = [ "microvm@%i.service" ];
        unitConfig.ConditionPathExists = "/var/lib/microvms/%i/current/share/microvm/virtiofs";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          WorkingDirectory = "/var/lib/microvms/%i";
          SyslogIdentifier = "microvm-zfs-datasets@%i";
        };
        path = with pkgs; [ zfs ];
        scriptArgs = "%i";
        script = # bash
          ''
            zfsExists() {
              zfs list $1 >/dev/null 2>/dev/null
            }

            NAME="$1"
            BASE="${cfg.baseZfsDataset}"
            zfsExists $BASE || \
              zfs create $BASE
            zfsExists $BASE/$NAME || \
              zfs create $BASE/$NAME
            for d in current/share/microvm/virtiofs/*; do
              SOURCE=$(cat $d/source)
              TAG=$(basename $d)
              MNT=$SOURCE
              if [[ "$MNT" == /var/lib/microvms/$NAME/* ]]; then
                zfsExists $BASE/$NAME/$TAG || \
                  zfs create -o mountpoint=$MNT $BASE/$NAME/$TAG
              fi
            done
          '';
      };
    };

    nix.settings = {
      min-free =
        10 # gb
        * 1024
        * 1024
        * 1024;
      max-free =
        20 # gb
        * 1024
        * 1024
        * 1024;
    };
  };
}
