{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  extendedPkgs = import inputs.nixpkgs {
    system = pkgs.stdenv.hostPlatform.system;
    overlays = [ (import ../lib inputs) ];
  };
  evaluate =
    hostName: modules:
    (inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      specialArgs.lib = extendedPkgs.lib;
      modules = modules ++ [
        {
          nixpkgs.pkgs = extendedPkgs;
          networking.hostName = hostName;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
        }
      ];
    }).config;

  siteHost =
    hostName:
    evaluate hostName [
      ../config/site.nix
      ../modules/site-net
      {
        options.repo = {
          secrets = lib.mkOption { type = lib.types.attrs; };
          secretFiles = lib.mkOption { type = lib.types.attrs; };
        };
        config.repo.secrets.global = {
          domain.home = "example.test";
          email.home = "ops@example.test";
          pubKeys = [ ];
        };
      }
    ];
  timesyncdConf = hostName: (siteHost hostName).environment.etc."systemd/timesyncd.conf".text;

  expectedNtp = {
    debord = "10.9.4.1 10.9.8.1 10.9.10.1 172.20.20.1";
    dewey = "10.9.4.1 10.9.8.1 10.9.10.1 172.20.20.1";
    mali = "10.9.4.1 10.9.8.1 10.9.10.1";
    mootforge = "10.9.8.1 10.9.10.1";
    peirce = "10.9.4.1";
    quine = "10.9.4.1 10.9.8.1 10.9.10.1 172.20.20.1 10.8.70.1";
  };
  checkHost =
    hostName: servers:
    let
      conf = timesyncdConf hostName;
    in
    lib.assertMsg (lib.hasInfix "\nNTP=${servers}\n" conf) "${hostName} timesyncd.conf:\n${conf}";

  addams = timesyncdConf "addams";
  addamsChrony =
    (evaluate "addams" [
      ../hosts/addams/modules/chrony.nix
      {
        options.environment.persistence = lib.mkOption { type = lib.types.attrs; };
      }
    ]).systemd.services.chronyd.serviceConfig.ExecStart;
in
assert lib.all lib.id (lib.mapAttrsToList checkHost expectedNtp);
assert lib.assertMsg (!lib.hasInfix "\nNTP=" addams) addams;
pkgs.runCommand "ntp-by-address" { } ''
  conf=$(echo '${addamsChrony}' | grep -o '/nix/store/[^ ]*-chrony.conf')
  cat "$conf"
  grep -qx 'pool id.pool.ntp.org iburst' "$conf"
  grep -qx 'server 162.159.200.1 iburst' "$conf"
  grep -qx 'server 162.159.200.123 iburst' "$conf"
  if grep -q '^local' "$conf"; then
    echo "chrony must not serve its own clock as a reference while unsynced" >&2
    exit 1
  fi
  touch "$out"
''
