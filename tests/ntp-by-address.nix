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
  pool = "0.nixos.pool.ntp.org 1.nixos.pool.ntp.org 2.nixos.pool.ntp.org 3.nixos.pool.ntp.org";

  expectedNtp = {
    debord = "10.9.10.1 10.9.8.1 10.9.4.1 172.20.20.1";
    dewey = "10.9.10.1 10.9.8.1 10.9.4.1 172.20.20.1";
    mali = "10.9.10.1 10.9.8.1 10.9.4.1";
    mootforge = "10.9.10.1 10.9.8.1";
    peirce = "10.9.4.1";
    quine = "10.9.10.1 10.9.8.1 10.9.4.1 172.20.20.1 10.8.70.1";
  };
  checkHost =
    hostName: servers:
    let
      conf = timesyncdConf hostName;
    in
    lib.assertMsg (lib.hasInfix "\nNTP=${servers} ${pool}\n" conf) "${hostName} timesyncd.conf:\n${conf}";

  addams = timesyncdConf "addams";
in
assert lib.all lib.id (lib.mapAttrsToList checkHost expectedNtp);
assert lib.assertMsg (!lib.hasInfix "\nNTP=" addams) addams;
pkgs.runCommand "ntp-by-address" { } ''
  touch "$out"
''
