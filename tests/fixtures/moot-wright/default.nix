{ lib, pkgs, ... }:
let
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
in
{
  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "thingstead";
    hostFQDN = "thingstead.example.test";
    mootWright = {
      enable = true;
      name = "workstead";
      zvolSize = 4096;
      network = {
        tapId = "mw-workstead";
        hostAddress = "10.77.0.1";
        guestAddress = "10.77.0.2";
        dnsResolver = "198.51.100.10";
      };
      credentials = [ "GITHUB_TOKEN" ];
      nix = {
        substituters = [ "https://moot-cache.example.ts.net" ];
        trustedPublicKeys = [ "moot-cache:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" ];
      };
      extraPackages = [ pkgs.socat ];
    };
  };
  microvm = {
    mem = 3072;
    vcpu = 2;
  };
  users.users.root.openssh.authorizedKeys.keys = [ sshKeys.snakeOilEd25519PublicKey ];
  repo.secretFiles = {
    global = lib.mkForce ./global.nix;
    site = lib.mkForce ../immich/site.nix;
    local = lib.mkForce ../immich/local.nix;
  };
}
