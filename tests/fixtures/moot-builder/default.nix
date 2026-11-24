{ lib, pkgs, ... }:
let
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
in
{
  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "mootforge";
    hostFQDN = "mootforge.example.test";
    mootVm = {
      zvolSize = 4096;
      network = {
        tapId = "mb-anvil";
        hostAddress = "10.78.0.1";
        guestAddress = "10.78.0.2";
        dnsResolver = "198.51.100.10";
      };
    };
    mootBuilder.enable = true;
  };
  microvm = {
    mem = 2048;
    vcpu = 2;
  };
  environment.systemPackages = [ pkgs.socat ];
  users.users.root.openssh.authorizedKeys.keys = [ sshKeys.snakeOilEd25519PublicKey ];
  repo.secretFiles = {
    global = lib.mkForce ../moot-wright/global.nix;
    site = lib.mkForce ../immich/site.nix;
    local = lib.mkForce ../immich/local.nix;
  };
}
