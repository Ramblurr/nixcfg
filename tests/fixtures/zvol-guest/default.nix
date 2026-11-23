{ lib, pkgs, ... }:
let
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
in
{
  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "dewey";
    hostFQDN = "dewey.example.test";
    autoNetSetup.enable = false;
    nixStoreBackend = "blk";
    writableStoreOverlay.zvolSize = 2048;
  };
  microvm.vsock = {
    cid = 4300;
    ssh.enable = true;
  };
  # Plain openssh from microvm.vsock.ssh, without the sops-managed host key.
  modules.services.sshd.enable = lib.mkForce false;
  services.resolved.settings.Resolve.FallbackDNS = lib.mkForce [ ];
  users.users.root.openssh.authorizedKeys.keys = [ sshKeys.snakeOilEd25519PublicKey ];
  environment.etc."moot-release".text = lib.mkDefault "v1";
  repo.secretFiles = {
    global = lib.mkForce ../immich/global.nix;
    site = lib.mkForce ../immich/site.nix;
    local = lib.mkForce ../immich/local.nix;
  };
}
