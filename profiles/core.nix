{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  imports = [
    ./user-ramblurr.nix
    ../mixins/common.nix

    ../mixins/sshd.nix
    ../mixins/tailscale.nix
  ];

  config = {};
}
