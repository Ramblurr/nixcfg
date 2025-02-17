{ inputs, ... }:
{
  imports = [
    inputs.microvm.nixosModules.microvm
    inputs.impermanence.nixosModules.impermanence
    inputs.sops-nix.nixosModules.sops
    ./common-server.nix
    ./site.nix
    ./secrets.nix
    ../modules/services/sshd.nix
    ../modules/microvm-guest
    ../modules/meta.nix
    ../modules/secrets.nix
    ../modules/sops.nix
    ../modules/impermanence/default.nix
  ];
}
