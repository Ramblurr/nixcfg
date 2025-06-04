{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

{

  disabledModules = [
    "${inputs.nixpkgs-unstable}/nixos/modules/services/misc/tabby.nix"
  ];
  imports = [
    "${inputs.nixpkgs-mine}/nixos/modules/services/misc/tabby.nix"
  ];
  config = {
    services.tabby = {
      enable = true;
      package = (
        pkgs.tabby.override {
          cudaSupport = true;
          acceleration = "cuda";
        }
      );
      model = "TabbyML/DeepseekCoder-6.7B";
    };
  };
}
