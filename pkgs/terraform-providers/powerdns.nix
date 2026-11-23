{
  git,
  nix,
  nix-update,
  pkgs-lib,
  terraform-providers,
}:
(terraform-providers.mkProvider {
  owner = "mmianl";
  repo = "terraform-provider-powerdns";
  rev = "v2.6.0";
  hash = "sha256-GPoX4cB1tBSTGzZKZHK8GZEfCyDcAEIVeD6C2QoWj0I=";
  vendorHash = null;
  homepage = "https://registry.terraform.io/providers/mmianl/powerdns";
  provider-source-address = "registry.terraform.io/mmianl/powerdns";
  spdx = "MIT";
}).overrideAttrs
  (old: {
    passthru = old.passthru // {
      updateScript = pkgs-lib.writeUpdateScript {
        packageToUpdate = "terraform-provider-powerdns";
        utils = [
          git
          nix
          nix-update
        ];
        script = ./update.bb;
      };
    };
  })
