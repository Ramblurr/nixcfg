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
  rev = "v2.3.0";
  hash = "sha256-oRDBHfI2nWu29rvU+/LbI1OeIyJd4PbXJzA6w7EmXP8=";
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
