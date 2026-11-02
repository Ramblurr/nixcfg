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
  rev = "v2.5.0";
  hash = "sha256-8AoWrat7V67tQciDPGw20l0zYQJH1NVp3JwWbs6JYEE=";
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
