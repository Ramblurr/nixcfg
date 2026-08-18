{
  git,
  nix,
  nix-update,
  pkgs-lib,
  terraform-providers,
}:
(terraform-providers.mkProvider {
  owner = "jkossis";
  repo = "terraform-provider-garage";
  rev = "v1.0.5";
  hash = "sha256-j9rf8ynqPl0JTv/K5ouVBitH41lIRnDiMvulx7OarZE=";
  vendorHash = "sha256-r6WqUIatjVjvC98PlbVPm3w1/XZPhdYriG4KsoqrkgY=";
  homepage = "https://registry.terraform.io/providers/jkossis/garage";
  provider-source-address = "registry.terraform.io/jkossis/garage";
  spdx = "MPL-2.0";
}).overrideAttrs
  (old: {
    passthru = old.passthru // {
      updateScript = pkgs-lib.writeUpdateScript {
        packageToUpdate = "terraform-provider-garage";
        utils = [
          git
          nix
          nix-update
        ];
        script = ./update.bb;
      };
    };
  })
