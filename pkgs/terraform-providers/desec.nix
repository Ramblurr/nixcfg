{
  git,
  nix,
  nix-update,
  pkgs-lib,
  terraform-providers,
}:
(terraform-providers.mkProvider {
  owner = "timofurrer";
  repo = "terraform-provider-desec";
  rev = "v0.6.3";
  hash = "sha256-+3Dh3GHSgGFLw0sGlex1Y7kxMfvR3K2bxcboXjWnDOs=";
  vendorHash = "sha256-peMRKmrFKClyWHTEYJlu05ho1VjF+B7hK1Sr7r+gqDc=";
  homepage = "https://registry.terraform.io/providers/timofurrer/desec";
  provider-source-address = "registry.terraform.io/timofurrer/desec";
  spdx = "MPL-2.0";
}).overrideAttrs
  (old: {
    passthru = old.passthru // {
      updateScript = pkgs-lib.writeUpdateScript {
        packageToUpdate = "terraform-provider-desec";
        utils = [
          git
          nix
          nix-update
        ];
        script = ./update.bb;
      };
    };
  })
