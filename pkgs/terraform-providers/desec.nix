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
  rev = "v0.6.4";
  hash = "sha256-ko04UMXGzholQhTQ4y/kKp0m+Y94TKPRxDSCZUcmllY=";
  vendorHash = "sha256-Qgm97rmsDVz7nxHSoJtUeUkKLBsuJtO8Bz1eNfDgpPw=";
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
