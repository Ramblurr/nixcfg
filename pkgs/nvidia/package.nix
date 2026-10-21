{
  ast-grep,
  nix-prefetch-github,
  pkgs-lib,
  kernelPackages,
}:
(kernelPackages.nvidiaPackages.mkDriver {
  version = "595.104.02";
  sha256_64bit = "sha256-5CHCAuTHn1jDx/MWG75xRU67PYiTb4ggWg4yfNBMWco=";
  openSha256 = "sha256-FWk5ra2yjz8VAxAA8GXrSoeBj/XC1BKvsKsBKR09joE=";

  useSettings = false;
  usePersistenced = false;
}).overrideAttrs
  (pkg: {
    passthru = pkg.passthru // {
      updateScript = pkgs-lib.writeUpdateScript {
        packageToUpdate = "nvidia";
        utils = [
          ast-grep
          nix-prefetch-github
        ];
        script = ./update.bb;
      };
    };
  })
