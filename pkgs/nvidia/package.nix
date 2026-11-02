{
  ast-grep,
  nix-prefetch-github,
  pkgs-lib,
  kernelPackages,
}:
(kernelPackages.nvidiaPackages.mkDriver {
  version = "595.112.07";
  sha256_64bit = "sha256-Gjjs1Tii2mIypzMBhAY69Z6fyUbJCMylVb0HkJhIufA=";
  openSha256 = "sha256-nQnXGCcvo4i7eGaVft3Xc9dRUPPH0v9Zi87cfI4i/Ys=";

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
