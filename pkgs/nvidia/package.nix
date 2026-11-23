{
  ast-grep,
  nix-prefetch-github,
  pkgs-lib,
  kernelPackages,
}:
(kernelPackages.nvidiaPackages.mkDriver {
  version = "620.73.04";
  sha256_64bit = "sha256-tfLi1dqENbMlOTBAFbib9+si/grsjNaQW3puip+bjxs=";
  openSha256 = "sha256-+GPU2bXC/QWZTt0eH3dTvI0bMAta1GCdU2WFI4E4LWE=";

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
