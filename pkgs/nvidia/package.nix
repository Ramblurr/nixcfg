{
  inputs,
  ast-grep,
  nix-prefetch-github,
  pkgs-lib,
}:
let
  inherit (inputs.self.nixosConfigurations.quine.config.boot) kernelPackages;
in
(kernelPackages.nvidiaPackages.mkDriver {
  version = "580.126.18";
  sha256_64bit = "sha256-p3gbLhwtZcZYCRTHbnntRU0ClF34RxHAMwcKCSqatJ0=";
  openSha256 = "sha256-1Q2wuDdZ6KiA/2L3IDN4WXF8t63V/4+JfrFeADI1Cjg=";

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
