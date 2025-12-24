{
  inputs,
  lib,
  ast-grep,
  nushellPlugins,
  nix-prefetch-github,
}:
let
  inherit (inputs.self.nixosConfigurations.quine.config.boot) kernelPackages;
in
(kernelPackages.nvidiaPackages.mkDriver {
  version = "580.119.02";
  sha256_64bit = "sha256-gCD139PuiK7no4mQ0MPSr+VHUemhcLqerdfqZwE47Nc=";
  openSha256 = "sha256-l3IQDoopOt0n0+Ig+Ee3AOcFCGJXhbH1Q1nh1TEAHTE=";

  useSettings = false;
  usePersistenced = false;
}).overrideAttrs
  (pkg: {
    passthru = pkg.passthru // {
      updateScript = lib.writeUpdateScript {
        packageToUpdate = "nvidia";

        utils = [
          ast-grep
          nix-prefetch-github
        ];
        nushellPlugins = [ nushellPlugins.query ];

        script = ./update.nu;
      };
    };
  })
