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
  version = "580.105.08";
  sha256_64bit = "sha256-2cboGIZy8+t03QTPpp3VhHn6HQFiyMKMjRdiV2MpNHU=";
  openSha256 = "sha256-FGmMt3ShQrw4q6wsk8DSvm96ie5yELoDFYinSlGZcwQ=";

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
