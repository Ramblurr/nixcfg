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
kernelPackages.nvidiaPackages.mkDriver {
  version = "580.82.09";
  sha256_64bit = "sha256-Puz4MtouFeDgmsNMKdLHoDgDGC+QRXh6NVysvltWlbc=";
  openSha256 = "sha256-YB+mQD+oEDIIDa+e8KX1/qOlQvZMNKFrI5z3CoVKUjs=";
  useSettings = false;
  usePersistenced = false;
}
