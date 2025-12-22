{
  config,
  ...
}:
let
  inherit (config.repo.secrets.global) domain;
in
{
  system.stateVersion = "24.11";
  microvm = {
    mem = 4096;
  };
  modules.microvm-guest = {
    host = "quine";
    hostFQDN = "quine.prim.${domain.home}";
    devSandbox = {
      enable = true;
      sharedDirs = [
        ".config/claude"
        "nixcfg"
        "src/nixpkgs"
      ];
    };
  };
}
