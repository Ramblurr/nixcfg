{ inputs, pkgs }:
let
  cfg =
    (inputs.self.lib.nixcfg.mkGuest "immich-home" {
      extraModules = [ { modules.microvm-guest.host = "dewey"; } ];
    }).config;
in
assert !cfg.services.nginx.enable;
assert !(builtins.hasAttr "nginx" cfg.systemd.services);
assert !cfg.security.acme.acceptTerms;
assert cfg.security.acme.defaults.email == null;
assert cfg.security.acme.defaults.reloadServices == [ ];
assert cfg.security.acme.defaults.server == "https://acme-v02.api.letsencrypt.org/directory";
assert builtins.all builtins.hasContext (
  builtins.filter (e: builtins.match "(nixpkgs|nixos)=.*" e != null) cfg.nix.nixPath
);
pkgs.runCommand "common-server-test" { } ''
  touch "$out"
''
