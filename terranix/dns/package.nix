{
  pkgs,
  zones,
  zoneSurfaces,
  records ? [ ],
}:
let
  json = pkgs.formats.json { };
  zoneConfig = json.generate "main.tf.json" (import ./zone-module.nix);
  zoneModule = pkgs.runCommand "dns-zone-module" { } ''
    mkdir -p "$out"
    ln -s ${zoneConfig} "$out/main.tf.json"
  '';
  terranix = import "${pkgs.terranix}/core/default.nix" {
    inherit pkgs;
    modules = [
      (import ./default.nix {
        moduleSource = "${zoneModule}";
        inherit records zones zoneSurfaces;
      })
    ];
  };
in
{
  config = json.generate "config.tf.json" terranix.config;
  runtime = pkgs.opentofu-dns;
}
