{
  pkgs,
  zones,
}:
let
  terranix = import "${pkgs.terranix}/core/default.nix" {
    inherit pkgs;
    modules = [ (import ./default.nix { inherit zones; }) ];
  };
in
{
  config = (pkgs.formats.json { }).generate "config.tf.json" terranix.config;
  runtime = pkgs.opentofu-powerdns;
}
