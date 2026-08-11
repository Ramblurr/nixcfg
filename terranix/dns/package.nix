{
  pkgs,
  zones,
  records ? [ ],
}:
let
  terranix = import "${pkgs.terranix}/core/default.nix" {
    inherit pkgs;
    modules = [ (import ./default.nix { inherit records zones; }) ];
  };
in
{
  config = (pkgs.formats.json { }).generate "config.tf.json" terranix.config;
  runtime = pkgs.opentofu-dns;
}
