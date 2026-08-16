{
  pkgs,
  buckets,
  clientKeys,
  grants ? [ ],
}:
let
  json = pkgs.formats.json { };
  terranix = import "${pkgs.terranix}/core/default.nix" {
    inherit pkgs;
    modules = [
      (import ./default.nix {
        inherit buckets clientKeys grants;
      })
    ];
  };
in
{
  config = json.generate "garage.tf.json" terranix.config;
  runtime = pkgs.opentofu-garage;
}
