{
  pkgs,
  zones,
}:
let
  module = pkgs.writeText "dns-terranix.nix" ''
    import ${./default.nix} {
      zones = ${builtins.toJSON zones};
    }
  '';
in
{
  config = pkgs.runCommand "dns-config.tf.json" { nativeBuildInputs = [ pkgs.terranix ]; } ''
    ${pkgs.terranix}/bin/terranix ${module} > "$out"
  '';
  runtime = pkgs.opentofu-powerdns;
}
