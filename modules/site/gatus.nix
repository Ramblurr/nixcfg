{ lib, ... }:
{
  options.site.gatus = {
    endpoints = lib.mkOption {
      description = "Gatus endpoints contributed by services on this host";
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      default = [ ];
    };
    externalEndpoints = lib.mkOption {
      description = "Gatus external endpoints contributed by services on this host";
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      default = [ ];
    };
  };
}
