{ lib, ... }:
let
  endpointType = lib.types.submodule {
    freeformType = lib.types.attrsOf lib.types.anything;
    options = {
      interval = lib.mkOption {
        type = lib.types.nonEmptyStr;
        default = "5m";
        description = "Interval between Gatus checks";
      };
      conditions = lib.mkOption {
        type = lib.types.listOf lib.types.nonEmptyStr;
        default = [ "[STATUS] == 200" ];
        description = "Conditions that determine whether the Gatus check succeeds";
      };
      alerts = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
        default = [ { type = "pushover"; } ];
        description = "Alerts sent when the Gatus check fails";
      };
    };
  };
in
{
  options.site.gatus = {
    endpoints = lib.mkOption {
      description = "Gatus endpoints contributed by services on this host";
      type = lib.types.listOf endpointType;
      default = [ ];
    };
    externalEndpoints = lib.mkOption {
      description = "Gatus external endpoints contributed by services on this host";
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      default = [ ];
    };
  };
}
