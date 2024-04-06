{ config, lib, ... }:

{
  options = {
    external = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to expose the service externally";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      example = "example.com";
      description = "The ingress domain to use";
    };
  };
}
