{ lib, ... }:
{
  options.node = {
    name = lib.mkOption { type = lib.types.str; };
    secretsDir = lib.mkOption { type = lib.types.path; };
  };
}
