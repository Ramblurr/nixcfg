{ config, lib, ... }:
let
  cfg = config.modules.router;
in
{
  options = {
    modules.router = {
      enable = lib.mkEnableOption "";
      nat.forwardPorts = lib.mkOption {
        type =
          with lib.types;
          listOf (submodule {
            options = {
              sourcePort = lib.mkOption {
                type = lib.types.either lib.types.int (lib.types.strMatching "[[:digit:]]+:[[:digit:]]+");
                example = 8080;
                description = lib.mdDoc ''Source port of the external interface; to specify a port range, use a string with a colon (e.g. "60000:61000")'';
              };

              destination = lib.mkOption {
                type = lib.types.str;
                example = "10.0.0.1:80";
                description = lib.mdDoc "Forward connection to destination ip:port (or [ipv6]:port); to specify a port range, use ip:start-end";
              };

              proto = lib.mkOption {
                type = lib.types.str;
                default = "tcp";
                example = "udp";
                description = lib.mdDoc "Protocol of forwarded connection";
              };

              loopbackIPs = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                example = literalExpression ''[ "55.1.2.3" ]'';
                description = lib.mdDoc "Public IPs for NAT reflection; for connections to `loopbackip:sourcePort' from the host itself and from other hosts behind NAT";
              };
            };
          });
        default = [ ];
        example = [
          {
            sourcePort = 8080;
            destination = "10.0.0.1:80";
            proto = "tcp";
          }
          {
            sourcePort = 8080;
            destination = "[fc00::2]:80";
            proto = "tcp";
          }
        ];
        description = lib.mdDoc ''
          List of forwarded ports from the external interface to
          internal destinations by using DNAT. Destination can be
          IPv6 if IPv6 NAT is enabled.
        '';
      };
    };
  };
}
