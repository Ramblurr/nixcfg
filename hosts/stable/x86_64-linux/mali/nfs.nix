{
  config,
  lib,
  pkgs,
  ...
}: {
  services.nfs.server = {
    enable = true;
    # showmount -e localhost
    exports = builtins.readFile ../../../../secrets/mali-nfs-exports.secrets;
  };
  networking.firewall.allowedTCPPorts = [111 2049 4000 4001 4002 20048];
  networking.firewall.allowedUDPPorts = [111 2049 4000 4001 4002 20048];
}
