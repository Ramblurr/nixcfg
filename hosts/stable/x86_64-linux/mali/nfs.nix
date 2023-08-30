{
  config,
  lib,
  pkgs,
  ...
}: let
  sopsFile = builtins.getEnv "SOPS_SECRETS_FILE";
  nfsExports = (lib.importJSON sopsFile).nfs_exports;
in {
  services.nfs.server = {
    enable = true;
    # showmount -e localhost
    exports = nfsExports;
  };
  networking.firewall.allowedTCPPorts = [111 2049 4000 4001 4002 20048];
  networking.firewall.allowedUDPPorts = [111 2049 4000 4001 4002 20048];
}
