{
  config,
  ...
}:
let
  inherit (config.repo.secrets.global) domain email;
  homeDomain = domain.home;
in
{
  modules.services.caddy.edge = {
    certificateHosts = [
      "attic.mgmt.${homeDomain}"
      "attic.int.${homeDomain}"
      "nix-cache.int.${homeDomain}"
      "s3.data.${homeDomain}"
      "*.s3.data.${homeDomain}"
      "minio.data.${homeDomain}"
      "*.s3.mgmt.${homeDomain}"
      "minio.mgmt.${homeDomain}"
      "s3.mgmt.${homeDomain}"
    ];
    protocols = [
      "h1"
      "h2"
      "h3"
    ];
    redirectPort = 80;
    redirectStatus = 301;
    acmeEmail = email.acme;
    sopsFile = ../../configs/home-ops/shared.sops.yml;
  };
}
