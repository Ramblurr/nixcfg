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
      "databasus.mgmt.${homeDomain}"
      "nix-cache.int.${homeDomain}"
      "garage.data.${homeDomain}"
      "garage.mgmt.${homeDomain}"
    ];
    protocols = [
      "h1"
      "h2"
      "h3"
    ];
    redirectPort = 80;
    redirectStatus = 301;
    acmeEmail = email.acme;
  };
}
