{ config }:
let
  inherit (config.repo.secrets.global.domain)
    be
    caseylink
    et
    family
    home
    moot
    ov
    partner
    work
    work2
    ;
  caseyLink = config.repo.secrets.global.domain."casey.link";
  localBaseDomains = [
    work
    work2
    caseylink
    caseyLink
    partner
    ov
    be
    et
    family
  ];
in
{
  # James HAProxy selects these public hostnames for TLS passthrough to Dewey.
  # Dewey Caddy terminates TLS and proxies each route to its registered upstream.
  deweyServices = [
    "ci.${work}"
    "dav.${home}"
    "home.${home}"
    "books.${home}"
    "files.${home}"
    "clients.${work}"
    "matrix.${work}"
    "data.${work}"
  ];

  thingsteadServices = [
    moot
    ".${moot}"
  ];

  localServices = builtins.concatMap (domain: [
    domain
    ".${domain}"
  ]) localBaseDomains;

  haproxyBackends = {
    dewey = {
      host = "dewey.prim.${home}";
      port = 443;
    };
    thingstead = {
      host = "thingstead.moot.${home}";
      port = 443;
    };
    local = {
      host = "127.0.0.1";
      port = 8443;
    };
  };
}
