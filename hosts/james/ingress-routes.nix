{ config }:
let
  inherit (config.repo.secrets.global.domain)
    family
    home
    personal1
    personal2
    personal3
    personal4
    personal5
    personal6
    work
    work2
    ;
  localBaseDomains = [
    work
    work2
    personal1
    personal2
    personal3
    personal4
    personal5
    personal6
    family
  ];
in
{
  deweyServices = [
    "y2pod.${home}"
    "dav.${home}"
    "home.${home}"
    "books.${home}"
    "auth.${home}"
    "clients.${work}"
    "auth.${work}"
    "matrix.${work}"
    "data.${work}"
  ];

  thingsteadServices = [
    "moot.land"
    ".moot.land"
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
