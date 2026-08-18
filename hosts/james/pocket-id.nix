{
  config,
  lib,
  ...
}:
let
  homeDomain = config.repo.secrets.global.domain.home;
  workDomain = config.repo.secrets.global.domain.work;
  instances = {
    "id.${homeDomain}" = 1411;
    "id.${workDomain}" = 1412;
  };
in
{
  networking.hosts."127.0.0.1" = lib.attrNames instances;

  modules.services.pocket-id.instances = {
    home = {
      containerName = "pocket-id-home";
      publicDomain = "id.${homeDomain}";
      port = 1411;
      actorPort = 1414;
      dataset = "rpool/encrypted/safe/svc/id.${homeDomain}";
      hostPath = "/var/lib/pocket-id-containers/id.${homeDomain}";
      sopsKey = "home-pocket-id-encryption-key";
    };
    work = {
      containerName = "pocket-id-work";
      publicDomain = "id.${workDomain}";
      port = 1412;
      actorPort = 1415;
      dataset = "rpool/encrypted/safe/svc/id.${workDomain}";
      hostPath = "/var/lib/pocket-id-containers/id.${workDomain}";
      sopsKey = "work-pocket-id-encryption-key";
    };
  };

  site.gatus.endpoints = [
    {
      name = "So" + "Cozy ID";
      group = "Infrastructure & Operations";
      url = "https://id.${homeDomain}/healthz";
      conditions = [ "[STATUS] == 204" ];
    }
    {
      name = "Outskirts Labs ID";
      group = "Infrastructure & Operations";
      url = "https://id.${workDomain}/healthz";
      conditions = [ "[STATUS] == 204" ];
    }
  ];
}
