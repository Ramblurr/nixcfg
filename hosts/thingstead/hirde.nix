{
  config,
  ...
}:

let
  serviceName = "hirde";
  user = serviceName;
  group = serviceName;
  workspaceDir = "/var/lib/wrights/wright-000";
  sopsAgeDir = "${workspaceDir}/.config/sops/age";
  sopsAgeKeyFile = "${sopsAgeDir}/keys.txt";
in
{
  sops.secrets.hirde-age-secret-key = {
    owner = user;
    group = group;
    mode = "0600";
    path = sopsAgeKeyFile;
  };
  users.manageLingering = true;
  users.groups.${group}.gid = 1001;
  users.users.${user} = {
    isNormalUser = true;
    uid = 1001;
    group = group;
    extraGroups = [ "wheel" ];
    home = workspaceDir;
    createHome = true;
    linger = true;
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/wrights 0755 root root -"
    "d ${workspaceDir} 0750 ${user} ${group} -"
    "d ${sopsAgeDir} 0700 ${user} ${group} -"
  ];

  systemd.services.home-manager-hirde = {
    unitConfig.ConditionPathExists = sopsAgeKeyFile;
    unitConfig.RequiresMountsFor = [ workspaceDir ];
  };
  home-manager.users.hirde = import ./home-hirde.nix;
}
