{
  config,
  pkgs,
  ...
}:

let
  reportDir = "/var/lib/goaccess";
  dbDir = "${reportDir}/db";
  logFile = "/var/log/caddy/access.log";
  caddyUser = config.services.caddy.user;
  caddyGroup = config.services.caddy.group;
in
{
  environment.persistence."/persist".directories = [
    {
      directory = reportDir;
      user = caddyUser;
      group = caddyGroup;
      mode = "0750";
    }
  ];

  systemd.tmpfiles.rules = [
    "Z '${reportDir}' - ${caddyUser} ${caddyGroup} - -"
    "d '${dbDir}' 0750 ${caddyUser} ${caddyGroup} - -"
  ];

  systemd.services.goaccess-report = {
    description = "Generate GoAccess Caddy access log report";
    after = [ "caddy.service" ];
    unitConfig.ConditionPathExists = logFile;
    serviceConfig = {
      Type = "oneshot";
      User = caddyUser;
      Group = caddyGroup;
      ExecStart = "${pkgs.goaccess}/bin/goaccess --no-global-config --log-format=CADDY --restore --persist --db-path=${dbDir} --agent-list --output=${reportDir}/index.html --no-progress ${logFile}";
    };
  };

  systemd.timers.goaccess-report = {
    description = "Periodic GoAccess Caddy log report generation";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2min";
      OnUnitActiveSec = "5min";
    };
  };
}
