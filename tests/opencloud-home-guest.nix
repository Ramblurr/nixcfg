{
  pkgs,
  guest,
  expectedSenderName ? null,
  branding ? null,
}:
let
  c = guest.config;
  volumes = builtins.filter (volume: volume.mountPoint == "/var") c.microvm.volumes;
  secretShares = builtins.filter (share: share.tag == "host-secrets") c.microvm.shares;
  mountedSource =
    target:
    let
      suffix = ":${target}:ro";
      matches = builtins.filter (pkgs.lib.hasSuffix suffix) c.virtualisation.quadlet.containers.opencloud-home.containerConfig.Volume;
    in
    assert builtins.length matches == 1;
    pkgs.lib.removeSuffix suffix (builtins.head matches);
  theme = if branding == null then null else mountedSource "/etc/opencloud/themes";
  email = if branding == null then null else mountedSource "/etc/opencloud/email";
  expectedEnvironment = [
    "OC_ADD_RUN_SERVICES=collaboration,notifications"
    "NOTIFICATIONS_SMTP_AUTHENTICATION=none"
    "NOTIFICATIONS_SMTP_ENCRYPTION=none"
  ]
  ++ pkgs.lib.optionals (branding != null) [
    "WEB_ASSET_THEMES_PATH=/etc/opencloud/themes"
    "NOTIFICATIONS_EMAIL_TEMPLATE_PATH=/etc/opencloud/email"
  ];
  brandingChecks =
    if branding == null then
      ""
    else
      ''
        for name in ${pkgs.lib.concatMapStringsSep " " pkgs.lib.escapeShellArg branding.themeFiles}; do
          test -s "${theme}/opencloud/assets/$name"
          test ! -L "${theme}/opencloud/assets/$name"
        done
        ${pkgs.jq}/bin/jq -e --arg expected ${pkgs.lib.escapeShellArg branding.name} '.common.name == $expected and (has("clients") | not)' ${theme}/_branding/theme.json
        test -s ${pkgs.lib.escapeShellArg "${email}/templates/html/img/${branding.emailImage}"}
        grep -F ${pkgs.lib.escapeShellArg "cid:${branding.emailImage}"} ${email}/templates/html/email.html.tmpl
        grep -F 'https://data.${c.repo.secrets.home-ops.homeDomain}' ${email}/templates/text/email.text.tmpl
        ! grep -F '@opencloudUrl@' ${email}/templates/{html/email.html.tmpl,text/email.text.tmpl}
      '';
in
assert pkgs.lib.all (assertion: assertion.assertion) c.assertions;
assert c.networking.hostName == "opencloud-home";
assert c.modules.microvm-guest.host == "dewey";
assert c.microvm.hypervisor == "qemu";
assert builtins.isString c.microvm.declaredRunner.drvPath;
assert builtins.length volumes == 1;
assert (builtins.head volumes).fsType == "ext4";
assert (builtins.head volumes).image == "/var/lib/opencloud-home-vm/var.img";
assert c.fileSystems."/var".neededForBoot;
assert c.users.users.root.hashedPassword == "!";
assert !c.services.openssh.settings.PasswordAuthentication;
assert !c.services.openssh.settings.PermitEmptyPasswords;
assert !c.services.openssh.openFirewall;
assert c.networking.firewall.enable;
assert builtins.all (port: !(builtins.elem port c.networking.firewall.allowedTCPPorts)) [
  22
  9200
  9201
];
assert c.networking.firewall.allowedTCPPortRanges == [ ];
assert pkgs.lib.hasInfix "ip saddr 172.20.20.3 tcp dport { 22, 9200, 9201 } accept"
  c.networking.firewall.extraInputRules;
assert c.modules.services.opencloud.instances.home.listenAddress == "172.20.20.24";
assert c.microvm.vsock.ssh.enable;
assert c.modules.microvm-guest.hostSecrets.enable;
assert builtins.length secretShares == 1;
assert (builtins.head secretShares).source == "/run/microvms/secrets/opencloud-home";
assert (builtins.head secretShares).mountPoint == "/run/host-secrets";
assert (builtins.head secretShares).proto == "virtiofs";
assert (builtins.head secretShares).readOnly;
assert c.microvm.credentialFiles == { };
assert
  c.systemd.services.opencloud-home-credentials.serviceConfig.LoadCredential == [
    "IDM_ADMIN_PASSWORD:/run/host-secrets/IDM_ADMIN_PASSWORD"
    "JWT_SECRET:/run/host-secrets/JWT_SECRET"
  ];
assert
  c.systemd.services.opencloud-home-credentials.unitConfig.RequiresMountsFor
  == [ "/run/host-secrets" ];
assert c.microvm.vsock.cid == 4244;
assert c.modules.services.opencloud.instances.home.uid == 3100;
assert c.modules.services.opencloud.instances.home.dataMount == "/mnt/opencloud";
assert c.fileSystems."/mnt/opencloud".fsType == "nfs";
assert builtins.all (option: builtins.elem option c.fileSystems."/mnt/opencloud".options) [
  "vers=4.2"
  "noac"
  "hard"
];
assert builtins.all (
  value:
  builtins.elem value c.virtualisation.quadlet.containers.opencloud-home.containerConfig.Environment
) expectedEnvironment;
assert
  expectedSenderName == null
  || builtins.elem (builtins.toJSON "NOTIFICATIONS_SMTP_SENDER=${expectedSenderName} <${c.repo.secrets.home-ops.mail.notificationsFromAddress}>") c.virtualisation.quadlet.containers.opencloud-home.containerConfig.Environment;
assert
  c.modules.services.opencloud.instances.home.environment.PROXY_ROLE_ASSIGNMENT_DRIVER == "oidc";
assert
  c.modules.services.opencloud.instances.home.environment.PROXY_ROLE_ASSIGNMENT_OIDC_CLAIM
  == "opencloud_home_roles";
# The guest cannot reach Dewey's prim address selected by split DNS.
assert builtins.all (host: builtins.elem host (c.networking.hosts."172.20.20.3" or [ ])) [
  c.modules.services.opencloud.instances.home.domain
  c.modules.services.opencloud.instances.home.office.domain
];
assert builtins.all
  (
    name:
    builtins.all
      (
        host:
        builtins.elem "${host}:172.20.20.3" (
          c.virtualisation.quadlet.containers.${name}.containerConfig.AddHost or [ ]
        )
      )
      [
        c.modules.services.opencloud.instances.home.domain
        c.modules.services.opencloud.instances.home.office.domain
      ]
  )
  [
    "opencloud-home"
    "opencloud-home-office"
  ];
pkgs.runCommand "opencloud-home-guest" { } ''
  ${brandingChecks}
  touch $out
''
