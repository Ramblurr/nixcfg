{
  pkgs,
  dewey,
  mali,
  jamesIngressRoutes,
  dataDataset,
  dataDir,
}:
let
  d = dewey.config;
  m = mali.config;
  guest = d.microvm.vms.opencloud-home.evaluatedConfig.config;
  secretService = d.systemd.services."microvm-secrets-opencloud-home";
  stateDataset = "rpool/encrypted/safe/vms/opencloud-home";
  hasSelection =
    selector: jobs: builtins.any (job: (job.filesystems or { }).${selector} or false) jobs;
in
assert pkgs.lib.all (assertion: assertion.assertion) d.assertions;
assert pkgs.lib.all (assertion: assertion.assertion) m.assertions;
assert d.microvm.vms.opencloud-home.evaluatedConfig.config.networking.hostName == "opencloud-home";
assert d.modules.zfs.datasets.properties.${stateDataset}.mountpoint == "/var/lib/opencloud-home-vm";
assert builtins.elem "/var/lib/opencloud-home-vm"
  d.systemd.services."microvm@opencloud-home".unitConfig.AssertPathIsMountPoint;
assert hasSelection "rpool/encrypted/safe/vms<" d.services.zrepl.settings.jobs;
assert hasSelection "tank2<" (
  builtins.filter (job: job.name == "mali_snap") m.services.zrepl.settings.jobs
);
assert hasSelection "tank2<" (
  builtins.filter (job: job.name == "mali_rsyncnet") m.services.zrepl.settings.jobs
);
assert hasSelection "tank2/replication/dewey/rpool/encrypted/safe/vms<" (
  builtins.filter (job: job.name == "mali_rsyncnet") m.services.zrepl.settings.jobs
);
assert m.modules.zfs.datasets.properties.${dataDataset}.xattr == "sa";
assert m.users.users.opencloud-home.uid == 3100;
assert pkgs.lib.hasInfix "${dataDir} 172.20.20.24(rw,sync,root_squash,no_subtree_check)"
  m.services.nfs.server.exports;
assert d.modules.services.caddy.routes.opencloud-home.upstream == "http://172.20.20.24:9200";
assert d.modules.services.caddy.routes.opencloud-home-office.upstream == "http://172.20.20.24:9201";
assert
  d.modules.services.caddy.routes.opencloud-home-office.publicHost
  == "euro-office.${d.repo.secrets.home-ops.homeDomain}";
assert builtins.all
  (
    path:
    let
      response = d.modules.services.caddy.routes.opencloud-home-office.staticResponses.${path} or { };
    in
    (response.status or 0) == 404 && (response.body or null) == ""
  )
  [
    "/"
    "/welcome"
    "/welcome/*"
  ];
assert
  !(d.modules.services.caddy.routes.opencloud-home-office.staticResponses ? "/hosting/discovery");
assert
  d.microvm.vms.opencloud-home.evaluatedConfig.config.modules.services.opencloud.instances.home.office.domain
  == d.modules.services.caddy.routes.opencloud-home-office.publicHost;
assert d.modules.services.caddy.routes.opencloud-home.requestBodyMaxSize == null;
assert !d.modules.services.caddy.routes.opencloud-home.directWan;
assert !d.modules.services.caddy.routes.opencloud-home-office.directWan;
assert builtins.elem d.modules.services.caddy.routes.opencloud-home.publicHost
  jamesIngressRoutes.deweyServices;
assert builtins.elem d.modules.services.caddy.routes.opencloud-home-office.publicHost
  jamesIngressRoutes.deweyServices;
assert
  d.modules.services.onepassword-systemd-credentials.microvmSecrets.opencloud-home == {
    IDM_ADMIN_PASSWORD = "op://home-ops-prod/opencloud-home/admin-password";
    JWT_SECRET = "op://home-ops-prod/opencloud-home/office-jwt-secret";
  };
assert !(d.modules.services.onepassword-systemd-credentials.consumers ? "opencloud-home-env-setup");
assert builtins.elem "microvm-secrets-opencloud-home.service"
  d.systemd.services."microvm@opencloud-home".requires;
assert builtins.elem "microvm-secrets-opencloud-home.service"
  d.systemd.services."microvm-virtiofsd@opencloud-home".requires;
assert builtins.elem "microvm@opencloud-home.service" secretService.partOf;
assert secretService.serviceConfig.User == "microvm";
assert secretService.serviceConfig.Group == "kvm";
assert secretService.serviceConfig.UMask == "0077";
assert
  secretService.serviceConfig.LoadCredential == [
    "IDM_ADMIN_PASSWORD:${d.modules.services.onepassword-systemd-credentials.socketPath}"
    "JWT_SECRET:${d.modules.services.onepassword-systemd-credentials.socketPath}"
  ];
assert guest.microvm.credentialFiles == { };
assert guest.modules.microvm-guest.hostSecrets.enable;
assert builtins.elem "opencloud-home-credentials" guest.modules.microvm-guest.hostSecrets.services;
assert secretService.script != "";
pkgs.runCommand "opencloud-home-hosts" { } ''
  export CREDENTIALS_DIRECTORY="$TMPDIR/credentials"
  mkdir -m 0700 "$CREDENTIALS_DIRECTORY"
  printf '%064d' 1 > "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD"
  printf '%064d' 2 > "$CREDENTIALS_DIRECTORY/JWT_SECRET"
  renderer=${
    pkgs.writeShellScript "opencloud-secret-renderer" (
      pkgs.lib.replaceStrings [ "/run/microvms/secrets/opencloud-home" ] [ "$TMPDIR/source" ]
        secretService.script
    )
  }
  "$renderer"
  test "$(cat "$TMPDIR/source/IDM_ADMIN_PASSWORD")" = "$(printf '%064d' 1)"
  test "$(cat "$TMPDIR/source/JWT_SECRET")" = "$(printf '%064d' 2)"
  test "$(stat -c %a "$TMPDIR/source/IDM_ADMIN_PASSWORD")" = 400
  test "$(stat -c %a "$TMPDIR/source/JWT_SECRET")" = 400
  test ! -e "$TMPDIR/source/IDM_ADMIN_PASSWORD.new"
  test ! -e "$TMPDIR/source/JWT_SECRET.new"
  printf '%064d' 3 > "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD"
  "$renderer"
  test "$(cat "$TMPDIR/source/IDM_ADMIN_PASSWORD")" = "$(printf '%064d' 3)"
  rm "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD"
  if "$renderer"; then
    echo 'Credential materialization must fail when a credential is missing' >&2
    exit 1
      fi
      test ! -e "$TMPDIR/source/IDM_ADMIN_PASSWORD"
      test ! -e "$TMPDIR/source/JWT_SECRET"
      touch $out
''
