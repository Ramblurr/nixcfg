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
assert pkgs.lib.hasInfix
  "${dataDir} 172.20.20.24(rw,sync,root_squash,no_subtree_check)"
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
  d.modules.services.onepassword-systemd-credentials.consumers.opencloud-home-env-setup == {
    IDM_ADMIN_PASSWORD = "op://home-ops-prod/opencloud-home/admin-password";
    JWT_SECRET = "op://home-ops-prod/opencloud-home/office-jwt-secret";
  };
assert !(d.sops.secrets ? opencloud-home-admin-password);
assert !(d.sops.secrets ? opencloud-home-env);
assert builtins.elem "opencloud-home-env-setup.service"
  d.systemd.services."microvm@opencloud-home".requires;
assert builtins.elem "microvm@opencloud-home.service"
  d.systemd.services.opencloud-home-env-setup.partOf;
assert d.systemd.services.opencloud-home-env-setup.serviceConfig.User == "microvm";
assert
  d.microvm.vms.opencloud-home.evaluatedConfig.config.microvm.credentialFiles.opencloud-home-env
  == "/run/opencloud-home-env/app.env";
pkgs.runCommand "opencloud-home-hosts" { } ''
  export CREDENTIALS_DIRECTORY="$TMPDIR/credentials"
  export RUNTIME_DIRECTORY="$TMPDIR/runtime"
  mkdir -m 0700 "$CREDENTIALS_DIRECTORY" "$RUNTIME_DIRECTORY"
  printf '%064d' 1 > "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD"
  printf '%064d' 2 > "$CREDENTIALS_DIRECTORY/JWT_SECRET"
  renderer=${pkgs.writeShellScript "opencloud-env-renderer" d.systemd.services.opencloud-home-env-setup.script}
  "$renderer"
  test "$(cat "$RUNTIME_DIRECTORY/app.env")" = "IDM_ADMIN_PASSWORD=$(printf '%064d' 1)"
  test "$(cat "$RUNTIME_DIRECTORY/office.env")" = "JWT_SECRET=$(printf '%064d' 2)"
  test "$(stat -c %a "$RUNTIME_DIRECTORY/app.env")" = 400
  test "$(stat -c %a "$RUNTIME_DIRECTORY/office.env")" = 400
  printf '%064d' 3 > "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD"
  "$renderer"
  test "$(cat "$RUNTIME_DIRECTORY/app.env")" = "IDM_ADMIN_PASSWORD=$(printf '%064d' 3)"
  rm "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD"
  if "$renderer"; then
    echo 'Credential rendering must fail when a credential is missing' >&2
    exit 1
  fi
  touch $out
''
