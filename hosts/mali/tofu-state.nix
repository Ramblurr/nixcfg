{ pkgs, ... }:
let
  stateDir = "/var/lib/tofu-state";
  repository = "${stateDir}/state.git";
in
{
  modules.zfs.datasets = {
    enable = true;
    properties."rpool2/encrypted/safe/svc/tofu-state" = {
      atime = "off";
      compression = "zstd";
      mountpoint = stateDir;
    };
  };

  users.groups.tofu-state = { };
  users.users.tofu-state = {
    isSystemUser = true;
    group = "tofu-state";
    home = stateDir;
    createHome = false;
    shell = "${pkgs.git}/bin/git-shell";
    openssh.authorizedKeys.keys = [
      "restrict ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICuBNAV8r1pHQrII09lKM4L5nLPEXKv2R/UWuLAynkq8 tofu-state@quine"
    ];
  };

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0700 tofu-state tofu-state -"
  ];

  systemd.services.tofu-state-repository = {
    description = "Initialize the OpenTofu state repository";
    wantedBy = [ "multi-user.target" ];
    after = [
      "systemd-tmpfiles-setup.service"
      "zfs-datasets.service"
    ];
    requires = [ "zfs-datasets.service" ];
    unitConfig.AssertPathIsMountPoint = stateDir;
    path = [ pkgs.git ];
    serviceConfig = {
      Type = "oneshot";
      User = "tofu-state";
      Group = "tofu-state";
      UMask = "0077";
      RemainAfterExit = true;
    };
    script = ''
      if [[ -e ${repository} && ! -d ${repository}/objects ]]; then
        echo "tofu-state: ${repository} exists but is not a bare Git repository" >&2
        exit 1
      fi

      if [[ ! -e ${repository} ]]; then
        git init --bare --initial-branch=main ${repository}
        git --git-dir=${repository} config user.name tofu-state
        git --git-dir=${repository} config user.email tofu-state@mali
        empty_tree="$(git --git-dir=${repository} mktree </dev/null)"
        initial_commit="$(printf '%s\n' 'Initialize OpenTofu state repository' \
          | git --git-dir=${repository} commit-tree "$empty_tree")"
        git --git-dir=${repository} update-ref refs/heads/main "$initial_commit"
      fi

      chmod 0700 ${repository}
      git --git-dir=${repository} config receive.denyNonFastForwards true
    '';
  };
}
