{ config, lib, pkgs, ... }:
let
  backupUser = "borg";
  backupDir = "/mnt/tank2/backups/borg_repos";
in {
  users.users."${backupUser}" = { };
  systemd.tmpfiles.rules = [ "d ${backupDir} 0755 root root - -" ];
  services.borgbackup.repos = {
    aquinas = {
      path = "/mnt/tank2/backups/borg_repos/aquinas";
      authorizedKeys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUmi4lHn11g1eo0KFax0R2gQEKmn3J+quvM5Nx0UipJ"
        "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBM9LkzTZXu/qsSuj+rdy24BBySWOLSmjcfA142AZcXJ5bDIrjWPHBg5pm0iroaRqC5eArWCD6VFA4e8cQzTzJE4="
      ];
    };
    proxmox = {
      path = "/mnt/tank2/backups/borg_repos/proxmox";
      authorizedKeys =
        [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDVhO6fCS/WBKebnGaNLxUDg5jWyMTv7nXvirPONXY3a" ];
    };
  };
  systemd.services = lib.mapAttrs' (repo: repoCfg: {
    name = "borgbackup-compact-${repo}";
    value = {
      path = with pkgs; [ borgbackup ];
      script = "borg compact --verbose ${repoCfg.path}";
      serviceConfig = {
        CPUSchedulingPolicy = "idle";
        IOSchedulingClass = "idle";
        PrivateTmp = true;
        ProtectSystem = "strict";
        ReadWritePaths = repoCfg.path;
        User = backupUser;
      };
      startAt = "Mon 4:55";
    };
  }) config.services.borgbackup.repos;
}
