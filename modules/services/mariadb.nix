{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.mariadb;
  mysqlUser = config.services.mysql.user;

  serviceDeps = [
    "var-lib-mysql.mount"
    "zfs-datasets.service"
  ];
in
{
  options.modules.services.mariadb = {
    enable = lib.mkEnableOption "mariadb";
    package = lib.mkPackageOption pkgs "mariadb_110" { };
  };

  config = lib.mkIf cfg.enable {

    services.mysql = {
      enable = true;
      package = cfg.package;
      dataDir = "/var/lib/mysql/data";
      settings = {
        mysqld = {
          skip-innodb_doublewrite = true;
          innodb_flush_method = "fsync";
          innodb_doublewrite = 0;
          innodb_use_atomic_writes = 0;
          innodb_use_native_aio = 0;
          innodb_read_io_threads = 10;
          innodb_write_io_threads = 10;
          innodb_buffer_pool_size = "4G";
          innodb_flush_log_at_trx_commit = 1;
          innodb_log_file_size = "1G";
          innodb_flush_neighbors = 0;
          innodb_fast_shutdown = 2;
        };
      };
    };

    systemd.services.mysql.requires = serviceDeps;
    systemd.services.mysql.wants = serviceDeps;

    # services.mysqlBackup = {
    #   enable = true;
    #   location = "/var/backup/mysql";
    #   calendar = "11:00:00";
    #   singleTransaction = true;
    # };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/mariadb"."mountpoint" = "/var/lib/mysql";
      "rpool/encrypted/safe/svc/mariadb"."com.sun:auto-snapshot" = "false";
      "rpool/encrypted/safe/svc/mariadb"."recordsize" = "16k";
      "rpool/encrypted/safe/svc/mariadb"."primarycache" = "all";
      "rpool/encrypted/safe/svc/mariadb"."logbias" = "throughput";
    };
    systemd.tmpfiles.rules = [ "d /var/lib/mysql/data 750 ${mysqlUser} ${mysqlUser}" ];
  };
}
