{
  config,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
in
{
  environment.persistence."/persist" = {
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    users.${username} = {
      directories = [
        "nixcfg"
        "nixcfg-private"
        "docs"
        "downloads"
        "src"
        "sync"
        "vendor"
        "work"
        ".cache"
        ".config"
        ".local/bin"
        ".local/share"
        ".local/state"
        ".sprites"
      ];
    };
  };
}
