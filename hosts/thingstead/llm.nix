{ config, ... }:
let
  inherit (config.modules.users.primaryUser) username;
in
{
  modules.dev.llms = {
    # hirde's Codex reads the same /etc/codex/config.toml.
    profile = {
      enable = true;
      codexSessionHookUsers = [ username ];
    };
    onepasswordConnect.enable = true;
    proxy.enable = true;
    t3 = {
      port = 3774;
      tailscaleServePort = 8444;
      startAtBoot = true;
    };
  };
}
