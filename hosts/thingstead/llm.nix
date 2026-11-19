{ config, ... }:
let
  inherit (config.modules.users.primaryUser) username;
in
{
  sops.secrets.onepassword-connect-llm-token = {
    owner = username;
    mode = "0400";
  };

  modules.dev.llms = {
    # hirde's Codex reads the same /etc/codex/config.toml.
    profile = {
      enable = true;
      codexSessionHookUsers = [ username ];
    };
    onepasswordConnect.tokenFile = config.sops.secrets.onepassword-connect-llm-token.path;
    proxy = {
      enable = true;
      url = "https://quine.${config.repo.secrets.global.domain.tailnet}:8317";
      credential = "op://LLM/CLIProxyAPI/credential";
    };
    t3 = {
      port = 3774;
      tailscaleServePort = 8444;
    };
  };

  # Start the headless T3 Code user service at boot, without a login.
  users.users.${username}.linger = true;
  systemd.services."user@${toString config.users.users.${username}.uid}" = rec {
    wants = [
      "tailscaled.service"
      "tailscaled-set.service"
    ];
    after = wants;
  };

  # T3 Code configures Tailscale Serve as this user.
  services.tailscale.extraSetFlags = [ "--operator=${username}" ];
}
