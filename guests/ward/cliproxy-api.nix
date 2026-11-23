let
  models = import ../../modules/dev/llms/models.nix;
in
{
  modules.services.cliproxy-api.credentials = {
    "op://mootward/CLIProxyAPI/management-secret-unhashed" =
      "/run/host-secrets/cliproxy-management-secret";
    "op://mootward/CLIProxyAPI/credential" = "/run/host-secrets/cliproxy-client-key";
    "op://mootward/CLIProxyAPI hirde/credential" = "/run/host-secrets/cliproxy-hirde-key";
    "op://mootward/OpenRouter/credential" = "/run/host-secrets/cliproxy-openrouter-key";
    "op://mootward/Subq/credential" = "/run/host-secrets/cliproxy-subq-key";
    "op://mootward/Mistral/credential" = "/run/host-secrets/cliproxy-mistral-key";
  };
  modules.services.cliproxy-api.settings = {
    server.trusted-proxies = [
      "127.0.0.1"
      "::1"
    ];
    management = {
      allow-remote = true;
      secret-key = "op://mootward/CLIProxyAPI/management-secret-unhashed";
    };
    access.api-keys = [
      "op://mootward/CLIProxyAPI/credential"
      "op://mootward/CLIProxyAPI hirde/credential"
    ];
    api-keys.openai-compatibility = [
      {
        name = "openrouter";
        base-url = "https://openrouter.ai/api/v1";
        keys = [ { api-key = "op://mootward/OpenRouter/credential"; } ];
        models = map (name: { inherit name; }) models.openrouter;
      }
      {
        name = "subq";
        base-url = "https://api.subq.ai/v1";
        keys = [ { api-key = "op://mootward/Subq/credential"; } ];
        models = [ { name = models.subq.savant.id; } ];
      }
      {
        name = "mistral";
        base-url = "https://api.mistral.ai/v1";
        keys = [ { api-key = "op://mootward/Mistral/credential"; } ];
        models = [ { name = models.mistral.large4.id; } ];
      }
    ];
    routing = {
      strategy = "soonest-reset";
      session-affinity = true;
      session-affinity-ttl = "1h";
      session-affinity-rebind-on = "quota";
      retry = {
        request-retry = 3;
        max-retry-interval = 30;
      };
      cooldown.transient-error-cooldown-seconds = 15;
    };
  };
}
