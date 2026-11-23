let
  models = import ../../modules/dev/llms/models.nix;
in
{
  # The routes and models of quine's CLIProxyAPI, with keys from moot-ward.
  modules.services.cliproxy-api.settings = {
    server.trusted-proxies = [
      "127.0.0.1"
      "::1"
    ];
    management = {
      allow-remote = true;
      secret-key = "op://moot-ward/CLIProxyAPI/management-secret-unhashed";
    };
    access.api-keys = [
      "op://moot-ward/CLIProxyAPI/credential"
      "op://moot-ward/CLIProxyAPI hirde/credential"
    ];
    api-keys.openai-compatibility = [
      {
        name = "openrouter";
        base-url = "https://openrouter.ai/api/v1";
        keys = [ { api-key = "op://moot-ward/OpenRouter/credential"; } ];
        models = map (name: { inherit name; }) models.openrouter;
      }
      {
        name = "subq";
        base-url = "https://api.subq.ai/v1";
        keys = [ { api-key = "op://moot-ward/Subq/credential"; } ];
        models = [ { name = models.subq.savant.id; } ];
      }
      {
        name = "mistral";
        base-url = "https://api.mistral.ai/v1";
        keys = [ { api-key = "op://moot-ward/Mistral/credential"; } ];
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
