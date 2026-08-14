{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks = {
        caddy-security = import ../tests/caddy-security.nix { inherit inputs pkgs; };
        deploy = import ../tests/deploy.nix {
          inherit pkgs;
          inherit (pkgs) deploy;
        };
        hindsight = import ../tests/hindsight.nix {
          inherit inputs pkgs;
        };
        james-crowdsec = import ../tests/james-crowdsec.nix { inherit inputs pkgs; };
        james-webhook = import ../tests/james-webhook.nix { inherit inputs pkgs; };
        local-issues = import ../tests/local-issues.nix { inherit inputs pkgs; };
        linux-voice-assistant = import ../tests/linux-voice-assistant.nix {
          inherit inputs pkgs;
        };
        linux-voice-assistant-output-only = import ../tests/linux-voice-assistant-output-only.nix {
          inherit inputs pkgs;
        };
        oauth2-proxy = import ../tests/oauth2-proxy.nix { inherit inputs pkgs; };
      };
    };
}
