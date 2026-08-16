{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks = {
        home-ops-caddy = import ../tests/home-ops-caddy.nix { inherit inputs pkgs; };
        deploy = import ../tests/deploy.nix {
          inherit pkgs;
          inherit (pkgs) deploy;
        };
        hindsight = import ../tests/hindsight.nix {
          inherit inputs pkgs;
        };
        james-haproxy-caddy-proxy-protocol = import ../tests/james-haproxy-caddy-proxy-protocol.nix {
          inherit pkgs;
        };
        james-caddy = import ../tests/james-caddy.nix { inherit inputs pkgs; };
        james-crowdsec = import ../tests/james-crowdsec.nix { inherit inputs pkgs; };
        james-pocket-id = import ../tests/james-pocket-id.nix { inherit inputs pkgs; };
        james-webhook = import ../tests/james-webhook.nix { inherit inputs pkgs; };
        local-issues = import ../tests/local-issues.nix { inherit inputs pkgs; };
        ocis = import ../tests/ocis.nix { inherit inputs pkgs; };
        paperless = import ../tests/paperless.nix { inherit inputs pkgs; };
        linux-voice-assistant = import ../tests/linux-voice-assistant.nix {
          inherit inputs pkgs;
        };
        linux-voice-assistant-output-only = import ../tests/linux-voice-assistant-output-only.nix {
          inherit inputs pkgs;
        };
      };
    };
}
