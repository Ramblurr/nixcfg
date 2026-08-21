{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks = {
        atuin-postgresql = import ../tests/atuin-postgresql.nix { inherit inputs pkgs; };
        borgmatic = import ../tests/borgmatic.nix { inherit inputs pkgs; };
        calibre = import ../tests/calibre.nix { inherit inputs pkgs; };
        databasus = import ../tests/databasus.nix { inherit inputs pkgs; };
        git-archive = import ../tests/git-archive.nix { inherit inputs pkgs; };
        gatus-heartbeats = import ../tests/gatus-heartbeats.nix { inherit inputs pkgs; };
        home-ops-caddy = import ../tests/home-ops-caddy.nix { inherit inputs pkgs; };
        home-dl-qbittorrent = import ../tests/home-dl-qbittorrent.nix { inherit inputs pkgs; };
        invoiceninja = import ../tests/invoiceninja.nix { inherit inputs pkgs; };
        davis-onepassword-credentials = import ../tests/davis-onepassword-credentials.nix {
          inherit inputs pkgs;
        };
        matrix-postgres-incremental = import ../tests/matrix-postgres-incremental.nix {
          inherit inputs pkgs;
        };
        onepassword-connect-availability = import ../tests/onepassword-connect-availability.nix {
          inherit inputs pkgs;
        };
        onepassword-systemd-credentials = import ../tests/onepassword-systemd-credentials.nix {
          inherit inputs pkgs;
        };
        onepassword-systemd-credentials-vm = import ../tests/onepassword-systemd-credentials-vm.nix {
          inherit inputs pkgs;
        };
        mali-garage = import ../tests/mali-garage.nix { inherit inputs pkgs; };
        mali-zrepl-retention = import ../tests/mali-zrepl-retention.nix { inherit inputs pkgs; };
        mali-zrepl-reconcile = import ../tests/mali-zrepl-reconcile.nix { inherit inputs pkgs; };
        mali-zrepl-reconcile-vm = import ../tests/mali-zrepl-reconcile-vm.nix { inherit pkgs; };
        zrepl-outage-continuity = import ../tests/zrepl-outage-continuity.nix { inherit pkgs; };
        common-server = import ../tests/common-server.nix { inherit inputs pkgs; };
        debord-monitoring = import ../tests/debord-monitoring.nix { inherit inputs pkgs; };
        deploy = import ../tests/deploy.nix {
          inherit pkgs;
          inherit (pkgs) deploy;
        };
        garage-terranix = import ../tests/garage-terranix.nix { inherit inputs pkgs; };
        gatus-aggregation = import ../tests/gatus-aggregation.nix { inherit inputs pkgs; };
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
