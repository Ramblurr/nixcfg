{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (config.networking) hostName;
  nadApiPort = config.repo.secrets.home-ops.ports.nad-api;
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ./networking.nix
    ./nixbot.nix
    ./prometheus.nix
    #./grafana
    ../../config
    ../../config/home-ops.nix
    ../../modules/site-net
  ];
  system.stateVersion = "24.05";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  security.rtkit.enable = true;

  environment.systemPackages = [
    pkgs.alsa-utils
    pkgs.pipewire
    pkgs.wireplumber
    pkgs.pulsemixer
    pkgs.jless
    pkgs.linux-voice-assistant-unstable
    pkgs.gptfdisk
    pkgs.parted
  ];
  users.users.ramblurr = {
    extraGroups = [ "pipewire" ];
    linger = true;
  };
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = false;
    jack.enable = false;
    wireplumber.enable = true;
    audio.enable = true;
    systemWide = true;
  };
  home-ops = {
    enable = true;
    containers.enable = false;
    hypervisor.enable = true;
    apps = {
      hindsight.enable = true;
    };
  };
  modules.services.caddy.edge = {
    certificateHosts = map (host: "${host}.${config.repo.secrets.global.domain.home}") [
      "hindsight"
      "home"
      "nad"
      "octoprint"
    ];
    acmeEmail = config.repo.secrets.global.email.acme;
    redirectStatus = 301;
  };
  modules.services.hindsight = {
    llm = {
      retain.profile = "openai-codex-gpt-5.4-mini";
      consolidation.profile = "openai-codex-gpt-5.4-mini";
      #consolidation.profile = "cerebras-gpt-oss-120b";
      reflect.profile = "openai-codex-gpt-5.4-mini";
    };
    embeddings.profile = "local-bge-small-en-v1.5";
    # Temporary CPU-only profile to limit contention between local Hindsight jobs.
    extraEnvironment = {
      # Active GPU reranker.
      HINDSIGHT_API_RERANKER_PROVIDER = "tei";
      HINDSIGHT_API_RERANKER_TEI_URL = "http://10.9.4.3:8082";
      HINDSIGHT_API_RERANKER_TEI_BATCH_SIZE = "128";
      HINDSIGHT_API_RERANKER_TEI_MAX_CONCURRENT = "4";
      HINDSIGHT_API_RERANKER_TEI_HTTP_TIMEOUT = "5";
      HINDSIGHT_API_RERANKER_MAX_CANDIDATES = "1000";

      # Used only when RERANKER_PROVIDER is changed back to local.
      HINDSIGHT_API_RERANKER_LOCAL_BUCKET_BATCHING = "true";
      HINDSIGHT_API_RERANKER_LOCAL_MAX_CONCURRENT = "1";

      # Workload controls.
      HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT = "3";
      HINDSIGHT_API_CONSOLIDATION_LLM_PARALLELISM = "3";
      #HINDSIGHT_API_RETAIN_BATCH_ENABLED = "true";
      HINDSIGHT_API_RETAIN_LLM_MAX_CONCURRENT = "8";
      HINDSIGHT_API_WORKER_MAX_SLOTS = "4";
    };
  };
  home.nix-lan-cache.enable = true;
  myhm = _: {
    #home.persistence."/persist${ramblurr.homeDirectory}" = {
    #  directories = [ { directory = "work"; } ];
    #};
  };

  # Merge in the site secrets
  inherit (config.repo.secrets.site) site;
  systemd.network = {
    links = {
      "10-lan0" = {
        matchConfig.MACAddress = config.repo.secrets.site.site.hosts.debord.interfaces.lan0.hwaddr;
        linkConfig.Name = "lan0";
      };
      "10-lan1" = {
        matchConfig.MACAddress = config.repo.secrets.local.lan1.hwaddr;
        linkConfig.Name = "lan1";
      };
    };

    networks = {
      "10-lan1" =
        let

          hostConfig = config.site.hosts.${hostName};
          hostBridges = lib.naturalSort (
            lib.mori.keys (lib.mori.filter (_: iface: iface.type == "bridge") hostConfig.interfaces)
          );
          vlansForThisIface = lib.mori.filter (
            bridgeName:
            (hostConfig.interfaces.${bridgeName}.parent != null)
            && (hostConfig.interfaces.${bridgeName}.parent == "lan1")
          ) hostBridges;
        in
        {
          matchConfig.Name = "lan1";
          networkConfig = {
            DHCPServer = false;
            VLAN = map (net: "vlan-${net}") vlansForThisIface;
            LinkLocalAddressing = false;
            LLDP = true;
            EmitLLDP = true;
            Description = "I am the 10gbe sfp+ link";
          };
          linkConfig = {
            MTUBytes = 9000;
            RequiredForOnline = "carrier";
          };
        };
    };
  };
  modules.server.virtd-host.net.prim.iface = "prim";
  services.linux-voice-assistant = {
    enable = true;
    openFirewall = true;
    user = "ramblurr";
    group = "audio";
    name = "kitchen-announce-satellite";
    audioOutputDevice = "pipewire";
    outputOnly = true;
  };
  services.nad-api = {
    enable = true;
    openFirewall = true;
    user = "ramblurr";
    group = "ramblurr";
    devices = [
      {
        name = "nadt778";
        host = "nad-t778-living-room.prim.${config.repo.secrets.global.domain.home}";
        port = 23;
      }
    ];
    http.ip = "127.0.0.1";
    http.port = nadApiPort;
  };
  modules.services.caddy.routes.nad = {
    publicHost = "nad.${config.repo.secrets.global.domain.home}";
    upstream = "http://127.0.0.1:${toString nadApiPort}";
  };
}
