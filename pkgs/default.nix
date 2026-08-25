inputs: [
  (
    _final: prev:
    let
      # Get pkgs-lib
      pkgs-lib = prev.callPackage ../lib/pkgs.nix { flake-inputs = inputs; };
    in
    {
      caddy-with-security = prev.callPackage ./caddy/package.nix {
        buildPkgs = inputs.nixpkgs.legacyPackages.${prev.stdenv.hostPlatform.system};
      };
      nvidia = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./nvidia/package.nix {
        kernelPackages = prev.linuxPackages;
      };
      # TODO: remove when nixpkgs Jet works with GraalVM 25.2.4.
      jet =
        if prev.stdenv.hostPlatform.system == "x86_64-linux" then
          prev.stdenvNoCC.mkDerivation (finalAttrs: {
            pname = "jet";
            inherit (prev.jet) version;
            src = prev.fetchurl {
              url = "https://github.com/borkdude/jet/releases/download/v${finalAttrs.version}/jet-${finalAttrs.version}-linux-amd64.tar.gz";
              hash = "sha256-QR5ly+bqlOpplOInI8vHOEPHFeKYKMs/uMpsWvY5po0=";
            };
            sourceRoot = ".";
            dontConfigure = true;
            dontBuild = true;
            installPhase = ''
              runHook preInstall
              install -Dm755 jet $out/bin/jet
              runHook postInstall
            '';
            meta = prev.jet.meta // {
              sourceProvenance = [ prev.lib.sourceTypes.binaryNativeCode ];
            };
          })
        else
          prev.jet;
      terraform-provider-powerdns = prev.lib.callPackageWith (
        prev // { inherit pkgs-lib; }
      ) ./terraform-providers/powerdns.nix { };
      terraform-provider-desec = prev.lib.callPackageWith (
        prev // { inherit pkgs-lib; }
      ) ./terraform-providers/desec.nix { };
      terraform-provider-garage = prev.lib.callPackageWith (
        prev // { inherit pkgs-lib; }
      ) ./terraform-providers/garage.nix { };
      opentofu-powerdns = prev.opentofu.withPlugins (_plugins: [ _final.terraform-provider-powerdns ]);
      opentofu-garage = prev.opentofu.withPlugins (_plugins: [ _final.terraform-provider-garage ]);
      opentofu-dns = prev.opentofu.withPlugins (_plugins: [
        _final.terraform-provider-desec
        _final.terraform-provider-powerdns
      ]);
      # webkitgtk_4_0' has been removed, port to `libsoup_3` and switch to `webkitgtk_4_1
      #java-mission-control = prev.callPackage ./java-mission-control { };
      netns-proxy = prev.callPackage ./netns-proxy.nix { };
      mcp-inspector = prev.callPackage ./mcp-inspector.nix { };
      pi-web = prev.callPackage ./pi-web/package.nix { };
      pi-nrepl = prev.callPackage ./pi/pi-nrepl/package.nix { };
      pi-hashline-edit-pro = prev.callPackage ./pi/pi-hashline-edit-pro/package.nix { };
      plannotator-pi-extension = prev.callPackage ./pi/plannotator/package.nix { };
      epimetheus = prev.callPackage ./pi/epimetheus/package.nix { };
      pi-mcp-adapter = prev.callPackage ./pi/pi-mcp-adapter/package.nix { };
      pi-link = prev.callPackage ./pi/pi-link/package.nix { };
      pi-reload = prev.callPackage ./pi/pi-reload/package.nix { };
      pi-heartbeat = prev.callPackage ./pi/pi-heartbeat/package.nix { };
      pi-ghost = prev.callPackage ./pi/pi-ghost/package.nix { };
      brepl-balance = prev.callPackage ./pi/brepl-balance/package.nix { };
      pi-link-control = prev.callPackage ./pi/pi-link-control/package.nix { };
      pi-nono-sandbox = prev.callPackage ./pi/pi-nono-sandbox/package.nix { };
      pi-openai-fast = prev.callPackage ./pi/pi-openai-fast/package.nix { };
      pi-sexp-edit = prev.callPackage ./pi/pi-sexp-edit/package.nix { };
      pi-matrix-relay = prev.callPackage ./pi/pi-matrix-relay/package.nix { };
      glimpseui = prev.callPackage ./glimpseui/package.nix { };
      udpbroadcastrelay = prev.callPackage ./udpbroadcastrelay.nix { };
      brepl = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./brepl/package.nix { };
      spdx-util = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./spdx-util/package.nix { };
      nixbot-cli = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./nixbot-cli/package.nix { };
      hindsight-cli = prev.lib.callPackageWith (
        prev // { inherit pkgs-lib; }
      ) ./hindsight-cli/package.nix { };
      #linux-voice-assistant = (import ./linux-voice-assistant.nix) prev;
      linux-voice-assistant-unstable = (import ./linux-voice-assistant-unstable.nix) prev;
      youtube-to-rss = prev.callPackage ./youtube-to-rss/package.nix { };
      deploy = prev.callPackage ./deploy.nix { };
      swhkd = prev.callPackage ./swhkd { };
      qobuz-dl = prev.callPackage ./qobuz-dl.nix { };
      waytray = prev.callPackage ./waytray.nix { };

      # 2026-01: disabled due to not building, they want poetry1 but nixpkgs only has poetry 2
      # ref: https://github.com/gtronset/beets-filetote/pull/202
      #beets-dynamicrange = prev.callPackage ./beets-dynamicrange.nix {
      #  #beets = prev.beetsPackages.beets-minimal;
      #  beets = prev.beets-minimal;
      #};
      #beets-filetote = prev.callPackage ./beets-filetote.nix {
      #  #beets = prev.beetsPackages.beets-minimal;
      #  beets = prev.beets-minimal;
      #};
    }
  )
]
