inputs: [
  (
    _final: prev:
    let
      # Get pkgs-lib
      pkgs-lib = prev.callPackage ../lib/pkgs.nix { flake-inputs = inputs; };
    in
    {
      nvidia = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./nvidia/package.nix {
        inherit inputs;
      };
      pdns-unstable = prev.callPackage ./pdns-unstable/package.nix { };
      gitbutler-bin = prev.callPackage ./gitbutler-bin.nix { };
      java-mission-control = prev.callPackage ./java-mission-control { };
      netns-proxy = prev.callPackage ./netns-proxy.nix { };
      mcp-inspector = prev.callPackage ./mcp-inspector.nix { };
      udpbroadcastrelay = prev.callPackage ./udpbroadcastrelay.nix { };
      brepl = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./brepl/package.nix { };
      #linux-voice-assistant = (import ./linux-voice-assistant.nix) prev;
      linux-voice-assistant-unstable = (import ./linux-voice-assistant-unstable.nix) prev;
      deploy = prev.callPackage ./deploy.nix { };
      swhkd = prev.callPackage ./swhkd { };
      overseerr = prev.callPackage ./overseerr/package.nix { };
      qobuz-dl = prev.callPackage ./qobuz-dl.nix { };
      waytray = prev.callPackage ./waytray.nix { };
      sprite-bin = prev.lib.callPackageWith (prev // { inherit pkgs-lib; }) ./sprite-bin/package.nix { };
      #kwin6-bismuth-decoration = prev.callPackage ./kwin6-bismuth-decoration.nix { };
      #klassy = prev.callPackage ./klassy.nix { };
      #invoiceninja-mine = prev.callPackage ./invoiceninja/package.nix { };

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
