inputs: [
  (_final: prev: {
    nvidia = prev.callPackage ./nvidia/package.nix { inherit inputs; };
    pdns-unstable = prev.callPackage ./pdns-unstable/package.nix { };
    gitbutler-bin = prev.callPackage ./gitbutler-bin.nix { };
    java-mission-control = prev.callPackage ./java-mission-control { };
    netns-proxy = prev.callPackage ./netns-proxy.nix { };
    mcp-inspector = prev.callPackage ./mcp-inspector.nix { };
    udpbroadcastrelay = prev.callPackage ./udpbroadcastrelay.nix { };
    brepl = (import ./brepl.nix) prev;
    deploy = prev.callPackage ./deploy.nix { };
    swhkd = prev.callPackage ./swhkd { };
    overseerr = prev.callPackage ./overseerr/package.nix { };
    actual-server = prev.callPackage ./actual-server.nix { };
    qobuz-dl = prev.callPackage ./qobuz-dl.nix { };
    #kwin6-bismuth-decoration = prev.callPackage ./kwin6-bismuth-decoration.nix { };
    #klassy = prev.callPackage ./klassy.nix { };
    #invoiceninja-mine = prev.callPackage ./invoiceninja/package.nix { };
    beets-dynamicrange = prev.callPackage ./beets-dynamicrange.nix {
      beets = prev.beetsPackages.beets-minimal;
    };
    beets-filetote = prev.callPackage ./beets-filetote.nix {
      inherit (prev.python3Packages) poetry-core;
      beets = prev.beetsPackages.beets-minimal;
    };
  })
]
