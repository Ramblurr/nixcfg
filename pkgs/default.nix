inputs: [
  (_final: prev: {
    overseerr = prev.callPackage ./overseerr.nix { };
    cloudflare-utils = prev.callPackage ./cloudflare-utils.nix { };
    actual-server = prev.callPackage ./actual-server.nix { };
    qobuz-dl = prev.callPackage ./qobuz-dl.nix { };
    kwin6-bismuth-decoration = prev.callPackage ./kwin6-bismuth-decoration.nix { };
    klassy = prev.callPackage ./klassy.nix { };
    beets-dynamicrange = prev.callPackage ./beets-dynamicrange.nix {
      beets = prev.beetsPackages.beets-minimal;
    };
    beets-filetote = prev.callPackage ./beets-filetote.nix {
      poetry-core = prev.python311Packages.poetry-core;
      beets = prev.beetsPackages.beets-minimal;
    };
  })
]
