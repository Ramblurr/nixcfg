{ pkgs-stable }:
pkgs-stable.lib.makeScope pkgs-stable.newScope (
  self:
  let
    pkgs = pkgs-stable;
    callPackage = self.callPackage;
  in
  {
    #mysql-backup = callPackage ./mysql-backup { };
    #hacompanion = callPackage ./hacompanion { };
    #bootleg = callPackage ./bootleg { };
    beets-filetote = callPackage ./beets-filetote {
      inherit pkgs;
      poetry-core = pkgs.python311Packages.poetry-core;
      beets = pkgs.beetsPackages.beets-minimal;
    };
    beets-dynamicrange = pkgs.callPackage ./beets-dynamicrange {
      beets = pkgs.beetsPackages.beets-minimal;
    };
    #muse-sounds-deb = callPackage ./muse-sounds-deb { inherit pkgs; };
    #muse-sounds-manager = callPackage ./muse-sounds-manager { };
    overseerr = callPackage ./overseerr { };
    actual-server = callPackage ./actual-server { };
    cloudflare-utils = callPackage ./cloudflare-utils { };
  }
)
