{pkgs-stable}:
pkgs-stable.lib.makeScope pkgs-stable.newScope (self: let
  pkgs = pkgs-stable;
  callPackage = self.callPackage;
in {
  microsocks = callPackage ./microsocks {};
  hacompanion = callPackage ./hacompanion {};
  bootleg = callPackage ./bootleg {};
  beets-filetote = callPackage ./beets-filetote {
    inherit pkgs;
    poetry-core = pkgs.python311Packages.poetry-core;
    beets = pkgs.beetsPackages.beets-minimal;
  };
  beets-dynamicrange = pkgs.callPackage ./beets-dynamicrange {
    beets = pkgs.beetsPackages.beets-minimal;
  };
  muse-sounds-deb = callPackage ./muse-sounds-deb {inherit pkgs;};
  muse-sounds-manager = callPackage ./muse-sounds-manager {};
  #pigpio = callPackage ./pigpio {};
})
