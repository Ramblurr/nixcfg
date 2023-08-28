{pkgs}:
pkgs.lib.makeScope pkgs.newScope (self: let
  callPackage = self.callPackage;
in {
  microsocks = callPackage ./microsocks {};
})
