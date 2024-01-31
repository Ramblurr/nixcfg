{pkgs}:
pkgs.lib.makeScope pkgs.newScope (self: let
  callPackage = self.callPackage;
in {
  microsocks = callPackage ./microsocks {};
  hacompanion = callPackage ./hacompanion {};
  bootleg = callPackage ./bootleg {};
  filetote = callPackage ./filetote {};
  beets-audible = callPackage ./beets-audible {};
  beets-filetote = callPackage ./beets-filetote {inherit (pkgs.my) beets-audible;};
  muse-sounds-deb = callPackage ./muse-sounds-deb {inherit pkgs;};
  #muse-sounds-manager = callPackage ./muse-sounds-manager {};
  muse-sounds-manager = (
    pkgs.buildFHSUserEnv {
      name = "muse-sounds-manager";
      targetPkgs = pkgs: (with pkgs; [
        # dotnet
        curl
        icu
        libunwind
        libuuid
        openssl
        zlib

        # mono
        krb5

        pkgs.my.muse-sounds-deb
      ]);
      multiPkgs = pkgs: [pkgs.dpkg];
      runScript = "/opt/muse-sounds-manager/Muse.Client.Linux";
    }
  );
})
