{buildFHSUserEnv}:
buildFHSUserEnv {
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
