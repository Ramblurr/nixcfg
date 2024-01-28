{
  stdenv,
  pkgs,
}:
stdenv.mkDerivation rec {
  name = "muse-sounds-deb";
  builder = ./builder.sh;
  dpkg = pkgs.dpkg;
  src = pkgs.fetchurl {
    url = "https://muse-cdn.com/Muse_Sounds_Manager_Beta.deb";
    hash = "sha256-wzZAIjme1cv8+jMLiKT7kUQvCb+UhsvOnLDV4hCL3hw=";
  };
}
