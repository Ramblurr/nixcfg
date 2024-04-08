{
  stdenv,
  lib,
  fetchFromGitHub,
  cmake,
}:
# C libraries & daemon for pigpio
stdenv.mkDerivation rec {
  pname = "pigpio";
  version = "79";

  src = fetchFromGitHub {
    owner = "joan2937";
    repo = "pigpio";
    rev = "v${version}";
    sha256 = "sha256-Z+SwUlBbtWtnbjTe0IghR3gIKS43ZziN0amYtmXy7HE=";
  };

  #nativeBuildInputs = [cmake];

  makeFlags = [ "prefix=$(out)" ];

  postPatch = ''
    substituteInPlace Makefile \
      --replace '$(DESTDIR)/opt/pigpio/cgi' '$(DESTDIR)/$(prefix)/cgi' \
      --replace 'ldconfig' '# ldconfig'
  '';

  meta = with lib; {
    description = "A C library for the Raspberry Pi which allows control of the General Purpose Input Outputs (GPIO)";
    homepage = "http://abyz.me.uk/rpi/pigpio/";
    license = licenses.unlicense;
    platforms = [
      "aarch64-linux"
      "armv7l-linux"
    ];
    maintainers = [ ];
    isRpiPkg = true;
  };
}
