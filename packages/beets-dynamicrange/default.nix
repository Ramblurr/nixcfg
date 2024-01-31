{
  lib,
  fetchFromGitHub,
  python3Packages,
  beets,
  dr14_tmeter,
}:
python3Packages.buildPythonApplication rec {
  pname = "beets-dynamicrange";
  version = "unstable-2023-12-05";

  src = fetchFromGitHub {
    owner = "auchter";
    repo = pname;
    rev = "62fc157f85293d1d2dcc36b5afa33d5322cc8c5f";
    sha256 = "sha256-ALNGrpZOKdUE3g4np8Ms+0s8uWi6YixF2IVHSgaQVj4=";
  };

  nativeBuildInputs = [beets];

  propagatedBuildInputs = [dr14_tmeter];

  postPatch = ''
    substituteInPlace beetsplug/dynamicrange.py \
      --replace dr14_tmeter ${dr14_tmeter}/bin/dr14_tmeter
  '';

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/auchter/beets-dynamicrange";
    description = "Calculate and store dynamic range of music for beets";
    license = licenses.mit;
    maintainers = with maintainers; [auchter];
    platforms = platforms.linux;
  };
}
