{
  lib,
  gnused,
  python312,
  fetchFromGitHub,
}:

python312.pkgs.buildPythonApplication rec {
  pname = "jellyplex-watched";
  version = "8.5.3";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "luigi311";
    repo = "JellyPlex-Watched";
    rev = "v${version}";
    hash = "sha256-H20nVkuAcJ8nA20ZGxIJqxC55roN5XPZv/Bc4jxxSZc=";
  };

  nativeBuildInputs = [ python312.pkgs.wrapPython ];

  build-system = [ python312.pkgs.setuptools ];

  dependencies = [
    python312.pkgs.loguru
    python312.pkgs.packaging
    python312.pkgs.plexapi
    python312.pkgs.pydantic
    python312.pkgs.python-dotenv
    python312.pkgs.requests
  ];

  pythonRelaxDeps = true;

  postPatch = ''
    ${gnused}/bin/sed -i "1s|^|#!\/usr/bin/env python3\n\n|" main.py

    substituteInPlace $(grep -rl "from src\." main.py src) \
      --replace-fail "from src." "from "
  '';

  postInstall = ''
    install -Dm755 main.py $out/bin/jellyplex-watched
  '';

  meta = {
    description = "Sync watched status between Jellyfin, Plex, and Emby locally";
    homepage = "https://github.com/luigi311/JellyPlex-Watched";
    license = lib.licenses.gpl3Only;
    mainProgram = "jellyplex-watched";
  };
}
