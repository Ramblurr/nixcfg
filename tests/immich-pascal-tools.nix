{
  lib,
  pkgs,
  ml ? pkgs.callPackage ../pkgs/immich-machine-learning-pascal.nix { },
}:
let
  portrait = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/immich-app/immich/8aa95c67470a02a8ddedf03c2e52963af33065ff/docs/docs/overview/img/alex-picture.jpeg";
    hash = "sha256-Vu8vCKHi3VWwJXtXzBc7yd1ORYaZO+xCFUqP5CYyH44=";
  };
in
pkgs.writeShellApplication {
  name = "immich-pascal-validate";
  runtimeInputs = [
    ml.validationPython
    pkgs.coreutils
  ];
  text = ''
    if [[ $# != 1 || $(id -un) != immich ]]; then
      echo "Run as immich: immich-pascal-validate NEW-RESULT-DIRECTORY" >&2
      exit 1
    fi
    # Refuse to overwrite a previous run or unrelated files.
    mkdir -- "$1"
    cd -- "$1"
    export LD_LIBRARY_PATH=/run/opengl-driver/lib
    export MACHINE_LEARNING_CACHE_FOLDER=/var/cache/immich
    export XDG_CACHE_HOME=/var/cache/immich
    export MPLCONFIGDIR=/var/cache/immich
    export PYTHONPATH=${lib.escapeShellArg "${ml}/${ml.python.sitePackages}:${ml.python.pkgs.makePythonPath ml.dependencies}"}
    timeout --kill-after=30s 15m python3 ${./immich-pascal-smoke.py} 2>&1 | tee smoke.log
    timeout --kill-after=30s 15m python3 ${./immich-pascal-models.py} ${portrait} 2>&1 | tee models.log
    timeout --kill-after=30s 15m python3 ${./immich-pascal-service.py} ${portrait} ocr-fixture.png ${pkgs.jellyfin-ffmpeg}/bin 2>&1 | tee service.log
  '';
  derivationArgs = {
    passthru.machineLearning = ml;
    meta = {
      description = "Opt-in GTX 1070 Ti hardware validation against the local Immich ML service";
      platforms = [ "x86_64-linux" ];
    };
  };
}
