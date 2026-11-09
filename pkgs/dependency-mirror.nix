{ lib, pkgs }:
let
  sdk = pkgs.fetchurl {
    url = "https://repo.clojars.org/dev/data-star/clojure/sdk/1.0.0-RC13/sdk-1.0.0-RC13.jar";
    hash = "sha256-6cvp0xKX5wF4ifYTWjOMhagSVnnhq8KmBvhr5mGW36w=";
  };
  adapter = pkgs.fetchurl {
    url = "https://repo.clojars.org/dev/data-star/clojure/http-kit/1.0.0-RC13/http-kit-1.0.0-RC13.jar";
    hash = "sha256-hix20j7b5+lEDe+2HhM/dBqAcwbJl0lhalKl0hYAbZo=";
  };
  datastar = pkgs.fetchurl {
    url = "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js";
    hash = "sha256-cnhErfyCXuZR+5PFRKKnOZhvmiGCCpRSSzXwysRwz5E=";
  };
in
pkgs.writeShellApplication {
  name = "dependency-mirror";
  runtimeInputs = [
    pkgs.git
    pkgs.gickup
  ];
  text = ''
    export DEPENDENCY_MIRROR_PACKAGED=1
    export DEPENDENCY_MIRROR_DATASTAR=${datastar}
    exec ${lib.getExe pkgs.babashka} --classpath "${sdk}:${adapter}" ${../scripts/dependency-mirror.clj} "$@"
  '';
}
