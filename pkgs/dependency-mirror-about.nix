{ pkgs }:
let
  python = pkgs.python3.withPackages (ps: [
    ps.markdown
    ps.bleach
  ]);
in
pkgs.runCommand "dependency-mirror-about" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
  mkdir -p $out/bin
  makeWrapper ${python}/bin/python $out/bin/dependency-mirror-about \
    --add-flags ${./dependency-mirror-about.py} \
    --prefix PATH : ${pkgs.asciidoctor}/bin
  ${python}/bin/python ${../tests/dependency-mirror-about.py} $out/bin/dependency-mirror-about
''
