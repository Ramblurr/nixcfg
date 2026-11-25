{
  lib,
  stdenvNoCC,
  babashka,
  coreutils,
  iproute2,
  makeWrapper,
  systemd,
}:
stdenvNoCC.mkDerivation {
  pname = "motd";
  version = "0.1.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./src
      ./test
    ];
  };
  nativeBuildInputs = [
    babashka
    makeWrapper
  ];
  dontConfigure = true;
  # The wrapper runs bb directly. Fixup has no bb to resolve the env -S shebang.
  dontPatchShebangs = true;

  buildPhase = ''
    runHook preBuild
    ${babashka}/bin/bb --config /dev/null --classpath src \
      uberscript uberscript.clj -m motd.main
    {
      printf '#!/usr/bin/env -S bb --config /dev/null\n'
      cat uberscript.clj
    } > motd
    chmod +x motd
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    app="$out/share/motd"
    mkdir -p "$app" "$out/bin"
    install -m755 motd "$app/motd"
    makeWrapper ${babashka}/bin/bb "$out/bin/motd" \
      --prefix PATH : ${
        lib.makeBinPath [
          coreutils
          iproute2
          systemd
        ]
      } \
      --add-flags "--config /dev/null $app/motd"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    ${babashka}/bin/bb --config /dev/null --classpath src:test \
      -e "(require 'clojure.test 'motd.collect-test 'motd.render-test) (let [r (clojure.test/run-tests 'motd.collect-test 'motd.render-test)] (System/exit (+ (:fail r) (:error r))))"

    # Verify the installed script runs outside the checkout and beside an unrelated bb.edn.
    mkdir -p "$TMPDIR/unrelated"
    printf '{:deps {nonexistent/pkg {:mvn/version "0.0.0"}}}\n' > "$TMPDIR/unrelated/bb.edn"
    (cd "$TMPDIR/unrelated" && "$out/bin/motd" --help >/dev/null)
    (cd "$TMPDIR/unrelated" && "$out/bin/motd" --demo host --no-color >/dev/null)
    runHook postInstallCheck
  '';

  meta = {
    description = "Fast login status summary for NixOS hosts and guests";
    mainProgram = "motd";
    platforms = lib.platforms.linux;
  };
}
