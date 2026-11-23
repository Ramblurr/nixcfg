{
  lib,
  stdenvNoCC,
  runCommand,
  google-fonts,
  python3,
  resvg,
}:
let
  generator = ./catalog.py;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "label-fonts";
  inherit (google-fonts) version src;

  dontUnpack = true;
  nativeBuildInputs = [
    python3
    resvg
  ];

  # One directory per google/fonts family directory, so file names never collide.
  installPhase = ''
    runHook preInstall
    fonts=$out/share/fonts
    python3 -I ${generator} select "$src" > families
    while read -r dir; do
      install -m 444 -Dt "$fonts/$dir" "$src/$dir"/*.ttf
    done < families
    mkdir -p "$out/share/label-fonts"
    python3 -I ${generator} google "$src" "$fonts" "$out/share/label-fonts/google.json"
    runHook postInstall
  '';

  passthru = {
    fontsDir = "${finalAttrs.finalPackage}/share/fonts";
    # The catalog for SVG rendering over this package's fonts plus bundledFontDirs.
    mkCatalog =
      bundledFontDirs:
      runCommand "label-fonts-catalog.json"
        {
          nativeBuildInputs = [
            python3
            resvg
          ];
        }
        ''
          python3 -I ${generator} merge ${finalAttrs.finalPackage}/share/label-fonts/google.json "$out" \
            ${lib.escapeShellArgs ([ finalAttrs.finalPackage.fontsDir ] ++ bundledFontDirs)}
        '';
  };

  meta = {
    description = "Latin Google Fonts families and their catalog for SVG label rendering";
    homepage = "https://fonts.google.com";
    license = [
      lib.licenses.asl20
      lib.licenses.ofl
      lib.licenses.ufl
    ];
    platforms = lib.platforms.all;
    sourceProvenance = [ lib.sourceTypes.binaryBytecode ];
  };
})
