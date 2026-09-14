{
  lib,
  stdenvNoCC,
  fetchurl,
}:
stdenvNoCC.mkDerivation rec {
  pname = "betterunsubscribe";
  version = "2.9.1";

  src = fetchurl {
    url = "https://addons.thunderbird.net/thunderbird/downloads/file/1046526/betterunsubscribe-${version}-tb.xpi";
    hash = "sha256-QA4zeHj1Ht1sXn6ySyUqs3/cN6mKpiKsPNgD15Yzd6A=";
  };

  dontUnpack = true;
  installPhase = ''
    runHook preInstall
    install -Dm644 $src \
      $out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{4753278b-acea-4b2b-a111-1fc9450d239d}.xpi
    runHook postInstall
  '';

  meta = {
    description = "Unsubscribe from mailing lists in Thunderbird";
    homepage = "https://addons.thunderbird.net/thunderbird/addon/betterunsubscribe/";
    license = lib.licenses.mpl20;
    platforms = lib.platforms.all;
  };
}
