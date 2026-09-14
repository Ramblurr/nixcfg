{
  lib,
  stdenvNoCC,
  fetchurl,
}:
stdenvNoCC.mkDerivation {
  pname = "thunderbird-ai-bridge";
  version = "2.0.0";

  src = fetchurl {
    url = "https://github.com/vitalio-sh/thunderbird-cli/releases/download/v1.0.2/thunderbird_ai_bridge-2.0.0-tb.xpi";
    hash = "sha256-+s32NlRhmi5cpTn3IUwaY2mIP1ojQ42WYyXY+EIktsw=";
  };

  dontUnpack = true;
  installPhase = ''
    runHook preInstall
    install -Dm644 $src \
      $out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/thunderbird-ai@extension.xpi
    runHook postInstall
  '';

  meta = {
    description = "Thunderbird add-on connecting email to the Thunderbird CLI bridge";
    homepage = "https://github.com/vitalio-sh/thunderbird-cli";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
