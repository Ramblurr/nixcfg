{
  lib,
  stdenvNoCC,
  fetchurl,
}:
stdenvNoCC.mkDerivation {
  pname = "thunderbird-ai-bridge";
  version = "2.1.0";

  src = fetchurl {
    url = "https://github.com/vitalio-sh/thunderbird-cli/releases/download/v1.1.0/thunderbird_ai_bridge-2.1.0-tb.xpi";
    hash = "sha256-mZo+9VAVPUmUYAc3UQk3nTxp0BcB5e4vXfKjq0Pzvck=";
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
