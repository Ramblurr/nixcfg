{
  lib,
  stdenvNoCC,
  fetchurl,
  ast-grep,
  nushellPlugins,
  pkgs-lib,
}:

let
  version = "0.0.1-rc29";

  sources = {
    aarch64-darwin.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-darwin-arm64.tar.gz";
    aarch64-darwin.hash = "sha256-HyEHGQF0S9BaKp80jZ8NzcwgtUFGrmycs5cJbCJJTs8=";
    x86_64-darwin.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-darwin-amd64.tar.gz";
    x86_64-darwin.hash = "sha256-dJLI0Cn+8MhOtf7Id6/Pnlxz1+GYYhTsQI56TgBmE6U=";
    aarch64-linux.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-linux-arm64.tar.gz";
    aarch64-linux.hash = "sha256-PECd9HjRFNLMfVBEvNZhyU1JsRdwOY2hmaOdyq4Qd9Y=";
    x86_64-linux.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-linux-amd64.tar.gz";
    x86_64-linux.hash = "sha256-4AVjv/ZWM4QYmOJFz9/ky1w3cO9Vc93Tq0Voa2dRfP4=";
  };

  platform = stdenvNoCC.hostPlatform.system;
  source = sources.${platform} or (throw "Unsupported platform: ${platform}");
in
stdenvNoCC.mkDerivation {
  pname = "sprite";
  inherit version;

  src = fetchurl {
    inherit (source) url hash;
  };

  sourceRoot = ".";

  unpackPhase = ''
    tar -xzf $src
  '';

  installPhase = ''
    install -Dm755 sprite $out/bin/sprite
  '';
  passthru = {
    updateScript = pkgs-lib.writeUpdateScript {
      packageToUpdate = "sprite-bin";

      utils = [
        ast-grep
      ];
      nushellPlugins = [ nushellPlugins.query ];

      script = ./update.nu;
    };
  };

  meta = {
    description = "CLI for Sprites - durable, interactive cloud sandboxes by Fly.io";
    homepage = "https://sprites.dev";
    license = lib.licenses.unfree;
    platforms = builtins.attrNames sources;
    mainProgram = "sprite";
  };
}
