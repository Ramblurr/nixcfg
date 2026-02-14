{
  lib,
  stdenvNoCC,
  fetchurl,
  ast-grep,
  pkgs-lib,
}:

let
  version = "0.0.1-rc32";

  sources = {
    aarch64-darwin.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-darwin-arm64.tar.gz";
    aarch64-darwin.hash = "sha256-zRz7mKuI549UKStsdEd39I9SS+LJCJFPLWsn1jDtKRQ=";
    x86_64-darwin.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-darwin-amd64.tar.gz";
    x86_64-darwin.hash = "sha256-ZOEvdcNid4I2aPmpPLGH4VCwkPtyPlIItr6vLEPCx/s=";
    aarch64-linux.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-linux-arm64.tar.gz";
    aarch64-linux.hash = "sha256-81hgLzFYeCVVCHr/6rWNat3THoiEANBpEhqL7OWcfEs=";
    x86_64-linux.url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-linux-amd64.tar.gz";
    x86_64-linux.hash = "sha256-bd6rIFA5WE4z0iHHst9+ZdVWnATBSaKJGI0G4CX78g4=";
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
      script = ./update.bb;
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
