{
  pkgs,
  lib,
  system ? builtins.currentSystem,
  extraAssets ? { }, # allow adding versions and hashes
}:
let
  # Mapping from version to per-platform asset info (fill in hashes!)
  version = "0.5.13";
  assets = {
    ${version} = {
      "x86_64-linux" = {
        name = "opencode-linux-x64.zip";
        hash = "sha256-jA+xfi/3GmguyR9fHGf1s9vENHpXZKOGSGkSB3o+JyM=";
      };
      "aarch64-darwin" = {
        name = "opencode-darwin-arm64.zip";
        hash = lib.fakeHash;
      };
    };
  }
  // extraAssets;

  platformAssets = assets.${version} or (throw "Unsupported version: ${version}");

  asset = platformAssets.${system} or (throw "Unsupported system for version ${version}: ${system}");
  url = "https://github.com/sst/opencode/releases/download/v${version}/${asset.name}";
in
pkgs.stdenv.mkDerivation {
  pname = "opencode";
  inherit version;

  src = pkgs.fetchurl {
    inherit url;
    hash = asset.hash;
  };
  dontUnpack = true;

  buildInputs = [ ];
  nativeBuildInputs = [ pkgs.unzip ];

  installPhase = ''
    mkdir -p $out/bin
    unzip -j $src
    # Adjust if more files in archive
    cp opencode $out/bin/
  '';

  meta = {
    mainProgram = "opencode";
    description = "OpenCode: AI coding agent for the terminal";
    homepage = "https://github.com/sst/opencode";
  };
}
