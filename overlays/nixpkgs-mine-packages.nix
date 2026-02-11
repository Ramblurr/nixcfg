# Overlay to selectively import packages from nixpkgs-mine
inputs: final: _prev:
let
  # Import nixpkgs-mine with the same system and config as the main nixpkgs
  nixpkgs-mine = import inputs.nixpkgs-mine {
    inherit (final.stdenv.hostPlatform) system;
    inherit (final) config;
  };
in
{
  # Packages that should come from nixpkgs-mine instead of regular nixpkgs
  # claude-code = (nixpkgs-mine.claude-code.override { nodejs_20 = nixpkgs-mine.nodejs_24; });
  #claude-code = nixpkgs-mine.claude-code;
  #codex = nixpkgs-mine.codex;
  #opencode = nixpkgs-mine.opencode;
  #gemini-cli = nixpkgs-mine.gemini-cli;
  #crush = nixpkgs-mine.crush;
  inherit (nixpkgs-mine) llm;
  inherit (nixpkgs-mine) clojure-lsp;
  inherit (nixpkgs-mine) davis;
  inherit (nixpkgs-mine) plex;
  inherit (nixpkgs-mine) emacs-pgtk;
  inherit (nixpkgs-mine) gost;
  inherit (nixpkgs-mine) ocis;
  inherit (nixpkgs-mine) ocis_71-bin;
  inherit (nixpkgs-mine) ocis_72-bin;
  inherit (nixpkgs-mine) ocis_73-bin;
  inherit (nixpkgs-mine) roon-server;
  inherit (nixpkgs-mine) yt-dlp;
  #chromium = nixpkgs-mine.chromium;
  #chromedriver = nixpkgs-mine.chromedriver;
  inherit (nixpkgs-mine) babashka;
  inherit (nixpkgs-mine) bbin;
  inherit (nixpkgs-mine) jdk25;
  inherit (nixpkgs-mine) jdk25_headless;
  inherit (nixpkgs-mine) yubioath-flutter;
  inherit (nixpkgs-mine) _1password-gui;
  inherit (nixpkgs-mine) _1password-cli;
  inherit (nixpkgs-mine) obs-cmd;
  sops = nixpkgs-mine.sops.withAgePlugins (p: [
    p.age-plugin-fido2-hmac
    p.age-plugin-yubikey
    p.age-plugin-tpm
  ]);
}
