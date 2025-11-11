# Overlay to selectively import packages from nixpkgs-mine
inputs: final: prev:
let
  # Import nixpkgs-mine with the same system and config as the main nixpkgs
  nixpkgs-mine = import inputs.nixpkgs-mine {
    inherit (final) system;
    config = final.config;
  };
in
{
  # Packages that should come from nixpkgs-mine instead of regular nixpkgs
  # claude-code = (nixpkgs-mine.claude-code.override { nodejs_20 = nixpkgs-mine.nodejs_24; });
  claude-code = nixpkgs-mine.claude-code;
  codex = nixpkgs-mine.codex;
  opencode = nixpkgs-mine.opencode;
  gemini-cli = nixpkgs-mine.gemini-cli;
  clojure-lsp = nixpkgs-mine.clojure-lsp;
  davis = nixpkgs-mine.davis;
  plex = nixpkgs-mine.plex;
  emacs-pgtk = nixpkgs-mine.emacs-pgtk;
  gost = nixpkgs-mine.gost;
  ocis = nixpkgs-mine.ocis;
  ocis_71-bin = nixpkgs-mine.ocis_71-bin;
  roon-server = nixpkgs-mine.roon-server;
  #chromium = nixpkgs-mine.chromium;
  #chromedriver = nixpkgs-mine.chromedriver;
  crush = nixpkgs-mine.crush;
  llm = nixpkgs-mine.llm;
  jdk25 = nixpkgs-mine.jdk25;
  jdk25_headless = nixpkgs-mine.jdk25_headless;
  yubioath-flutter = nixpkgs-mine.yubioath-flutter;
  _1password-gui = nixpkgs-mine._1password-gui;
  _1password-cli = nixpkgs-mine._1password-cli;
  sops = (
    nixpkgs-mine.sops.withAgePlugins (p: [
      p.age-plugin-fido2-hmac
      p.age-plugin-yubikey
      p.age-plugin-tpm
    ])
  );
}
