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
  claude-code = nixpkgs-mine.claude-code;
  clojure-lsp = nixpkgs-mine.clojure-lsp;
  davis = nixpkgs-mine.davis;
  emacs-pgtk = nixpkgs-mine.emacs-pgtk;
  gost = nixpkgs-mine.gost;
  ocis = nixpkgs-mine.ocis;
  ocis_71-bin = nixpkgs-mine.ocis_71-bin or null;
  roon-server = nixpkgs-mine.roon-server;
  chromium = nixpkgs-mine.chromium;
  chromedriver = nixpkgs-mine.chromedriver;
}
