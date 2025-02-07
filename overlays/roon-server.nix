final: prev: {
  #roon-server = prev.roon-server.overrideAttrs (old: {
  #  version = "2.0-1470";
  #  src = prev.fetchurl {
  #    url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001470.tar.bz2";
  #    hash = "sha256-esaxrSdvl1qUNfotOSs8Tj/AUg6hFpl23DGbji/uFO8=";
  #  };
  #});
}
