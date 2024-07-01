final: prev: {
  roon-server = prev.roon-server.overrideAttrs (old: {
    version = "2.0-1432";
    src = prev.fetchurl {
      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001432.tar.bz2";
      hash = "sha256-h0Ly5S8ML29RtaZOpe0k4U/R0coClHHGUZyu5d1PqzQ=";
    };
  });
}
