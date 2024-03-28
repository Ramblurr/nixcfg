final: prev: {
  roon-server = prev.roon-server.overrideAttrs (old: {
    version = "2.0-1388";
    src = prev.fetchurl {
      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001368.tar.bz2";
      hash = "sha256-FH5edAtPS7qPtShGz1paEmL1O5xDmCLTRvEWFPiiVjg=";
    };
  });
}
