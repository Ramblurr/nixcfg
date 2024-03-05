final: prev: {
  roon-server = prev.roon-server.overrideAttrs (old: {
    version = "2.0-1368";
    src = prev.fetchurl {
      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001368.tar.bz2";
      hash = "sha256-osyEiefd+Gb7Wfo7mDDeP2QHb2lUTD+dxBzMdsrof0M=";
    };
  });
}
