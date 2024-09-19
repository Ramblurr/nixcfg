final: prev: {
  roon-server = prev.roon-server.overrideAttrs (old: {
    version = "2.0-1455";
    src = prev.fetchurl {
      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001455.tar.bz2";
      hash = "sha256-R555u33S5jmqIKdDtRhMbfCMe5sG3x94naPX+qgrwHY=";
    };
  });
}
