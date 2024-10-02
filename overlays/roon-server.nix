final: prev: {
  roon-server = prev.roon-server.overrideAttrs (old: {
    version = "2.0-1462";
    src = prev.fetchurl {
      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001462.tar.bz2";
      hash = "sha256-irTDjT9oN0CdxFKAgcHbL2grJ702E6H+WtifGh0pf0E=";
    };
  });
}
