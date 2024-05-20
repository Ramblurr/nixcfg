final: prev: {
  roon-server = prev.roon-server.overrideAttrs (old: {
    version = "2.0-1413";
    src = prev.fetchurl {
      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_200001413.tar.bz2";
      hash = "sha256-VoTJu5+zuFFknDolGJ/69e1i6B4vfR9ev7sAKhfeRlU=";
    };
  });
}
