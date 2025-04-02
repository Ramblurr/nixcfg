final: prev: {
  roon-server = prev.roon-server.overrideAttrs (
    old:

    let
      version = "2.48.1517";
      urlVersion = builtins.replaceStrings [ "." ] [ "0" ] version;
    in
    {
      version = version;
      src = prev.fetchurl {
        url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_${urlVersion}.tar.bz2";
        hash = "sha256-2H8lQykhzbHcEW/+Rj+4eQdUMUugUeXivz+/+MEAYxk=";
      };
    }
  );
}
