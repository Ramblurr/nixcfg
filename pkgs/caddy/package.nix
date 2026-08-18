{
  buildPkgs,
  caddy,
}:
(caddy.withPlugins.override {
  inherit (buildPkgs) go xcaddy;
})
  {
    plugins = [
      "github.com/caddy-dns/desec@v1.1.0"
      "github.com/greenpau/caddy-security@v1.1.64"
    ];
    hash = "sha256-LNSaxNXR+LzxRhGAhNTcX1eQ5XhbaVYlrQZ9XnLjsks=";
  }
