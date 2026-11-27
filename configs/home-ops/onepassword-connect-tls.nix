let
  port = 8443;
  certificate = ./onepassword-connect.crt;
  trustDirectory =
    pkgs: certificate:
    pkgs.linkFarm "onepassword-connect-trust" [
      {
        name = "onepassword-connect.pem";
        path = certificate;
      }
    ];
  trustEnvironment =
    pkgs: certificate:
    let
      directory = trustDirectory pkgs certificate;
    in
    {
      SSL_CERT_FILE = "${directory}/onepassword-connect.pem";
      SSL_CERT_DIR = "${directory}";
    };
in
{
  inherit
    port
    certificate
    trustDirectory
    trustEnvironment
    ;
  keyFile = ./onepassword-connect-tls.sops.yaml;
  url = address: "https://${address}:${toString port}";
  tailnetUrl = tailnet: "https://op-connect.${tailnet}";
  clientExports =
    pkgs: url:
    pkgs.lib.optionalString (builtins.match "https://[0-9.]+(:[0-9]+)?/?" url != null) (
      pkgs.lib.concatLines (
        pkgs.lib.mapAttrsToList (name: value: "export ${name}=${pkgs.lib.escapeShellArg value}") (
          trustEnvironment pkgs certificate
        )
      )
    );
}
