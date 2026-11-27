let
  port = 8443;
in
{
  inherit port;
  certificate = ./onepassword-connect.crt;
  keyFile = ./onepassword-connect-tls.sops.yaml;
  url = address: "https://${address}:${toString port}";
  tailnetUrl = tailnet: "https://op-connect.${tailnet}";
  trustDirectory =
    pkgs: certificate:
    pkgs.linkFarm "onepassword-connect-trust" [
      {
        name = "onepassword-connect.pem";
        path = certificate;
      }
    ];
}
