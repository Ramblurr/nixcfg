{
  pkgs,
  ...
}:
{
  services.udpbroadcastrelay = {
    enable = true;
    instances = {
      roon = {
        port = 9003;
        id = 1;
        interfaces = [
          "vlprim4"
          "lan0"
          "vliot50"
        ];
      };
      mdns = {
        port = 5353;
        id = 2;
        interfaces = [
          "vlprim4"
          "lan0"
          "vliot50"
          "vlnot60"
        ];
        multicast = "224.0.0.251";
      };
      syncthing = {
        port = 21027;
        id = 3;
        interfaces = [
          "vlprim4"
          "lan0"
        ];
      };
    };

  };
}
