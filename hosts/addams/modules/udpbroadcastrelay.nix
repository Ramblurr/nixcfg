{
  pkgs,
  ...
}:
{
  services.udpbroadcastrelay = {
    enable = false;
    instances = {
      roon = {
        port = 9003;
        id = 1;
        interfaces = [
          "me-prim"
          "lan0"
          "me-iot"
        ];
      };
      mdns = {
        port = 5353;
        id = 2;
        interfaces = [
          "me-prim"
          "lan0"
          "me-iot"
          "me-not"
        ];
        multicast = "224.0.0.251";
      };
      syncthing = {
        port = 21027;
        id = 3;
        interfaces = [
          "me-prim"
          "lan0"
        ];
      };
    };

  };
}
