{ config, ... }:
let
  printerAddress = "10.8.60.64";
in
{
  modules.services.goqlprinter = {
    enable = true;
    listenAddress = builtins.head config.site.net.prim.hosts4.${config.networking.hostName};
    networkPrinter = {
      address = "${printerAddress}:9100";
      model = "QL-810W";
    };
  };

  services.avahi = {
    enable = true;
    openFirewall = false;
    settings = {
      server = {
        allow-interfaces = [ "prim" ];
        use-ipv6 = false;
      };
      publish.disable-publishing = false;
    };
    extraServiceFiles.brother-label-printer = ''
      <?xml version="1.0" standalone="no"?>
      <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
      <service-group>
        <name>Brother QL-810W</name>
        <service protocol="ipv4">
          <type>_ipp._tcp</type>
          <subtype>_universal._sub._ipp._tcp</subtype>
          <host-name>brother-label-printer.local</host-name>
          <port>631</port>
          <txt-record>txtvers=1</txt-record>
          <txt-record>qtotal=1</txt-record>
          <txt-record>rp=ipp/print</txt-record>
          <txt-record>ty=Brother QL-810W</txt-record>
          <txt-record>product=(Brother QL-810W)</txt-record>
          <txt-record>pdl=application/octet-stream,image/urf</txt-record>
          <txt-record>URF=V1.4,W8,CP1,IS1,MT1-5,OB10,PQ4-5,RS300,FN3-11-60-63</txt-record>
          <txt-record>Color=F</txt-record>
          <txt-record>Duplex=F</txt-record>
          <txt-record>kind=label,roll</txt-record>
        </service>
      </service-group>
    '';
  };

  environment.etc."avahi/hosts".text = ''
    ${printerAddress} brother-label-printer.local
  '';

  networking.firewall.interfaces.prim = {
    allowedTCPPorts = [ config.modules.services.goqlprinter.port ];
    allowedUDPPorts = [ 5353 ];
  };
}
