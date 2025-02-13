_inputs: _final: prev: {
  lib = prev.lib // {
    my = prev.lib.my // {
      /*
        cidrToIp :: string -> string

        Given an IP address in CIDR notation, returns the IP address without the CIDR suffix
      */
      cidrToIp = ip: builtins.head (builtins.split "/" ip);

      /*
        generateMacAddress :: string -> string

        Given a string, returns a MAC address.
      */
      generateMacAddress =
        s:
        let
          hash = builtins.hashString "sha256" s;
          c = off: builtins.substring off 2 hash;
        in
        "${builtins.substring 0 1 hash}2:${c 2}:${c 4}:${c 6}:${c 8}:${c 10}";

      /*
        eui64Suffix :: string -> string

        Given a MAC address, returns the EUI-64 suffix. The returned suffix can be appended to a prefix to create a valid ipv6 ip address.
      */
      eui64Suffix =
        mac:
        let
          inherit (prev.lib.std)
            list
            regex
            string
            num
            ;
          inherit (prev.lib.my) uint;
          parts = list.map string.toLower (regex.splitOn ":" mac);
          part = list.unsafeIndex parts;
          part0 =
            part:
            let
              nibble1' = uint.fromHexDigit (string.unsafeIndex part 1);
              nibble1 = num.bits.bitXor 2 nibble1';
              nibble0 = string.unsafeIndex part 0;
            in
            nibble0 + uint.toHexLower nibble1;
        in
        "${part0 (part 0)}${part 1}:${part 2}ff:fe${part 3}:${part 4}${part 5}";
    };
  };
}
