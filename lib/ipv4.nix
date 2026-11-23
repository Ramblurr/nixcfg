{ lib }:
let
  octet = "(25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])";
  toInt = address: lib.foldl' (acc: o: acc * 256 + lib.toInt o) 0 (lib.splitString "." address);
  blockSize = prefixLength: lib.foldl' (acc: _: acc * 2) 1 (lib.range 1 (32 - prefixLength));
in
rec {
  type = lib.types.strMatching "${octet}(\\.${octet}){3}";

  subnet =
    address: prefixLength:
    let
      n = toInt address;
      first = n - lib.mod n (blockSize prefixLength);
    in
    {
      inherit first;
      last = first + blockSize prefixLength - 1;
    };

  fromCidr =
    cidr:
    let
      parts = lib.splitString "/" cidr;
    in
    subnet (lib.head parts) (lib.toInt (lib.last parts));

  overlaps = a: b: a.first <= b.last && b.first <= a.last;

  # /31 and /32 have no network or broadcast address.
  isLink =
    a: b: prefixLength:
    let
      net = subnet a prefixLength;
      usable =
        address:
        prefixLength >= 31
        || !lib.elem (toInt address) [
          net.first
          net.last
        ];
    in
    a != b && subnet b prefixLength == net && usable a && usable b;
}
