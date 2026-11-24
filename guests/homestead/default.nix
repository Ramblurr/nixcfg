{
  imports = [
    (import ../moot-stead.nix {
      vcpu = 2;
      mem = 6144;
      zvolSize = 65536;
    })
  ];
}
