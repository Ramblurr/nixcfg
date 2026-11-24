{
  imports = [
    (import ../moot-stead.nix {
      vcpu = 4;
      mem = 8192;
      zvolSize = 131072;
    })
  ];
}
