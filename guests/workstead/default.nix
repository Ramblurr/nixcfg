{
  imports = [
    (import ../moot-stead.nix {
      vcpu = 4;
      mem = 12288;
      zvolSize = 131072;
    })
  ];
}
