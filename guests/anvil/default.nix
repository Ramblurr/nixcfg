{
  imports = [
    (import ../moot-builder.nix {
      vcpu = 6;
      mem = 24576;
      zvolSize = 409600;
    })
  ];
}
