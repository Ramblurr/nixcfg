{
  imports = [
    (import ../moot-builder.nix {
      vcpu = 4;
      mem = 12288;
      zvolSize = 204800;
    })
  ];
}
