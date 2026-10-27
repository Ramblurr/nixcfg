{
  pkgs,
  inputs,
  ...
}:
{
  environment.systemPackages = [ inputs.smolvm.packages.${pkgs.stdenv.hostPlatform.system}.default ];
}
