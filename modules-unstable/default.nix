{ ... }:

{
  imports = [
    #./hardware/nvidia.nix
    ./hardware/nvidia
    ./services/microsocks.nix
  ];
}
