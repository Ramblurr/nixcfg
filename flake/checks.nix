{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks.hindsight = import ../tests/hindsight.nix {
        inherit inputs pkgs;
      };
    };
}
