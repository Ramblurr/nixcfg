{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks = {
        hindsight = import ../tests/hindsight.nix {
          inherit inputs pkgs;
        };
        linux-voice-assistant = import ../tests/linux-voice-assistant.nix {
          inherit inputs pkgs;
        };
      };
    };
}
