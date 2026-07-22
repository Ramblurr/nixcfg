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
        linux-voice-assistant-output-only = import ../tests/linux-voice-assistant-output-only.nix {
          inherit inputs pkgs;
        };
      };
    };
}
