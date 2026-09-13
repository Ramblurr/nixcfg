{ pkgs }:
let
  get-mute = pkgs.writeShellApplication {
    name = "speaker-get-mute";
    runtimeInputs = [
      pkgs.alsa-utils
      pkgs.gnugrep
    ];
    text = ''
      state=$(amixer -D pipewire get Master)
      if grep -Eq 'Playback.*\[off\]' <<< "$state"; then
        echo 1
      elif grep -Eq 'Playback.*\[on\]' <<< "$state"; then
        echo 0
      else
        echo 'Cannot read PipeWire Master mute state' >&2
        exit 1
      fi
    '';
  };
  mute = pkgs.writeShellApplication {
    name = "speaker-mute";
    runtimeInputs = [ pkgs.alsa-utils ];
    text = "exec amixer -D pipewire set Master mute";
  };
  unmute = pkgs.writeShellApplication {
    name = "speaker-unmute";
    runtimeInputs = [ pkgs.alsa-utils ];
    text = "exec amixer -D pipewire set Master unmute";
  };
  toggle = pkgs.writeShellApplication {
    name = "speaker-toggle";
    runtimeInputs = [
      get-mute
      mute
      unmute
    ];
    text = ''
      state=$(speaker-get-mute)
      if [[ "$state" == 1 ]]; then
        exec speaker-unmute
      else
        exec speaker-mute
      fi
    '';
  };
in
[
  mute
  unmute
  get-mute
  toggle
]
