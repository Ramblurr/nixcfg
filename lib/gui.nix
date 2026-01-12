_inputs: _final: prev: {
  lib = prev.lib // {
    my = prev.lib.my // {
      autostart =
        command:
        let
          name = prev.lib.head (prev.lib.splitString " " command);
        in
        {
          text = ''
            [Desktop Entry]
            Name=${name}
            Exec=${command}
            Type=Application
            X-KDE-autostart-after=panel
            X-KDE-autostart-phase=2
            StartupNotify=false
            Terminal=false
          '';
        };
    };
  };
}
