{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.browsers.chromium;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.browsers.chromium = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/chromium"
          ".cache/chromium"
        ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, ... }@hm:
      {
        programs.chromium = {
          enable = true;
          commandLineArgs = mkIf config.modules.desktop.wayland.enable [
            "--enable-features=UseOzonePlatform"
            "--ozone-platform=wayland"
          ];
        };

        xdg.configFile =
          let
            flags = ''
              --enable-features=UseOzonePlatform
              --ozone-platform=wayland
            '';
          in
          mkIf config.modules.desktop.wayland.enable {
            "chromium-flags.conf".text = flags;
            "electron-flags.conf".text = flags;
            "electron-flags16.conf".text = flags;
            "electron-flags17.conf".text = flags;
            "electron-flags18.conf".text = flags;
            "electron-flags19.conf".text = flags;
            "electron-flags20.conf".text = flags;
            "electron-flags21.conf".text = flags;
            "electron-flags22.conf".text = flags;
            "electron-flags23.conf".text = flags;
            "electron-flags24.conf".text = flags;
            "electron-flags25.conf".text = flags;
            "electron-flags26.conf".text = flags;
            "electron-flags27.conf".text = flags;
            "electron-flags28.conf".text = flags;
          };
        # Chromium's PWA/SSB "installed" web apps don't open because the wrong path to chromium is used.
        # This fixes it to whatever is currently set in the nix profile.
        home.packages = mkIf config.modules.desktop.wayland.enable [
          (pkgs.writeShellScriptBin "fix-chromium-pwa" ''
            for file in /home/${username}/.local/share/applications/chrome-*-Default.desktop; do
              if [ -f "$file" ]; then
                tail -n +2 "$file" > tmpfile && echo '#!/run/current-system/sw/bin/xdg-open' | cat - tmpfile > "$file"; rm -f tmpfile
                sed -i 's|Exec=/nix/store/.*chromium-unwrapped.*libexec/chromium/chromium|Exec=/etc/profiles/per-user/${username}/bin/chromium|g' "$file"
              fi
            done
          '')
        ];
      };
  };
}
