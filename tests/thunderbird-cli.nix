{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluate =
    profile:
    let
      outer = lib.evalModules {
        specialArgs = { inherit pkgs; };
        modules = [
          ../modules/desktop/programs/thunderbird-cli.nix
          {
            options = {
              myhm = lib.mkOption { type = lib.types.deferredModule; };
              assertions = lib.mkOption { type = lib.types.listOf lib.types.attrs; };
              modules.desktop.programs.thunderbird.enable = lib.mkOption {
                type = lib.types.bool;
                default = false;
              };
            };
            config.modules.desktop.programs.thunderbird-cli = {
              enable = true;
              flatpak.profile = profile;
            };
          }
        ];
      };
    in
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        outer.config.myhm
        {
          home.username = "test";
          home.homeDirectory = "/home/test";
          home.stateVersion = "26.05";
        }
      ];
    };
  native = (evaluate null).config;
  flatpak = (evaluate "test.default-release").config;
  activation = pkgs.writeShellScript "install-flatpak-bridge-test" ''
    set -eu
    run() { "$@"; }
    ${lib.replaceStrings [ "/home/test/" ] [ "\${TMPDIR}/home/" ]
      flatpak.home.activation.thunderbirdFlatpakBridge.data
    }
  '';
in
assert !(native.home.activation ? thunderbirdFlatpakBridge);
assert !flatpak.programs.thunderbird.enable;
assert flatpak.systemd.user.services.tb-bridge.Install.WantedBy == [ "graphical-session.target" ];
pkgs.runCommand "thunderbird-cli-flatpak-test" { } ''
  profile="$TMPDIR/home/.var/app/org.mozilla.thunderbird/.thunderbird/test.default-release"
  if ${activation}; then
    echo "Missing profiles must be rejected" >&2
    exit 1
  fi
  mkdir -p "$profile/extensions"
  printf 'unrelated add-on\n' > "$profile/extensions/other.xpi"
  printf 'user_pref("existing.setting", true);\n' > "$profile/user.js"
  ${activation}
  test -f "$profile/extensions/thunderbird-ai@extension.xpi"
  test ! -L "$profile/extensions/thunderbird-ai@extension.xpi"
  cmp "$profile/extensions/thunderbird-ai@extension.xpi" \
    ${pkgs.thunderbird-ai-bridge}/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/thunderbird-ai@extension.xpi
  grep -qxF 'unrelated add-on' "$profile/extensions/other.xpi"
  grep -qxF 'user_pref("existing.setting", true);' "$profile/user.js"
  grep -qxF 'user_pref("extensions.autoDisableScopes", 0);' "$profile/user.js"
  cp "$profile/user.js" "$TMPDIR/user.js.before"
  ${activation}
  cmp "$TMPDIR/user.js.before" "$profile/user.js"

  # A later user preference must not silently defeat the managed setting.
  printf 'user_pref("extensions.autoDisableScopes", 15);\n' >> "$profile/user.js"
  ${activation}
  test "$(tail -n 1 "$profile/user.js")" = 'user_pref("extensions.autoDisableScopes", 0);'

  mv "$profile/user.js" "$profile/user.js.original"
  ln -s "$profile/user.js.original" "$profile/user.js"
  if ${activation}; then
    echo "Symlinked user.js must be rejected" >&2
    exit 1
  fi
  touch "$out"
''
