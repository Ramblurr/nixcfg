{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluate =
    extraModules:
    (inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        ../modules/dev/llms/pi-nix.nix
        {
          home.username = "test";
          home.homeDirectory = "/home/test";
          home.stateVersion = "26.05";
          programs.pi = {
            enable = true;
            package = pkgs.hello;
            instances = {
              pi = {
                settings = {
                  theme = lib.mkDefault "dark";
                  packages = [
                    (toString pkgs.hello)
                    {
                      source = "npm:example";
                      extensions = [ ];
                    }
                  ];
                };
                models.providers.example.apiKey = "!op read op://vault/item/key";
                fff.mode = "override";
              };
              pi-t3 = {
                configDir = "/home/test/custom-pi/agent";
                settings.skills = [ "/home/test/skills/t3" ];
              };
              disabled.enable = false;
            };
          };
        }
      ]
      ++ extraModules;
    }).config;
  config = evaluate [ { programs.pi.instances.pi.settings.theme = "light"; } ];
  disabled = evaluate [ { programs.pi.enable = lib.mkForce false; } ];
  configOnly = evaluate [ { programs.pi.package = lib.mkForce null; } ];
  files = config.home.file;
  readJson = path: builtins.fromJSON (builtins.unsafeDiscardStringContext files.${path}.text);
  targets = map (file: file.target) (lib.attrValues files);
in
assert
  readJson "/home/test/.config/pi/agent/settings.json" == {
    theme = "light";
    packages = [
      (toString pkgs.hello)
      {
        source = "npm:example";
        extensions = [ ];
      }
    ];
  };
assert
  readJson "/home/test/custom-pi/agent/settings.json" == {
    skills = [ "/home/test/skills/t3" ];
  };
assert
  readJson "/home/test/.config/pi/agent/models.json" == {
    providers.example.apiKey = "!op read op://vault/item/key";
  };
assert readJson "/home/test/.config/pi/agent/pi-fff.json" == { mode = "override"; };
assert !(files ? "/home/test/custom-pi/agent/models.json");
assert !(files ? "/home/test/custom-pi/agent/pi-fff.json");
assert !(files ? "/home/test/.config/disabled/agent/settings.json");
assert lib.elem ".config/pi/agent/settings.json" targets;
assert lib.elem "custom-pi/agent/settings.json" targets;
assert lib.elem pkgs.hello config.home.packages;
assert !(lib.elem pkgs.hello configOnly.home.packages);
assert !(disabled.home.file ? "/home/test/.config/pi/agent/settings.json");
assert !(lib.elem pkgs.hello disabled.home.packages);
pkgs.runCommand "pi-instances-test" { } ''
  touch "$out"
''
