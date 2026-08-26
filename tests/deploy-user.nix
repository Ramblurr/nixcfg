{ inputs, pkgs }:
let
  homeDirectory = "/var/lib/deploy-test";
  username = "deploy-test";
  cfg =
    (inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        inputs.home-manager.nixosModules.home-manager
        ../modules/users/deploy-user.nix
        (
          { lib, ... }:
          {
            options.modules.zfs.datasets.properties = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          }
        )
        {
          system.stateVersion = "26.05";
          modules.users.deploy-users.test = {
            inherit homeDirectory username;
            uid = 3001;
            gid = 3001;
            homeDirectoryOnZfs = {
              enable = false;
              datasetName = "unused";
            };
            homeManager.enable = false;
          };
        }
      ];
    }).config;
  rules = cfg.systemd.tmpfiles.rules;
  relevantRules = builtins.filter (rule: pkgs.lib.hasPrefix "d ${homeDirectory}" rule) rules;
  tmpfilesConfig = pkgs.writeText "deploy-user-tmpfiles.conf" (
    pkgs.lib.concatStringsSep "\n" relevantRules
  );
in
assert builtins.elem "d ${homeDirectory}/.config/systemd 0750 ${username} ${username} -" rules;
assert builtins.elem "d ${homeDirectory}/.config/systemd/user 0750 ${username} ${username}" rules;
pkgs.runCommand "deploy-user-test" { nativeBuildInputs = [ pkgs.systemd ]; } ''
  root="$TMPDIR/root"
  systemd_dir="$root${homeDirectory}/.config/systemd"
  uid="$(id -u)"
  gid="$(id -g)"

  mkdir -p "$root/etc" "$systemd_dir"
  chmod 0555 "$systemd_dir"
  printf '${username}:x:%s:%s::${homeDirectory}:/bin/sh\n' "$uid" "$gid" > "$root/etc/passwd"
  printf '${username}:x:%s:\n' "$gid" > "$root/etc/group"

  systemd-tmpfiles --root="$root" --create ${tmpfilesConfig}
  test "$(stat -c %a "$systemd_dir")" = 750

  rm -rf "$systemd_dir/user"
  mkdir "$systemd_dir/user"
  touch "$out"
''
