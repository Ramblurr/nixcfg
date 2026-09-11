{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  registry = import ../config/microvm-ssh.nix;
  host = (inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    specialArgs = { inherit inputs; };
    modules = [
      inputs.microvm.nixosModules.host
      inputs.impermanence.nixosModules.impermanence
      ../modules/microvm-host/ssh-access.nix
      ({ lib, ... }: {
        options.modules.microvm-host.enable = lib.mkEnableOption "MicroVM host test";
        config = {
          modules.microvm-host.enable = true;
          networking.hostName = "dewey";
          system.stateVersion = "26.05";
        };
      })
    ];
  }).config;
  guest = name: (inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.microvm.nixosModules.microvm
      ../modules/microvm-guest/options.nix
      ../modules/microvm-guest/ssh-access.nix
      {
        networking.hostName = name;
        modules.microvm-guest = { enable = true; host = "dewey"; };
        system.stateVersion = "26.05";
      }
    ];
  }).config;
  registered = builtins.attrNames registry.dewey.guests;
  checkGuest = name:
    let cfg = guest name;
    in cfg.microvm.vsock.cid == registry.dewey.guests.${name}.cid
      && cfg.microvm.vsock.ssh.enable
      && !(cfg.environment.etc ? "ssh/microvm-host.pub")
      && !(cfg.systemd.services."sshd-vsock@".serviceConfig ? ExecStart)
      && builtins.elem registry.dewey.publicKeyFile cfg.users.users.root.openssh.authorizedKeys.keyFiles;
  sshConfig = pkgs.writeText "microvm-ssh-config" host.environment.etc."ssh/ssh_config".text;
  helper = builtins.head (builtins.filter (p: (p.name or "") == "microvm-verified-ssh") host.environment.systemPackages);
in
assert lib.assertMsg (lib.all checkGuest registered) "Both guests must use standard root authorized keys without a custom VSOCK override";
assert lib.assertMsg ((guest "unregistered").users.users.root.openssh.authorizedKeys.keyFiles == [ ]) "Unregistered guests must not acquire host access";
assert lib.assertMsg (lib.all (name: host.programs.ssh.knownHosts."microvm-${name}".publicKeyFile == registry.dewey.guests.${name}.hostKeyFile) registered) "Guest host identities must be pinned";
pkgs.runCommand "microvm-ssh-check" {
  nativeBuildInputs = [ pkgs.openssh pkgs.python3 pkgs.bash ];
} ''
  bash -n ${helper}/bin/microvm
  if grep -Fq 'StrictHostKeyChecking=no' ${helper}/bin/microvm; then
    echo 'microvm helper still bypasses host-key verification' >&2
    exit 1
  fi
  # The Nix sandbox may map dependency ownership to nobody. ssh rejects such
  # included config files, so give the unchanged upstream include test ownership.
  cp ${host.systemd.package}/lib/systemd/ssh_config.d/20-systemd-ssh-proxy.conf proxy.conf
  cp ${sshConfig} ssh.conf
  chmod u+w ssh.conf
  substituteInPlace ssh.conf --replace-fail \
    '${host.systemd.package}/lib/systemd/ssh_config.d/20-systemd-ssh-proxy.conf' "$PWD/proxy.conf"
  ssh -F "$PWD/ssh.conf" -G vsock/4244 > qemu.config
  ssh -F "$PWD/ssh.conf" -G vsock-mux//var/lib/microvms/immich-home/notify.vsock > cloud.config
  python3 - <<'PY'
  from pathlib import Path
  for filename, name in [('qemu.config', 'opencloud-home'), ('cloud.config', 'immich-home')]:
      fields = dict(line.split(' ', 1) for line in Path(filename).read_text().splitlines())
      assert fields['hostkeyalias'] == f'microvm-{name}'
      assert fields['stricthostkeychecking'] == 'true'
      assert fields['identityagent'] == 'none'
      assert fields['identitiesonly'] == 'yes'
      assert fields['identityfile'] == '/var/lib/microvm-ssh/id_ed25519'
      assert fields['batchmode'] == 'yes'
      assert 'systemd-ssh-proxy' in fields['proxycommand']
  PY
  touch "$out"
''
