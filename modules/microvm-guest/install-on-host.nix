{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  upstream =
    (import "${inputs.microvm}/nixos-modules/microvm/ssh-deploy.nix" { inherit config lib pkgs; })
    .config.microvm.deploy.installOnHost;
  installer =
    pkgs.runCommand "microvm-install-on-host" { meta.mainProgram = "microvm-install-on-host"; }
      ''
        mkdir -p "$out/bin"
        cp ${upstream}/bin/microvm-install-on-host "$out/bin/microvm-install-on-host"
        chmod u+w "$out/bin/microvm-install-on-host"
        substituteInPlace "$out/bin/microvm-install-on-host" --replace-fail \
          '[[ $(realpath ./current) != $(realpath ./new) ]]' \
          '[[ \$(realpath ./current) != \$(realpath ./new) ]]' \
          --replace-fail ' is already installed"' ' is already installed"; rm -f new'
      '';
in
{
  config = lib.mkIf config.modules.microvm-guest.enable {
    microvm.deploy.installOnHost = lib.mkForce installer;
  };
}
