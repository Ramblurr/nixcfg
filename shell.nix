{
  system ? "x86_64-linux",
  pkgs ? import <nixpkgs> {inherit system;},
}: let
  packages = with pkgs; [
    python311
    python311Packages.passlib
    python311Packages.ruamel-yaml
    python311Packages.pyyaml
    python311Packages.kubernetes
    python311Packages.simple-term-menu
  ];
in
  pkgs.mkShell {
    buildInputs = packages;
    shellHook = ''
      export SHELL=${pkgs.zsh}
    '';
  }
