{
  system ? "x86_64-linux",
  pkgs ? import <nixpkgs> { inherit system; },
}:
let
  packages = with pkgs; [
    python3
    python3Packages.passlib
    python3Packages.ruamel-yaml
    python3Packages.pyyaml
    python3Packages.simple-term-menu
  ];
in
pkgs.mkShell {
  buildInputs = packages;
  shellHook = ''
    export SHELL=${pkgs.zsh}
  '';
}
