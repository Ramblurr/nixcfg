{ inputs, ... }:
{
  imports = [
    inputs.devshell.flakeModule
    inputs.pre-commit-hooks.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    {
      config,
      pkgs,
      ...
    }:
    {
      #pre-commit.settings.hooks.treefmt.enable = true;
      treefmt = {
        projectRootFile = "flake.nix";
        programs = {
          nixfmt.enable = pkgs.lib.meta.availableOn pkgs.stdenv.buildPlatform pkgs.nixfmt-rfc-style.compiler;
          nixfmt.package = pkgs.nixfmt-rfc-style;
          deadnix.enable = true;
          statix.enable = true;
        };
      };

      devshells.default = {
        packages = [
          pkgs.nix # Always use the nix version from this flake's nixpkgs version, so that nix-plugins (below) doesn't fail because of different nix versions.
          pkgs.ssh-to-age
          pkgs.age-plugin-fido2-hmac
          pkgs.age-plugin-yubikey
          pkgs.age-plugin-tpm
          pkgs.age-plugin-ledger
          pkgs.vim
          pkgs.sops
        ];

        commands = [
          {
            package = config.treefmt.build.wrapper;
            help = "Format all files";
          }
          {
            package = pkgs.deploy;
            help = "Build and deploy this nix config to nodes";
          }
          {
            package = pkgs.nix-tree;
            help = "Interactively browse dependency graphs of Nix derivations";
          }
          {
            package = pkgs.nvd;
            help = "Diff two nix toplevels and show which packages were upgraded";
          }
          {
            package = pkgs.nix-diff;
            help = "Explain why two Nix derivations differ";
          }
          {
            package = pkgs.nix-output-monitor;
            help = "Nix Output Monitor (a drop-in alternative for `nix` which shows a build graph)";
          }
          {
            package = pkgs.writeShellApplication {
              name = "build";
              text = ''
                set -euo pipefail
                [[ "$#" -ge 1 ]] \
                  || { echo "usage: build <HOST>..." >&2; exit 1; }
                HOSTS=()
                for h in "$@"; do
                  HOSTS+=(".#nixosConfigurations.$h.config.system.build.toplevel")
                done
                nom build --no-link --print-out-paths --show-trace "''${HOSTS[@]}"
              '';
            };
            help = "Build a host configuration";
          }
          {
            package = pkgs.writeShellApplication {
              name = "why-depends";
              text = ''
                set -euo pipefail

                [[ "$#" -ne 3 ]] \
                  || { echo "usage: why-depends HOST PKG" >&2; exit 1; }
                HOST=''${1:-}
                PKG=''${2:-}
                nix why-depends ".#nixosConfigurations.$HOST.config.system.build.toplevel" ".#nixosConfigurations.$HOST.pkgs.$PKG" --derivation | cat
              '';
            };
            help = "Explain why a NixOS host's configuration depends on some package";
          }
          {
            package = pkgs.writeShellApplication {
              name = "generate-host";
              text = ''
                set -euo pipefail
                python ./scripts/gen-host.py
              '';
            };
            help = "Scaffold a new host config";
          }
        ];

        #devshell.startup.pre-commit.text = config.pre-commit.installationScript;
      };
    };
}
