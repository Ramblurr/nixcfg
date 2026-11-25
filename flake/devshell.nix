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
    let
      lint = pkgs.writeShellApplication {
        name = "lint";
        runtimeInputs = [
          pkgs.deadnix
          pkgs.git
          pkgs.nixfmt
          pkgs.statix
        ];
        text = ''
          set -euo pipefail

          root="$(git rev-parse --show-toplevel)"
          cd "$root"

          status=0

          echo "==> statix check"
          if ! statix check --config ${./statix.toml} .; then
            status=1
          fi

          mapfile -d "" -t nix_files < <(
            git ls-files -z --cached --others --exclude-standard -- '*.nix' ':!pkgs/ts-egress/vendor/*'
          )
          existing_nix_files=()
          for f in "''${nix_files[@]}"; do
            if [[ -f "$f" ]]; then
              existing_nix_files+=("$f")
            fi
          done

          if [[ "''${#existing_nix_files[@]}" -gt 0 ]]; then
            echo "==> deadnix"
            if ! deadnix --fail -- "''${existing_nix_files[@]}"; then
              status=1
            fi

            echo "==> nixfmt --check"
            if ! nixfmt --check -- "''${existing_nix_files[@]}"; then
              status=1
            fi
          fi

          exit "$status"
        '';
      };
    in
    {
      #pre-commit.settings.hooks.treefmt.enable = true;
      treefmt = {
        projectRootFile = "flake.nix";
        settings.global.excludes = [ "pkgs/ts-egress/vendor/**" ];
        programs = {
          nixfmt.enable = pkgs.lib.meta.availableOn pkgs.stdenv.buildPlatform pkgs.nixfmt.compiler;
          nixfmt.package = pkgs.nixfmt;
          deadnix.enable = true;
          statix = {
            enable = true;
            disabled-lints = [ "repeated_keys" ];
          };
        };
      };

      packages.lint = lint;

      devshells.default = {
        packages = [
          inputs.determinate.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix
          pkgs.ssh-to-age
          pkgs.age-plugin-yubikey
          pkgs.vim
          (pkgs.python312.withPackages (ps: [
            ps.passlib
            ps.ruamel-yaml
            ps.simple-term-menu
            ps.pyyaml
          ]))
          # (pkgs.sops.withAgePlugins (p: [
          #   p.age-plugin-fido2-hmac
          #   p.age-plugin-yubikey
          #   p.age-plugin-tpm
          # ]))
        ];

        commands = [
          {
            package = config.treefmt.build.wrapper;
            help = "Format all files";
          }
          {
            package = lint;
            help = "Run statix, deadnix, and nixfmt checks without modifying files";
          }
          {
            package = pkgs.deploy;
            help = "Build and deploy this nix config to nodes";
          }
          {
            package = pkgs.cache-push;
            help = "Build and push Nix closures to an Attic cache";
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
            package = pkgs.build;
            help = "Build host systems or MicroVM guest runners";
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
