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
      nix-comment-lint = pkgs.writeShellApplication {
        name = "nix-comment-lint";
        runtimeInputs = [
          pkgs.ast-grep
          pkgs.babashka
          pkgs.git
        ];
        text = ''
          exec bb ${./nix-comment-lint.clj} "$@"
        '';
      };

      forbidden-strings = pkgs.writeShellApplication {
        name = "forbidden-strings";
        runtimeInputs = [
          pkgs.coreutils
          pkgs.gawk
          pkgs.git
          pkgs.gnugrep
          pkgs.ripgrep
        ];
        text = ''
          set -euo pipefail

          mode="working"
          if [[ "''${1:-}" == "--staged" ]]; then
            mode="staged"
          elif [[ "''${1:-}" == "-h" || "''${1:-}" == "--help" ]]; then
            cat <<'EOF'
          Usage: forbidden-strings [--staged]

          Check files case-insensitively against untracked forbidden-string patterns in
          <git-common-dir>/info/forbidden-strings (shared across worktrees).
          EOF
            exit 0
          elif [[ "$#" -gt 0 ]]; then
            echo "error: unknown argument: $1" >&2
            exit 2
          fi

          root="$(git rev-parse --show-toplevel)"
          common_dir="$(git rev-parse --git-common-dir)"
          case "$common_dir" in
            /*) ;;
            *) common_dir="$root/$common_dir" ;;
          esac

          patterns_src="$common_dir/info/forbidden-strings"
          if [[ ! -f "$patterns_src" ]]; then
            exit 0
          fi

          patterns_file="$(mktemp)"
          trap 'rm -f "$patterns_file"' EXIT
          grep -Ev '^[[:space:]]*(#|$)' "$patterns_src" > "$patterns_file" || true
          if [[ ! -s "$patterns_file" ]]; then
            exit 0
          fi

          cd "$root"

          if [[ "$mode" == "staged" ]]; then
            mapfile -d "" -t files < <(
              git diff --cached --name-only --diff-filter=ACM -z
            )
            [[ "''${#files[@]}" -eq 0 ]] && exit 0
            matches="$(git grep --cached -I -i -n -F -f "$patterns_file" -- "''${files[@]}" || true)"
          else
            mapfile -d "" -t files < <(
              git ls-files -z --cached -- ':!pkgs/ts-egress/vendor/*'
            )
            existing=()
            for f in "''${files[@]}"; do
              [[ -f "$f" ]] && existing+=("$f")
            done
            [[ "''${#existing[@]}" -eq 0 ]] && exit 0
            matches="$(rg --no-heading --line-number --ignore-case --fixed-strings --file "$patterns_file" -- "''${existing[@]}" || true)"
          fi

          if [[ -n "$matches" ]]; then
            printf '%s\n' "$matches" | awk -F: '{ print $1 ":" $2 ": forbidden string matched" }'
            exit 1
          fi
        '';
      };

      lint = pkgs.writeShellApplication {
        name = "lint";
        runtimeInputs = [
          forbidden-strings
          nix-comment-lint
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

          echo "==> nix-comment-lint"
          if ! nix-comment-lint; then
            status=1
          fi

          echo "==> forbidden-strings"
          if ! forbidden-strings; then
            status=1
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

      packages = {
        inherit forbidden-strings lint nix-comment-lint;
      };

      devshells.default = {
        packages = [
          inputs.determinate.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix
          pkgs.gitleaks
          pkgs.prek
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
            help = "Run statix, deadnix, nixfmt, and new-code comment checks without modifying files";
          }
          {
            package = nix-comment-lint;
            help = "Report Nix comments in new code, staged changes, all files, or explicit paths";
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
