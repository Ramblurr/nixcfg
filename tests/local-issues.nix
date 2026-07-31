{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  doomConfig = "/checkout/configs/doom";
  evaluated = lib.evalModules {
    modules = [
      ../modules/editors/emacs/default.nix
      (
        { lib, ... }:
        {
          options = {
            modules.users.primaryUser.homeDirectory = lib.mkOption { type = lib.types.str; };
            modules.dev.llms.enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            fonts.packages = lib.mkOption {
              type = lib.types.listOf lib.types.package;
              default = [ ];
            };
            environment.wordlist.enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            myhm = lib.mkOption { type = lib.types.raw; };
          };
          config = {
            modules.users.primaryUser.homeDirectory = "/home/tester";
            modules.editors.emacs = {
              enable = true;
              package = pkgs.emacs;
              localDoomConfigRepo = doomConfig;
            };
          };
        }
      )
    ];
  };
  homeConfiguration = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      inputs.sops-nix.homeManagerModules.sops
      evaluated.config.myhm
      {
        home = {
          username = "tester";
          homeDirectory = "/home/tester";
          stateVersion = "26.05";
        };
        sops.age.keyFile = "/tmp/test-age-key";
      }
    ];
  };
  launcher = homeConfiguration.config.home.file.".config/emacs/bin/local-issues";
  core = homeConfiguration.config.home.file.".config/emacs/lisp/local-issues-core.el";
in
assert launcher.target == ".config/emacs/bin/local-issues";
assert core.target == ".config/emacs/lisp/local-issues-core.el";
assert lib.all (
  path: !(lib.hasInfix ".config/emacs/bin" path)
) homeConfiguration.config.home.sessionPath;
pkgs.runCommand "local-issues-tests" { nativeBuildInputs = [ pkgs.emacs ]; } ''
    test "$(readlink ${launcher.source})" = "${doomConfig}/bin/local-issues"
    test "$(readlink ${core.source})" = "${doomConfig}/lisp/local-issues-core.el"
    sourceRoot="$TMPDIR/source"
    mkdir -p "$sourceRoot/configs/doom/bin" "$sourceRoot/configs/doom/lisp" "$sourceRoot/tests"
    cp ${../configs/doom/bin/local-issues} "$sourceRoot/configs/doom/bin/local-issues"
    cp ${../configs/doom/lisp/local-issues-core.el} "$sourceRoot/configs/doom/lisp/local-issues-core.el"
    cp ${./local-issues-test.el} "$sourceRoot/tests/local-issues-test.el"
    chmod +x "$sourceRoot/configs/doom/bin/local-issues"

    LOCAL_ISSUES_LAUNCHER="$sourceRoot/configs/doom/bin/local-issues" \
      ${pkgs.emacs}/bin/emacs --batch -Q \
        -l "$sourceRoot/tests/local-issues-test.el" \
        -f ert-run-tests-batch-and-exit

    socket="local-issues-matrix-$$"
    ${pkgs.emacs}/bin/emacs -Q --daemon="$socket"
    cleanup_server() {
      EMACS_SOCKET_NAME="$socket" \
        ${pkgs.emacs}/bin/emacsclient --alternate-editor=false --timeout=2 \
          --suppress-output --eval '(kill-emacs)' >/dev/null 2>&1 || true
    }
    trap cleanup_server EXIT
    EMACS_SOCKET_NAME="$socket" \
      ${pkgs.emacs}/bin/emacsclient --alternate-editor=false --timeout=2 \
        --suppress-output --eval \
        "(load \"$sourceRoot/configs/doom/lisp/local-issues-core.el\" nil t)"

    daemonBin="$TMPDIR/daemon-bin"
    mkdir -p "$daemonBin"
    cat > "$daemonBin/emacs" <<'EOF'
  #!/bin/sh
  for argument do
    [ "$argument" = --batch ] && exit 97
  done
  exec @emacs@ "$@"
  EOF
    substituteInPlace "$daemonBin/emacs" \
      --replace-fail @emacs@ ${pkgs.emacs}/bin/emacs
    chmod +x "$daemonBin/emacs"

    LOCAL_ISSUES_LAUNCHER="$sourceRoot/configs/doom/bin/local-issues" \
      LOCAL_ISSUES_TEST_SOCKET="$socket" \
      LOCAL_ISSUES_TEST_BATCH_EMACS=${pkgs.emacs}/bin/emacs \
      PATH="$daemonBin:$PATH" \
      ${pkgs.emacs}/bin/emacs --batch -Q \
        -l "$sourceRoot/tests/local-issues-test.el" \
        -f ert-run-tests-batch-and-exit
    cleanup_server
    trap - EXIT
    touch "$out"
''
