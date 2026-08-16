{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  doomConfig = "/checkout/configs/doom";
  evaluated = lib.evalModules {
    specialArgs = { inherit inputs pkgs; };
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
  homeFiles = homeConfiguration.config.home.file;
  localIssuesPackage = lib.findFirst (
    package: lib.getName package == "local-issues"
  ) null homeConfiguration.config.home.packages;
  launcher = "${localIssuesPackage}/bin/local-issues";
  isolatedPath = lib.makeBinPath [
    pkgs.coreutils
    pkgs.gnused
  ];
in
assert localIssuesPackage != null;
assert !(homeFiles ? ".config/emacs/bin/local-issues");
assert !(homeFiles ? ".config/emacs/lisp/local-issues-core.el");
assert lib.all (
  path: !(lib.hasInfix ".config/emacs/bin" path)
) homeConfiguration.config.home.sessionPath;
pkgs.runCommand "local-issues-tests" { nativeBuildInputs = [ pkgs.emacs ]; } ''
    test -x ${launcher}
    test "$(basename ${launcher})" = local-issues

    isolatedRoot="$TMPDIR/isolated"
    mkdir -p "$isolatedRoot/.scratch-org/001-test/issues"
    cat > "$isolatedRoot/.scratch-org/001-test/issues/01-packaged.org" <<'EOF'
  * READY-FOR-AGENT Packaged launcher
  :PROPERTIES:
  :TICKET_ID: 001-01
  :BLOCKED_BY:
  :ASSIGNEE:
  :END:
  EOF
    isolatedOutput=$(
      EMACS_SOCKET_NAME="local-issues-absent-$$" \
        PATH=${isolatedPath} \
        ${launcher} --root "$isolatedRoot" list
    )
    case "$isolatedOutput" in
      *"Packaged launcher"*) ;;
      *) exit 1 ;;
    esac

    sourceRoot="$TMPDIR/source"
    mkdir -p "$sourceRoot/configs/doom/lisp" "$sourceRoot/tests"
    cp ${../configs/doom/lisp/local-issues-core.el} "$sourceRoot/configs/doom/lisp/local-issues-core.el"
    cp ${../configs/doom/+local-issues.el} "$sourceRoot/configs/doom/+local-issues.el"
    cp ${./local-issues-test.el} "$sourceRoot/tests/local-issues-test.el"
    cp ${./project-scratch-test.el} "$sourceRoot/tests/project-scratch-test.el"
    ${pkgs.emacs}/bin/emacs --batch -Q \
      -l "$sourceRoot/tests/project-scratch-test.el" \
      -f ert-run-tests-batch-and-exit

    LOCAL_ISSUES_LAUNCHER=${launcher} \
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
    LOCAL_ISSUES_LAUNCHER=${launcher} \
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
