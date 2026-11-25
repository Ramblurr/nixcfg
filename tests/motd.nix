{ inputs, pkgs }:
let
  inherit (pkgs) lib;

  # The real guest builder needs private site secrets, so stand in for the
  # microvm-guest module with its enable option only.
  microvmGuestStub = enable: {
    options.modules.microvm-guest.enable = lib.mkEnableOption "microvm guest stub";
    config.modules.microvm-guest.enable = enable;
  };

  stubUnits = {
    systemd.services.caddy.script = "true";
    systemd.services."getty-stub@".script = "true";
    systemd.timers.restic-backup.timerConfig.OnCalendar = "daily";
  };

  mkHost =
    {
      microvmGuest ? false,
      extraModules ? [ ],
      ...
    }@motdConfig:
    (inputs.nixpkgs.lib.nixosSystem {
      inherit (pkgs.stdenv.hostPlatform) system;
      modules = [
        ../modules/motd.nix
        (microvmGuestStub microvmGuest)
        stubUnits
        {
          nixpkgs.pkgs = pkgs;
          boot.loader.grub.enable = false;
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          system.stateVersion = "25.11";
          modules.motd = removeAttrs motdConfig [
            "microvmGuest"
            "extraModules"
          ];
        }
      ]
      ++ extraModules;
    }).config;

  # Two modules registering overlapping units, as self-registering service
  # modules do, plus a template instance.
  host = mkHost {
    enable = true;
    units = [
      "caddy.service"
      "restic-backup.timer"
    ];
    extraModules = [
      {
        modules.motd.units = [
          "caddy.service"
          "getty-stub@tty1.service"
        ];
      }
    ];
    disks = [
      "/"
      "/persist"
    ];
  };
  disabled = mkHost { };
  guest = mkHost {
    enable = true;
    microvmGuest = true;
  };
  missing = mkHost {
    enable = true;
    units = [
      "caddy.service"
      "nope.service"
      "nope@tty1.service"
      "caddy.timer"
    ];
  };
  # A disabled host may name units it does not define.
  missingDisabled = mkHost { units = [ "nope.service" ]; };

  failedAssertions = cfg: map (a: a.message) (lib.filter (a: !a.assertion) cfg.assertions);
  has =
    needle: haystack:
    lib.hasInfix (builtins.unsafeDiscardStringContext needle) (
      builtins.unsafeDiscardStringContext haystack
    );
  hasMotd = cfg: lib.any (p: (p.pname or "") == "motd") cfg.environment.systemPackages;
  hook = "${lib.getExe pkgs.motd} || true";
  guard = ''[ -n "''${SSH_CONNECTION:-}" ] && [ -t 1 ] && [ -z "''${MOTD_SHOWN:-}" ]'';
in
assert hasMotd host;
assert hasMotd guest;
assert !(hasMotd disabled);
assert has guard host.environment.loginShellInit;
assert has hook host.environment.loginShellInit;
assert has hook guest.environment.loginShellInit;
assert !(has "motd" disabled.environment.loginShellInit);
assert !(disabled.environment.etc ? "motd/config.edn");
assert !host.services.openssh.settings.PrintMotd;
assert failedAssertions host == [ ];
assert failedAssertions guest == [ ];
assert failedAssertions missingDisabled == [ ];
assert
  failedAssertions missing == [
    "modules.motd.units names units this system does not define: nope.service, nope@tty1.service, caddy.timer"
  ];
pkgs.runCommand "motd-module-test"
  {
    nativeBuildInputs = [ pkgs.babashka ];
    hostConfig = host.environment.etc."motd/config.edn".text;
    guestConfig = guest.environment.etc."motd/config.edn".text;
  }
  ''
    export HOME="$TMPDIR"
    bb --config /dev/null -e '
      (require (quote [clojure.edn :as edn]))
      (let [host  (edn/read-string (System/getenv "hostConfig"))
            guest (edn/read-string (System/getenv "guestConfig"))]
        (assert (= "host" (:role host)) (pr-str host))
        (assert (= ["/" "/persist"] (:disks host)) (pr-str host))
        (assert (= #{"caddy.service" "restic-backup.timer" "getty-stub@tty1.service"} (set (:units host))) (pr-str host))
        (assert (apply distinct? (:units host)) (pr-str host))
        (assert (= {:role "microvm" :units [] :disks nil} guest) (pr-str guest)))'
    touch "$out"
  ''
