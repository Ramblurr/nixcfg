{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  blankRulesetMessage = "networking.nftables.ruleset must not contain only whitespace; use networking.nftables.tables for managed tables";
  evaluate =
    extraModule:
    inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        ../modules/firewall/default.nix
        ({ lib, ... }: {
          options = {
            modules.vpn.mullvad.enable = lib.mkEnableOption "Mullvad";
            modules.vpn.tailscale.enable = lib.mkEnableOption "Tailscale";
          };
          config = {
            boot.loader.grub.devices = [ "nodev" ];
            fileSystems."/" = {
              device = "none";
              fsType = "tmpfs";
            };
            modules.firewall.enable = true;
            system.stateVersion = "26.05";
          };
        })
        extraModule
      ];
    };
  defaultCfg = (evaluate { }).config;
  mullvadCfg =
    (evaluate {
      modules.vpn.mullvad.enable = true;
      modules.vpn.tailscale.enable = true;
    }).config;
  blankCfg =
    (evaluate {
      networking.nftables.ruleset = " \n\t";
    }).config;
  failedAssertions =
    cfg: map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
  mullvadRules = builtins.elemAt mullvadCfg.systemd.services.nftables.serviceConfig.ExecStart 1;
in
assert defaultCfg.networking.nftables.ruleset == "";
assert !defaultCfg.networking.nftables.flushRuleset;
assert !(defaultCfg.networking.nftables.tables ? "mullvad-local-exclude");
assert !(defaultCfg.networking.nftables.tables ? "mullvad-tailscale-exclude");
assert mullvadCfg.networking.nftables.tables ? "mullvad-local-exclude";
assert mullvadCfg.networking.nftables.tables ? "mullvad-tailscale-exclude";
assert lib.hasInfix "chain exclude-local-lan"
  mullvadCfg.networking.nftables.tables."mullvad-local-exclude".content;
assert lib.hasInfix "chain exclude-dns"
  mullvadCfg.networking.nftables.tables."mullvad-tailscale-exclude".content;
assert builtins.elem blankRulesetMessage (failedAssertions blankCfg);
pkgs.runCommand "firewall-evaluation" { inherit mullvadRules; } "touch $out"
