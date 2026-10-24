# TODO

- [ ] debord: 10GbE/sonnet is not working
- [ ] debord: zfs replication to mali is not working, because of 10GbE not working

### Temporary package fixes and pins

- [ ] **nix-output-monitor:** remove the compatibility patch once nixpkgs includes
  [upstream PR #321](https://github.com/maralorn/nix-output-monitor/pull/321).
  Source: [`overlays/last-known-good.nix:6`](overlays/last-known-good.nix#L6).
  Introduced: [`9e07ae6a5a33`](https://github.com/ramblurr/nixcfg/commit/9e07ae6a5a33c6300f7f9e707aad606dd5932160).
- [ ] **pyopenjtalk-plus:** remove the Cython error-text compatibility patch after
  [nixpkgs #576063](https://github.com/NixOS/nixpkgs/pull/576063) reaches our pin.
  Source: [`overlays/last-known-good.nix:17`](overlays/last-known-good.nix#L17).
  Introduced: [`680b6044d1bd`](https://github.com/ramblurr/nixcfg/commit/680b6044d1bd02812f82119310c9a62bf9b2796f).
  Upstream reference added: [`d5c1afdd9b80`](https://github.com/ramblurr/nixcfg/commit/d5c1afdd9b807276b66998f82459bb0d75a3fc5f).
- [ ] **Synapse:** remove the upstream integer-JWT-subject compatibility backport
  once [nixpkgs #575253](https://github.com/NixOS/nixpkgs/pull/575253) reaches our pin.
  The fix preserves integer-subject compatibility and the original regression test.
  References: [nixpkgs #575220](https://github.com/NixOS/nixpkgs/issues/575220),
  [Synapse #20330](https://github.com/element-hq/synapse/pull/20330).
  Source: [`overlays/last-known-good.nix:29`](overlays/last-known-good.nix#L29).
  Corrected in: [`afb040f1ed2b`](https://github.com/ramblurr/nixcfg/commit/afb040f1ed2ba052edbe6896dc67d9b3cfcf98bd).
  Supersedes the incorrect test-only workaround introduced in
  [`de8ed68a7bb9`](https://github.com/ramblurr/nixcfg/commit/de8ed68a7bb9973cc2555223523dc08f754b05c7).
- [ ] **Open WebUI:** remove the wrapper-argument override after
  [nixpkgs #575801](https://github.com/NixOS/nixpkgs/pull/575801) reaches our pin.
  Source: [`overlays/last-known-good.nix:41`](overlays/last-known-good.nix#L41).
  Introduced: [`09927baecd66`](https://github.com/ramblurr/nixcfg/commit/09927baecd66a072069ea14b74ac51fdb60c5301).
- [ ] **MuseScore:** remove the Qt 6.12 unity-build patch after
  [nixpkgs #576411](https://github.com/NixOS/nixpkgs/pull/576411) reaches our pin.
  Source: [`overlays/last-known-good.nix:50`](overlays/last-known-good.nix#L50).
  Introduced: [`aca480d88506`](https://github.com/ramblurr/nixcfg/commit/aca480d88506a12916786db9a5233e103e6a8fa2).
- [ ] **Quine CUDA/ONNX:** revisit `cudaKnownGood` and the host overrides only after
  [nixpkgs #575979](https://github.com/NixOS/nixpkgs/pull/575979) is included and
  replacement CUDA/ONNX outputs **and their full closures** are verified in Flox.
  Related root-cause fix: [patchelf #672](https://github.com/NixOS/patchelf/pull/672).
  Sources: [`overlays/last-known-good.nix:62`](overlays/last-known-good.nix#L62),
  [`flake/hosts.nix:40`](flake/hosts.nix#L40).
  Introduced: [`24b7ea59dceb`](https://github.com/ramblurr/nixcfg/commit/24b7ea59dcebac60231aaf8a2ea898d65c81814c).
- [ ] **NVIDIA:** remove the builder patch that splits RPATH/interpreter edits
  after [nixpkgs #576397](https://github.com/NixOS/nixpkgs/pull/576397) reaches our pin.
  Source: [`modules-unstable/hardware/nvidia/default.nix:118`](modules-unstable/hardware/nvidia/default.nix#L118).
  Introduced: [`96d5d7cc59b9`](https://github.com/ramblurr/nixcfg/commit/96d5d7cc59b93b820a0e3b357efdaac3fd655cef).
- [ ] **OBS/v4l2loopback:** remove the module-build override once nixpkgs includes
  [#575861](https://github.com/NixOS/nixpkgs/pull/575861).
  Source: [`modules/desktop/programs/obs.nix:19`](modules/desktop/programs/obs.nix#L19).
  Introduced: [`64a4f1a94d64`](https://github.com/ramblurr/nixcfg/commit/64a4f1a94d6452defc793ed0ea8a69aec878ee80).
- [ ] **Audacity:** restore the package after
  [nixpkgs #572942](https://github.com/NixOS/nixpkgs/issues/572942) is resolved.
  Source: [`modules/desktop/random-apps.nix:34`](modules/desktop/random-apps.nix#L34).
  Introduced: [`0c741d65cc99`](https://github.com/ramblurr/nixcfg/commit/0c741d65cc9911393e84976c048faa85cc239cb5).
- [ ] **Jet:** remove the official-binary override when nixpkgs Jet works with
  GraalVM 25.2.4.
  Source: [`pkgs/default.nix:23`](pkgs/default.nix#L23).
  Introduced: [`5841f5d95538`](https://github.com/ramblurr/nixcfg/commit/5841f5d95538230707dba74899972fc0e3875de6).

### Host configuration

- [ ] **Addams firewall:** revisit the duplicate `table inet firewall` workaround
  and the closing comment on
  [nixos-nftables-firewall #21](https://github.com/thelegy/nixos-nftables-firewall/issues/21).
  Source: [`hosts/addams/modules/firewall/default.nix:61`](hosts/addams/modules/firewall/default.nix#L61).
  Introduced: [`14b51608957d`](https://github.com/ramblurr/nixcfg/commit/14b51608957da868ec510370b82268ae43e3eaba).
- [ ] **Debord Grafana:** define the provisioned alert-rule groups; the TODO sits
  in the empty `rules.settings.groups` list.
  Source: [`hosts/debord/grafana/default.nix:154`](hosts/debord/grafana/default.nix#L154).
  Introduced: [`3b3d138abcf6`](https://github.com/ramblurr/nixcfg/commit/3b3d138abcf6f2e20e163cf0fa0cbafe56d34722).
- [ ] **Mali Beets:** revisit the beetcamp packaging problem using the linked
  [reference configuration](https://github.com/9999years/dotfiles/blob/main/config/npingler/pkgs/beets-with-beetcamp.nix).
  Source: [`hosts/mali/beets.nix:36`](hosts/mali/beets.nix#L36).
  Introduced: [`22fdef7f25a6`](https://github.com/ramblurr/nixcfg/commit/22fdef7f25a6770553fb97375205501953356677).
- [ ] **Octoprint/Octolapse:** revisit the missing-`distutils` failure noted for
  NixOS 24.11 and Python 3.12; see
  [Octolapse #957](https://github.com/FormerLurker/Octolapse/issues/957).
  Source: [`hosts/octoprint/plugins.nix:142`](hosts/octoprint/plugins.nix#L142).
  Introduced: [`a01427b59f61`](https://github.com/ramblurr/nixcfg/commit/a01427b59f611989d6f7e979d5d1ff2bc04788c7).
- [ ] **Quine ARM:** remove the temporary debug configuration when the ripping
  configuration is final (`LOGLEVEL = "DEBUG"` follows the TODO).
  Source: [`hosts/quine/arm.nix:101`](hosts/quine/arm.nix#L101).
  Introduced: [`d3f62c0ed797`](https://github.com/ramblurr/nixcfg/commit/d3f62c0ed797722ac614a825cc0749e9e8460df6).
- [ ] **Quine gaming:** investigate the Xbox controller driver choice: test with
  a regular controller and decide whether a fork is needed.
  Source: [`hosts/quine/gaming.nix:114`](hosts/quine/gaming.nix#L114).
  Introduced: [`81fa657e9639`](https://github.com/ramblurr/nixcfg/commit/81fa657e96395ca8b7ff68ab878e81e92b725f20).
- [ ] **Quine network-namespace experiment:** consolidate the repeated
  `ip netns exec` calls in the script.
  Source: [`hosts/quine/testing2.nix:165`](hosts/quine/testing2.nix#L165).
  Introduced: [`8acb01f8a5b0`](https://github.com/ramblurr/nixcfg/commit/8acb01f8a5b0da99cd91e1a02c3cdf3ff06dcfbe).

### Desktop, security, and shell

- [ ] **NVIDIA desktop integration:** move desktop-specific GPU-priority setup
  toward session startup scripts; the comment calls for upstream coordination.
  Source: [`modules-unstable/hardware/nvidia/default.nix:34`](modules-unstable/hardware/nvidia/default.nix#L34).
  Introduced: [`65fe9c0e0d15`](https://github.com/ramblurr/nixcfg/commit/65fe9c0e0d1584139914cb4e33972422c332242a).
- [ ] **VA-API browsers:** review the Firefox/Floorp settings TODO. Firefox
  settings now follow the comment, so determine what remains before changing it.
  Source: [`modules-unstable/hardware/nvidia/vaapi.nix:117`](modules-unstable/hardware/nvidia/vaapi.nix#L117).
  Introduced: [`0ed04983f171`](https://github.com/ramblurr/nixcfg/commit/0ed04983f171db77c9ef8e4e41399f68eb544c72).
- [ ] **OBS plugins:** determine whether the plugin block is needed or the
  functionality is already built in.
  Source: [`modules/desktop/programs/obs.nix:51`](modules/desktop/programs/obs.nix#L51).
  Introduced: [`dfe1e4aa9bf9`](https://github.com/ramblurr/nixcfg/commit/dfe1e4aa9bf9e723eb6749efec6b7417f809c24c).
- [ ] **Yubico SSH askpass:** replace the custom environment wrapper with
  `makeWrapper`.
  Source: [`modules/desktop/programs/yubico.nix:49`](modules/desktop/programs/yubico.nix#L49).
  Introduced: [`6f79c949f701`](https://github.com/ramblurr/nixcfg/commit/6f79c949f701c87f5c17cc8c46387776f1a3855b).
- [ ] **Systemd security settings:** migrate the commented `DefaultLimitNOFILE`
  setting from `systemd.extraConfig` to `systemd.settings.Manager`.
  Source: [`modules/security/default.nix:21`](modules/security/default.nix#L21).
  Introduced: [`b996585c91be`](https://github.com/ramblurr/nixcfg/commit/b996585c91beca58fef363c5e1f0c93bc25b60f0).
- [ ] **Zsh/FZF, NixOS 2026.11:** resolve all four renamed-option TODOs:
  `changeDirWidget.options`, `fileWidget.command`, `fileWidget.options`, and
  `historyWidget.command`.
  Source: [`modules/shell/zsh/default.nix:47–65`](modules/shell/zsh/default.nix#L47-L65).
  Introduced: [`c831ecf6786f`](https://github.com/ramblurr/nixcfg/commit/c831ecf6786fb19e9cdcbd0b7d9b003a74ed1fcc).

### MicroVMs, networking, and services

- [ ] **MicroVM guest DNS:** make fallback DNS user-configurable instead of
  selecting the first network's gateway.
  Source: [`modules/microvm-guest/common.nix:107`](modules/microvm-guest/common.nix#L107).
  Introduced: [`e627d5c8ea19`](https://github.com/ramblurr/nixcfg/commit/e627d5c8ea19bbb9628a7e953c2f0e4b61c7fb4d).
- [ ] **MicroVM host:** configure `microvm.autostart`; the current reminder is
  a commented empty list.
  Source: [`modules/microvm-host/default.nix:37`](modules/microvm-host/default.nix#L37).
  Introduced: [`1701da550733`](https://github.com/ramblurr/nixcfg/commit/1701da550733ba1a639b0fa4ec56a96959ab48e7).
- [ ] **Systemd network namespaces:** add a host-side macvlan shim so the host
  can reach the namespace.
  Source: [`modules/networking/systemd-netns.nix:139`](modules/networking/systemd-netns.nix#L139).
  Introduced: [`17d6936ee10f`](https://github.com/ramblurr/nixcfg/commit/17d6936ee10fe0e145d43abf6e21b6a278bf57ec).
- [ ] **Matrix IRC bridge:** support Synapse and Heisenbridge on separate
  machines rather than relying on the local registration file.
  Source: [`modules/services/matrix-irc.nix:30`](modules/services/matrix-irc.nix#L30).
  Introduced: [`4b6a2daff1c5`](https://github.com/ramblurr/nixcfg/commit/4b6a2daff1c56c3df52a7d846406cca1e1091dae).
- [ ] **Rclone:** manage the SSH key used by the mount configuration.
  Source: [`modules/services/rclone.nix:124`](modules/services/rclone.nix#L124).
  Introduced: [`681fb655b49f`](https://github.com/ramblurr/nixcfg/commit/681fb655b49f1e8c5a86b4cb91350d6ee98fa26d).
