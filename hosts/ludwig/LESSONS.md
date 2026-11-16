# ludwig cutover lessons (2026-11-16)

What went wrong when ludwig moved from TrueNAS CORE to NixOS with the
unattended installer, and what changed because of it. The operation ledger
is `.scratch-org/ocp/20261116T115539Z-ludwig-nixos-cutover.org` (local).
Read this before the next unattended install of any remote host.

The installer and its scripts (`installer.nix`, `install.sh`,
`truenas-stage.sh`, `flash-usb.sh`, `ludwig-boot.sh`) were removed after the
cutover; they are in git history up to commit `54db80cc`.

## Tailscale

1. Reused node state from another OS did not log in. The FreeBSD jail's
   `tailscaled.state` loaded on Linux and the control server accepted the
   node (`machineAuthorized=true`), but tailscaled then reported
   `active login: <missing-profile>` and stayed in `NeedsLogin`. The offline
   test before the cutover only checked that the state loaded, not that it
   logged in.
   Fixed: no more state reuse. The installer and the host each log in with
   the staged auth key under their own names; staging requires the key.

2. The auth-key fallback could not run on top of that state.
   `tailscaled-autoconnect` runs `tailscale up --auth-key`, which refuses
   while older prefs (the jail's `--accept-routes`, exit node) are set:
   "requires mentioning all non-default flags ... re-run with --reset".
   Fixed: `services.tailscale.extraUpFlags = [ "--reset" ]` in
   `tailscale-watchdog.nix`; `extraSetFlags` reapplies our flags afterwards.

3. Even `tailscale up --reset --auth-key` hung in `NeedsLogin` while the
   stale state was present: it re-registered the existing node key. Only
   removing the state forced a fresh login.
   Lesson: never seed node state you have not seen log in on the target OS.

4. A node advertising its own LAN cannot serve its own address. The new
   host advertised `192.168.8.0/22` and became primary over plato. Tailnet
   traffic for `192.168.8.4` then arrived on `tailscale0`, but replies left
   through the site router without SNAT and were lost, so the host's LAN
   address was unreachable from home. The jail never hit this because it
   was a separate host.
   Fixed: ludwig no longer advertises the route (`tailscaleFlags` in
   `site.nix`); plato is the only subnet router.

5. The new node's MagicDNS name was `ludwig-1`: `ludwig` was already taken.
   Fixed: renamed to `ludwig` in the admin console.

5a. Tailscale SSH was off: the jail never ran it, and the other hosts have
    it as runtime state rather than config, so nothing carried it over.
    `ssh root@ludwig` fell through to sshd and the YubiKey.
    Fixed: `--ssh` in ludwig's `extraSetFlags`. Also drop any saved sshd
    host key for the tailnet IP from `known_hosts`.

## Installer

6. `install.sh` seed step listed `/mnt/persist/etc /mnt/persist/var` with
   `find`; without Tailscale state there was no `var`, `find` failed under
   `pipefail`, and the install exited 1 right after seeding.
   Fixed: the listing covers only `/mnt/persist/etc`.

7. The rehearsals missed 1 to 4 and 6. The QEMU fixture never logged in to
   the tailnet, and the installer's own tailscaled always created a
   (logged-out) state file, so `/mnt/persist/var` always existed.
   Lesson: rehearse with a real test auth key and the same staged inputs as
   production, including what is absent.

8. impermanence's activation inside `nixos-install` prints
   `chown: invalid user` for every persisted directory: the chroot has no
   accounts until userborn is seeded. Ownership is correct after boot.
   Open, cosmetic.

## Operating it

9. The resumed install ran under `systemd-run --collect`. When it failed,
   the unit was garbage-collected, the watcher saw `inactive`, and nobody
   noticed until Casey looked at the console.
   Lesson: no `--collect`; treat any state other than active as a failure
   unless the log's last line says the install finished.

10. SSH through plato stalled after `SSH2_MSG_KEXINIT`: the path MTU from
    quine to the remote LAN is 1280 and the post-quantum key exchange
    packets are larger. `-o KexAlgorithms=curve25519-sha256
    -o HostKeyAlgorithms=ssh-ed25519` works.
    Fixed: both subnet routers, debord and plato, now clamp the MSS of TCP
    forwarded into Tailscale, so it no longer depends on ICMP getting
    through.

11. FreeBSD `efibootmgr -c` creates entries inactive and first in
    BootOrder. Create with `-a`, then put the default back first.

12. T3's browser refuses the BMC's self-signed certificate, so only a human
    could see the KVM console (screenshots worked).
    Open.

13. The RTC ran about two minutes fast under TrueNAS; the journal jumped
    back when NTP first synced. Fixed by itself: NixOS keeps the RTC in
    sync once the clock is synchronized.

14. `nixos-rebuild switch` does not rerun the oneshot `tailscaled-set`, so
    changed `extraSetFlags` only apply at the next boot. After a deploy that
    changes them, run `systemctl start tailscaled-set`.
