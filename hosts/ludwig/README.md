# ludwig - offsite replica NAS

ludwig is an iXsystems TrueNAS Mini at a remote site (hardware in
`hosts/inventory.nix`). It pulls mali's zrepl source, serves SMB and NFS to
the remote LAN, runs AdGuard Home for that LAN, is the target of plato's
borg backups, and is the remote site's Tailscale exit node. No subnet route joins the remote LAN to
the home sites; reach it over Tailscale node addresses.

It ran TrueNAS CORE 13.1 until 2026-11-16. Site-specific data (accounts,
shares, exports, the mali address) lives in
`nixcfg-private/hosts/ludwig/secrets/local.nix`.

## Layout

[`site.nix`](./site.nix) holds the values the host's modules share: LAN
addresses, the interface name, the hostid, the root pool, the dataset and ESP
layout, and where the migrated state lives.

| Item | Where |
| --- | --- |
| LAN | `networking.nix`; any other cabled port also takes the LAN address |
| AdGuard Home | `adguardhome.nix`; web UI on loopback only |
| BMC | DHCP reservation on the LAN's router |
| `tank` | raidz2 of HDDs, ZFS native encryption; imported under the former TrueNAS altroot, see `hardware-configuration.nix` |
| `fast` | SSD mirror, ZFS native encryption; root pool |
| NixOS root | the `modules.boot.zfs` dataset layout on the root pool |
| ESPs | partition 1 of each SSD, GRUB mirrored |
| SATA DOM | 16 GB, wiped and unused (the former TrueNAS boot disk) |

Pool keys are as TrueNAS made them (`keyformat=hex`, `keylocation=prompt`)
and are backed up in 1Password. NixOS loads them with explicit
`zfs load-key -L file://...` from the key directory in `site.nix`:

- The root pool's key is embedded in the initrd, so it is plaintext on both
  ESPs. TrueNAS kept the same key on its boot device, so unattended boot
  keeps the protection level it had.
- `tank`'s key is loaded after `zfs-import-tank`.

## Remote access

ludwig is only reachable over Tailscale, two independent ways:

1. Its own node, `ludwig`, with Tailscale SSH like the other hosts:
   `ssh root@ludwig` and `deploy ludwig` go through it. `tailscale-watchdog`
   restarts `tailscaled` while it is not connected.
2. Through plato, which sits on the same LAN: `ssh -J plato` to ludwig's LAN
   address works even if ludwig's own node is down, as long as plato is on.

There is no subnet route to the LAN. One existed until 2026-11-18; Tailscale
then carried its own traffic to ludwig inside it, WireGuard inside WireGuard,
and transfers ran at about 15 KB/s. ludwig must not advertise the LAN either;
see `tailscaleFlags` in `site.nix`.

If both ways fail, the BMC's web UI has a KVM console and power control; reach
it with an SSH tunnel through plato (`ssh -L 8443:<BMC address>:443 plato`). For a non-technical helper on site, the only
physical instruction is: unplug ludwig's power for ten seconds and plug it
back in.

After a deploy that changes `services.tailscale.extraSetFlags`, run
`systemctl start tailscaled-set`; a switch does not rerun it.

## Clients

- plato mounts both NFS exports and writes its nightly borg backup to the
  `plato` account's `borg_repo` over SSH.
- SMB clients know the server by its TrueNAS name; see `samba.nix`.

## Migration history

ludwig was migrated in place on 2026-11-16 with an unattended installer
stick; the pools were imported unchanged. [LESSONS.md](./LESSONS.md) lists
what went wrong and how it was fixed. The installer, its scripts and the
runbook were removed afterwards; they are in git history up to commit
`54db80cc`.

