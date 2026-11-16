# ludwig - offsite replica NAS

ludwig is an iXsystems TrueNAS Mini at a remote site (hardware in
`hosts/inventory.nix`). It pulls mali's zrepl source, serves SMB and NFS to
the remote LAN, runs AdGuard Home for that LAN, and is the remote LAN's
Tailscale subnet router and exit node.

It ran TrueNAS CORE 13.1 until the migration described below. Site-specific
data (accounts, shares, exports, the mali address) lives in
`nixcfg-private/hosts/ludwig/secrets/local.nix`.

## Layout

[`site.nix`](./site.nix) holds the values the host, the installer and the
scripts share: LAN addresses, the interface name, the hostid, the root pool,
the ESP labels, the TrueNAS ESP, and the paths the installer seeds.

| Item | Where |
| --- | --- |
| LAN | `networking.nix`; any other cabled port also takes the LAN address |
| AdGuard Home | `adguardhome.nix`; web UI on loopback only |
| BMC | DHCP on the LAN |
| `tank` | raidz2 of HDDs, ZFS native encryption; imported under the TrueNAS altroot, see `hardware-configuration.nix` |
| `fast` | SSD mirror, ZFS native encryption; root pool for NixOS, old jail datasets unused |
| NixOS root | the `modules.boot.zfs` dataset layout on the root pool; `hardware-configuration.nix` |
| ESPs | partition 1 of each SSD (former TrueNAS swap), GRUB mirrored |
| TrueNAS boot | SATA DOM, untouched |
| hostid | the TrueNAS hostid, so either OS imports the pools without `-f` |

Pool keys stay as TrueNAS made them (`keyformat=hex`, `keylocation=prompt`).
NixOS loads them with explicit `zfs load-key -L file://...` from the
persistent key directory in `site.nix`:

- The root pool's key is embedded in the initrd, so it is plaintext on both
  ESPs. TrueNAS kept the same key on its boot device, so unattended boot
  keeps the existing protection level.
- `tank`'s key is loaded after `zfs-import-tank`.

Neither NixOS nor the installer changes any pool property. TrueNAS finds both
pools exactly as it left them, so a fallback boot starts its jails, including
the `tailscale-ludwig` jail. Never run `zpool upgrade` while TrueNAS remains a
rollback option.

## Remote access

ludwig is only reachable over Tailscale, two independent ways:

1. Its own node: the `tailscale-ludwig` identity (`100.79.17.127`), reused by
   the installer and the installed host from the jail's state. If that state
   is ever rejected, a staged fallback auth key logs in instead, and
   `tailscale-watchdog` restarts `tailscaled` while it is not connected.
2. plato's subnet route for the LAN: SSH to the LAN address works even if
   ludwig's own node is down, as long as plato is on. The installer and the
   host take that address on whichever port has a cable.

Both run with `--accept-routes=false`: plato advertises ludwig's own LAN, and
accepting that route would send LAN traffic through Tailscale.

The commands below use one destination, through plato's route:

```sh
export LUDWIG_SSH=root@$(nix eval --raw --file ~/nixcfg/hosts/ludwig/site.nix lan.address)
```

An extra root key for unattended administration, `~/.ssh/id_ed25519_ludwig_agent`
on quine, is authorized on TrueNAS root (through the middleware), the
installer and the NixOS host; its public half is in the private `local.nix`.
Use it without the ssh config and control socket:

```sh
ssh -F /dev/null -i ~/.ssh/id_ed25519_ludwig_agent -o IdentitiesOnly=yes \
  -o HostKeyAlias=ludwig.int.<home domain> "$LUDWIG_SSH"
```

The installer generates a fresh host key on every boot, so connect to it
with `-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null` instead.
Remove the key everywhere once the migration is done.

For a non-technical helper on site, the only physical instruction is: unplug
ludwig's power for ten seconds and plug it back in. Until NixOS is made the
default boot entry, that always brings TrueNAS back.

## Migration runbook

Every step that writes to ludwig is marked HUMAN. The install itself runs
unattended from the installer stick; every step it takes can also be run by
hand with `ludwig-install <step>`.

### 1. Prepare on quine

1. `hosts/ludwig/secrets.sops.yaml` exists (created 2026-11-15). Its age
   recipient derives from the TrueNAS ed25519 host key, which NixOS keeps.

   | Key | Source |
   | --- | --- |
   | `root-password`, `ramblurr-password` | same hashes as `hosts/quine/secrets.sops.yaml` |
   | `ssh_host_ed25519_key_pub` | the TrueNAS ed25519 host key |
   | `zrepl/ludwigCert`, `zrepl/ludwigKey`, `zrepl/maliCert` | `ludwig.crt`, `ludwig.key`, `mali.crt` from the old zrepl jail |
   | `upsmonPassword` | random; only upsd and upsmon on ludwig use it |

2. Commit and update the `nixcfg` input in `nixcfg-private`.

3. Back up both pool keys to 1Password (read-only on TrueNAS). Each command
   puts one key on the clipboard for a single paste, without printing it;
   paste it into the pool's 1Password item before running the next:

   ```sh
   ssh "$LUDWIG_SSH" "midclt call -job pool.dataset.export_key fast" | tr -d '"\n' | wl-copy --paste-once
   ssh "$LUDWIG_SSH" "midclt call -job pool.dataset.export_key tank" | tr -d '"\n' | wl-copy --paste-once
   ```

4. Optional but recommended: create a fallback Tailscale auth key (tagged
   with the jail's tag, pre-approved, reusable, 90 days) and store it on TrueNAS as
   `/root/ludwig-ts-authkey`. Apply `nixcfg-private/tailscale/policy.hujson`,
   whose `autoApprovers` approve the remote LAN route for that tag.

### 2. Write the installer stick (from quine)

`flash-usb.sh` builds the installer from `nixcfg-private`, uploads it to
TrueNAS (resumable), finds the one USB disk that is in no pool and not
mounted, writes it and verifies the stick against the ISO. Rerun it after any
change to the installer or the ludwig system. The installer carries the
complete ludwig system, so nothing crosses the remote link during the install.

```sh
cd ~/nixcfg
hosts/ludwig/flash-usb.sh --dry-run   # build and check the target only
hosts/ludwig/flash-usb.sh             # asks for the disk name, then writes
```

Then create a one-time boot entry for the stick on TrueNAS:

```sh
gpart show da0          # the EFI partition, usually da0s2
mkdir -p /tmp/usbesp && mount_msdosfs /dev/da0s2 /tmp/usbesp
efibootmgr -c -l /tmp/usbesp/EFI/BOOT/BOOTX64.EFI -L ludwig-installer
umount /tmp/usbesp
efibootmgr -v           # note the new entry NNNN and the TrueNAS entry TTTT
efibootmgr -o TTTT,...  # put TrueNAS first again if -c moved it
```

### 3. Stage the bundle (HUMAN, on TrueNAS)

`truenas-stage.sh` copies everything the install needs onto the TrueNAS ESP,
plaintext, until the install wipes it. The secrets never leave ludwig and are
not printed. The script lists what it stages.

```sh
scp ~/nixcfg/hosts/ludwig/truenas-stage.sh "$LUDWIG_SSH:/tmp/"
ssh "$LUDWIG_SSH" sh /tmp/truenas-stage.sh --check   # read-only
ssh "$LUDWIG_SSH" sh /tmp/truenas-stage.sh --auto    # stage and arm
```

Without `--auto` the installer boots, joins Tailscale and waits for you to
run the steps by hand. `--wipe` removes a staged bundle.

The installer and the final system reuse the jail's Tailscale node state:
same tailnet address, tag and approved subnet route, so the remote LAN and
the BMC stay reachable throughout.

### 4. Cut over (HUMAN)

On TrueNAS:

```sh
tmux kill-session -t zrepl
efibootmgr -n -b NNNN   # boot the installer once
shutdown -r now
```

The installer then runs on its own:

1. It joins Tailscale with the jail state and waits for the grace period in
   `installer.nix`. To abort, SSH to the reused node (or `$LUDWIG_SSH`
   through plato) and run `systemctl stop ludwig-autoinstall`.
2. `ludwig-install auto` runs every step in `install.sh`. Each step stops at
   the first unexpected state; the installer then stays up on Tailscale with
   TrueNAS still the default boot entry. Follow along with
   `journalctl -fu ludwig-autoinstall`.
3. On success it overwrites and deletes the staged bundle, saves its log to
   `/persist/var/log/ludwig-install.log`, and reboots into NixOS once.

If the installer is unreachable both ways, a power cycle by someone on site
boots TrueNAS again.

### 5. Verify the first NixOS boot

```sh
ssh "$LUDWIG_SSH"
systemctl --failed
zpool status -x; zfs get -H keystatus fast tank
findmnt -t zfs          # tank under the TrueNAS altroot, the NixOS datasets
tailscale status | head -3
journalctl -b -u samba-import-truenas-passdb   # one-time SMB password import
pdbedit -L
exportfs -v
zrepl status            # confirm pull_mali replication resumes
upsc ups@localhost ups.status
ipmitool mc watchdog get; wdctl
```

AdGuard Home, from quine through plato's route:

```sh
dig +short @"$(nix eval --raw --file ~/nixcfg/hosts/ludwig/site.nix lan.dns)" example.com
```

- The NFS client must remount both exports: FreeBSD file handles are stale.
- Check that one file which had `user` extended attributes on TrueNAS
  (`lsextattr user <file>`) still shows them under `getfattr -d`.
- Confirm SMB login for each share user from a remote-site client.

When everything passes, make NixOS the default in the same session:

```sh
ludwig-boot nixos-default   # NixOS, NixOS fallback ESP, then TrueNAS
ludwig-boot status
```

`ludwig-boot` finds the entries by label and partition, not by number.
Deploys do not change the boot order afterwards.

Until then, any power cut boots TrueNAS, which works as before.

### Rollback to TrueNAS

Before NixOS is the default: power-cycle ludwig. Afterwards, from NixOS:
`ludwig-boot truenas-once && reboot` for one boot, or `ludwig-boot
truenas-default` to make TrueNAS the default again.

TrueNAS imports the pools under its altroot as before. Its swap mirror is
degraded because the SSD partitions became ESPs; that is harmless. Data
written under NixOS and the NixOS datasets stay in place.

### Cleanup once TrueNAS is retired

- If an install failed, the staged bundle may still be on the TrueNAS ESP;
  wipe it from TrueNAS with `truenas-stage.sh --wipe`.
- Destroy `tank/.system` and, after salvaging anything wanted, `fast/iocage`.
- `mali.key` sits in the old zrepl jail directory; ludwig never needs it.
- Remove the TrueNAS boot entry (`ludwig-boot status` shows it); consider
  `zpool upgrade`.
