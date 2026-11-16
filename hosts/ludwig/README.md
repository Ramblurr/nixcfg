# ludwig - offsite replica NAS

ludwig is an iXsystems TrueNAS Mini 3.0 X at a remote site. It pulls mali's
zrepl source, serves SMB and NFS to the remote LAN, runs AdGuard Home for that
LAN, and is the remote LAN's Tailscale subnet router and exit node.

It ran TrueNAS CORE 13.1 until the migration described below. Site-specific
data (accounts, shares, exports, the mali address) lives in
`nixcfg-private/hosts/ludwig/secrets/local.nix`.

## Layout

| Item | Value |
| --- | --- |
| LAN | `lan0` (X553 port 0, `pci-0000:04:00.0`), `192.168.8.4/22`, gateway `192.168.8.1`; any other cabled port also takes `192.168.8.4` |
| AdGuard Home | `192.168.8.2:53`; web UI on `127.0.0.1:3000` |
| BMC | DHCP, last seen at `192.168.10.144` |
| `tank` | raidz2, 5 x 12 TB HDD, ZFS native encryption; imported with altroot `/mnt`, so `/mnt/tank` as on TrueNAS |
| `fast` | mirror, 2 x Samsung 870 EVO 500 GB, ZFS native encryption; old jails mount under `/fast`, unused |
| NixOS root | `fast/encrypted/{local/root,local/nix,local/home,safe/persist}` |
| ESPs | partition 1 of each SSD (former TrueNAS swap), labels `ludwig-esp0`, `ludwig-esp1`, GRUB mirrored |
| TrueNAS boot | 16 GB SATA DOM, untouched; ESP partuuid `d4fb2736-d81c-11ec-bff8-d05099fb0f82` |
| hostid | `ac91e159`, the TrueNAS hostid, so either OS imports the pools without `-f` |

Pool keys stay as TrueNAS made them (`keyformat=hex`, `keylocation=prompt`).
NixOS loads them with explicit `zfs load-key -L file://...`:

- `fast`: from `/persist/etc/zfs-keys/fast.key`, embedded in the initrd. It is
  therefore plaintext on both ESPs. TrueNAS kept the same key on its boot
  device, so unattended boot keeps the existing protection level.
- `tank`: from `/persist/etc/zfs-keys/tank.key` after `zfs-import-tank`.

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
2. plato's subnet route for `192.168.8.0/22`: `ssh root@192.168.8.4` works
   even if ludwig's own node is down, as long as plato is on. The installer
   and the host take `192.168.8.4` on whichever port has a cable.

Both run with `--accept-routes=false`: plato advertises ludwig's own LAN, and
accepting that route would send LAN traffic through Tailscale.

An extra root key for unattended administration, `~/.ssh/id_ed25519_ludwig_agent`
on quine, is authorized on TrueNAS root (through the middleware), the
installer and the NixOS host; its public half is in the private `local.nix`.
Use it without the ssh config and control socket:

```sh
ssh -F /dev/null -i ~/.ssh/id_ed25519_ludwig_agent -o IdentitiesOnly=yes \
  -o HostKeyAlias=ludwig.int.<home domain> root@192.168.8.4
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
   | `ssh_host_ed25519_key_pub` | `/usr/local/etc/ssh/ssh_host_ed25519_key.pub` on TrueNAS |
   | `zrepl/ludwigCert`, `zrepl/ludwigKey`, `zrepl/maliCert` | `ludwig.crt`, `ludwig.key`, `mali.crt` in `/mnt/fast/iocage/jails/zrepl/root/usr/local/etc/zrepl/` |
   | `upsmonPassword` | random; only upsd and upsmon on ludwig use it |

2. Commit and update the `nixcfg` input in `nixcfg-private`.

3. Back up both pool keys to 1Password (read-only on TrueNAS):

   ```sh
   for p in fast tank; do
     ssh root@192.168.8.4 "midclt call -job pool.dataset.export_key $p" | tr -d '"\n' | wc -c   # 64
   done
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
efibootmgr -v           # note the new entry number NNNN
efibootmgr -o 0008,...  # put TrueNAS (0008) first again if -c moved it
```

### 3. Stage the bundle (HUMAN, on TrueNAS)

`truenas-stage.sh` copies everything the install needs onto the TrueNAS ESP
(`ludwig-installer/`, plaintext, until the install wipes it): both pool keys,
the SSH host keys, root's UPS router key, the SMB password export, the
`tailscale-ludwig` jail state and the fallback auth key if present. The
secrets never leave ludwig and are not printed.

```sh
scp ~/nixcfg/hosts/ludwig/truenas-stage.sh root@192.168.8.4:/tmp/
ssh root@192.168.8.4 sh /tmp/truenas-stage.sh --check   # read-only
ssh root@192.168.8.4 sh /tmp/truenas-stage.sh --auto    # stage and arm
```

Without `--auto` the installer boots, joins Tailscale and waits for you to
run the steps by hand. `--wipe` removes a staged bundle.

The installer and the final system reuse the jail's Tailscale node state:
same tailnet address (`100.79.17.127`), tag and approved subnet route, so the
remote LAN and the BMC stay reachable throughout.

### 4. Cut over (HUMAN)

On TrueNAS:

```sh
tmux kill-session -t zrepl
efibootmgr -n -b NNNN   # boot the installer once
shutdown -r now
```

The installer then runs on its own:

1. It joins Tailscale with the jail state and waits 3 minutes. To abort, SSH
   to `root@100.79.17.127` (or `root@192.168.8.4` through plato) and run
   `systemctl stop ludwig-autoinstall`.
2. `ludwig-install auto` runs preflight, esp, import, datasets, mount, seed,
   install and bootorder. Every step stops at the first unexpected state; the
   installer then stays up on Tailscale with TrueNAS still the default boot
   entry. Follow along with `journalctl -fu ludwig-autoinstall`.
3. On success it overwrites and deletes `ludwig-installer/` on the TrueNAS
   ESP, saves its log to `/persist/var/log/ludwig-install.log`, and reboots
   into NixOS once.

If the installer is unreachable both ways, a power cycle by someone on site
boots TrueNAS again.

### 5. Verify the first NixOS boot

```sh
ssh root@192.168.8.4
systemctl --failed
zpool status -x; zfs get -H keystatus fast tank
findmnt /mnt/tank /mnt/fast
tailscale status | head -3
journalctl -b -u samba-import-truenas-passdb   # one-time SMB password import
pdbedit -L
exportfs -v
zrepl status            # pull_mali resumes on its 10 minute interval
dig +short @192.168.8.2 example.com
upsc ups@localhost ups.status
ipmitool mc watchdog get; wdctl
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

TrueNAS imports the pools under its `/mnt` altroot as before. Its swap mirror
is degraded because the SSD partitions became ESPs; that is harmless. Data
written under NixOS and the `fast/encrypted` datasets stay in place.

### Cleanup once TrueNAS is retired

- If an install failed, `ludwig-installer/` may still be on the TrueNAS ESP;
  wipe it from TrueNAS with `truenas-stage.sh --wipe`.
- Destroy `tank/.system` and, after salvaging anything wanted, `fast/iocage`.
- `mali.key` sits in the old zrepl jail directory; ludwig never needs it.
- Remove TrueNAS boot entry `0008`; consider `zpool upgrade`.
