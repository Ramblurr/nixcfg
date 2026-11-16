#!/bin/sh
# Stage the ludwig migration bundle on the TrueNAS ESP. Run as root on ludwig
# while it still runs TrueNAS CORE. Secrets go straight from TrueNAS to its own
# ESP; nothing is printed except sizes.
#
#   sh truenas-stage.sh --check   read-only: verify every source, write nothing
#   sh truenas-stage.sh           stage the bundle; the installer waits for you
#   sh truenas-stage.sh --auto    stage the bundle and arm the unattended install
#   sh truenas-stage.sh --wipe    overwrite and delete a staged bundle
set -eu
umask 077

mode=${1:-stage}
esp=/dev/gptid/d4fb2736-d81c-11ec-bff8-d05099fb0f82
ssh_dir=/usr/local/etc/ssh
router_key=/root/.ssh/id_rsa
router_known_hosts=/root/.ssh/known_hosts
# A tagged, pre-approved Tailscale auth key; the installer and the NixOS host
# log in with it under their own names.
ts_authkey=/root/ludwig-ts-authkey

die() {
	echo "ERROR: $*" >&2
	exit 1
}

check_sources() {
	[ -e "$esp" ] || die "TrueNAS ESP $esp not found"
	for t in ed25519 rsa ecdsa; do
		[ -s "$ssh_dir/ssh_host_${t}_key" ] || die "missing $ssh_dir/ssh_host_${t}_key"
		[ -s "$ssh_dir/ssh_host_${t}_key.pub" ] || die "missing $ssh_dir/ssh_host_${t}_key.pub"
	done
	[ -s "$router_key" ] || die "missing $router_key (UPS router key)"
	[ -s "$router_known_hosts" ] || die "missing $router_known_hosts"
	[ -s "$ts_authkey" ] || die "missing $ts_authkey (Tailscale auth key)"
	for p in fast tank; do
		n=$(midclt call -job pool.dataset.export_key "$p" | tr -d '"\n' | wc -c | tr -d ' ')
		[ "$n" -eq 64 ] || die "exported $p key is $n bytes, expected 64"
	done
	n=$(pdbedit -L -w | wc -l | tr -d ' ')
	[ "$n" -gt 0 ] || die "pdbedit exported no SMB accounts"
	echo "sources OK: pool keys, host keys, router key, $n SMB accounts, tailscale auth key"
}

with_esp() {
	mount | grep -q "^$esp " && die "$esp is already mounted"
	mnt=$(mktemp -d /tmp/ludwig-esp.XXXXXX)
	mount_msdosfs "$esp" "$mnt"
	trap 'umount "$mnt" 2>/dev/null || true; rmdir "$mnt" 2>/dev/null || true' EXIT
	bundle=$mnt/ludwig-installer
}

wipe() {
	[ -d "$bundle" ] || return 0
	find "$bundle" -type f | while read -r f; do
		size=$(stat -f %z "$f")
		dd if=/dev/zero of="$f" bs=4096 conv=notrunc count=$(((size + 4095) / 4096)) 2>/dev/null
	done
	sync
	rm -rf "$bundle"
	sync
}

stage() {
	wipe
	m=$bundle/migration
	mkdir -p "$m/ssh" "$m/ups"
	for p in fast tank; do
		midclt call -job pool.dataset.export_key "$p" | tr -d '"\n' >"$m/$p.key"
	done
	for t in ed25519 rsa ecdsa; do
		cp "$ssh_dir/ssh_host_${t}_key" "$ssh_dir/ssh_host_${t}_key.pub" "$m/ssh/"
	done
	cp "$router_key" "$m/ups/router_key"
	cp "$router_known_hosts" "$m/ups/known_hosts"
	pdbedit -L -w >"$m/smbpasswd"
	cp "$ts_authkey" "$bundle/authkey"
	if [ "$mode" = --auto ]; then
		: >"$bundle/autoinstall"
	fi
	sync
	for p in fast tank; do
		[ "$(wc -c <"$m/$p.key" | tr -d ' ')" -eq 64 ] || die "staged $p key has the wrong size"
	done
	find "$bundle" -type f -exec stat -f '%z %N' {} + | sed "s|$mnt/||"
}

case $mode in
--check) check_sources ;;
stage | --auto)
	check_sources
	with_esp
	stage
	if [ "$mode" = --auto ]; then
		echo "bundle staged; the installer will run unattended"
	else
		echo "bundle staged; the installer will wait for you"
	fi
	;;
--wipe)
	with_esp
	wipe
	echo "bundle wiped"
	;;
*) die "usage: sh truenas-stage.sh [--check|--auto|--wipe]" ;;
esac
