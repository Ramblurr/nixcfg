# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Hosts

<!-- BEGIN HOSTS -->

| Hostname | Purpose | Board | CPU | RAM | GPU | Channel | Role | OS |
|---|---|---|---|---|---|---|---|---|
| [addams](./hosts/addams/) | Router | Lenovo ThinkCentre M720q (312D) | Intel Core i5-8400T | 64 GiB | Intel UHD Graphics 630 | unstable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [debord](./hosts/debord/) | [Home production server and Home Assistant](https://notes.binaryelysium.com/HomeOps/) | Intel NUC12WSBi5 (NUC12WSHi5) | Intel Core i5-1240P | 64 GiB | Intel Iris Xe Graphics | unstable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [dewey](./hosts/dewey/) | [Home production server](https://notes.binaryelysium.com/HomeOps/) | Intel NUC10i5FNB (NUC10i5FNH) | Intel Core i5-10210U | 64 GiB | Intel UHD Graphics (Comet Lake-U) | unstable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [james](./hosts/james/) | Hetzner VPS | KVM Q35 (Hetzner vServer) | AMD EPYC-Milan (2 vCPUs) | 8000 MiB | Virtio 1.0 GPU | unstable | <span title="Cloud Server">&#x2601;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [mali](./hosts/mali/) | [Storage NAS](https://notes.binaryelysium.com/HomeOps/NAS/) | Supermicro X11SCH-F | Intel Core i3-8100 | 64 GiB | ASPEED Graphics Family | stable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [octoprint](./hosts/octoprint/) | 3D printer controller | Raspberry Pi 4 Model B Rev 1.5 | ARM Cortex-A72 (4 cores) | Unknown | Broadcom BCM2711 V3D | stable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [quine](./hosts/quine/) | [Primary workstation](https://notes.binaryelysium.com/HomeOps/PrimaryWorkstation/) | ASUS ProArt X670E-CREATOR WIFI | AMD Ryzen 9 7950X3D | 64 GiB | NVIDIA GeForce RTX 4090 | unstable | <span title="Desktop">&#x1F5A5;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [witt](./hosts/witt/) | Travel laptop | Framework 13 (AMD 7040 series) | Unknown | Unknown | Unknown | unstable | <span title="Laptop">&#x1F4BB;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |

**Roles**

- &#x1F5A5;&#xFE0F;: Desktop
- &#x1F4BB;&#xFE0F;: Laptop
- &#x1F3AE;&#xFE0F;: Games Machine
- &#x1F404;&#xFE0F;: Virtual Machine
- &#x2601;&#xFE0F;: Cloud Server
- &#x1F5C4;&#xFE0F;: Bare-metal server
- &#x1F9DF;&#xFE0F;: Not in service

**OS**

- &#x2744;&#xFE0F;: NixOS

<!-- END HOSTS -->

### Updating the host inventory

Edit [`hosts/inventory.nix`](./hosts/inventory.nix), then regenerate the table:

```sh
python3 scripts/generate-readme.py
python3 scripts/generate-readme.py --check
```

The generator needs only Python 3 and Nix. It evaluates the pure inventory file,
not the flake, host configurations, or private secrets. `--check` fails when the
generated section is stale and never writes the README. The `host-inventory`
flake check also checks the table and its agreement with the builder arguments.

Hostnames link to their source directories. Hardware fields left as `null` appear
as **Unknown**. `ramMiB` records installed or guest-allocated capacity, not currently
free memory. Exact multiples of 1024 MiB render as GiB; other sizes retain MiB.
Role is descriptive metadata only; it does not enable services or change imports.
Set `showInReadme = false` to omit a row without removing the host's build definition.
The role icons and OS icon are encoded as HTML entities in Markdown source.

The inventory supplies platform, channel, and Raspberry Pi facts to
[`flake/hosts.nix`](./flake/hosts.nix), which keeps executable module and overlay
additions separate. Public metadata is also available as `lib.nixcfg.hostInventory`:

```sh
nix eval --json --file hosts/inventory.nix
```

When adding a host, add its inventory entry and `hosts/<hostname>/` configuration;
add executable extras in `flake/hosts.nix` only if needed. Real host outputs and
private secret wiring still belong in `nixcfg-private`. Do not put private site
data or secrets in the public inventory. Guest discovery is unchanged.


---

# License and Inspiration


I got help from some cool configs like:

* [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
* [colemickens/nixcfg](https://github.com/colemickens/nixcfg)
* [huantianad/nixos-config](https://github.com/huantianad/nixos-config)
* [oddlama/nix-config](https://github.com/oddlama/nix-config)


[Licensed under MIT](./LICENSE), have at it!
