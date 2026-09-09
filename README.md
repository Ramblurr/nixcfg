# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Hosts

<!-- BEGIN HOSTS -->

| Hostname | Purpose | Board | CPU | RAM | GPU | Channel | Role | OS |
|---|---|---|---|---|---|---|---|---|
| [addams](./hosts/addams/) | Router | Unknown | Unknown | Unknown | Unknown | unstable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [debord](./hosts/debord/) | [Home production server and Home Assistant](https://notes.binaryelysium.com/HomeOps/) | Unknown | Unknown | Unknown | Unknown | unstable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [dewey](./hosts/dewey/) | [Home production server](https://notes.binaryelysium.com/HomeOps/) | Unknown | Unknown | Unknown | Unknown | unstable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [james](./hosts/james/) | Hetzner VPS | QEMU virtual machine | Unknown | Unknown | Unknown | unstable | <span title="Cloud Server">&#x2601;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [mali](./hosts/mali/) | [Storage NAS](https://notes.binaryelysium.com/HomeOps/NAS/) | Unknown | Unknown | Unknown | Unknown | stable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [octoprint](./hosts/octoprint/) | 3D printer controller | Raspberry Pi 4 | Unknown | Unknown | Unknown | stable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [quine](./hosts/quine/) | [Primary workstation](https://notes.binaryelysium.com/HomeOps/PrimaryWorkstation/) | Unknown | Unknown | Unknown | Unknown | unstable | <span title="Desktop">&#x1F5A5;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [witt](./hosts/witt/) | Travel laptop | Framework 13 (AMD 7040 series) | Unknown | Unknown | Unknown | unstable | <span title="Laptop">&#x1F4BB;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |
| [wyoming-satellite-bedroom](./hosts/wyoming-satellite-bedroom/) | Bedroom voice satellite | Raspberry Pi 4 | Unknown | Unknown | Unknown | stable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |

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
as **Unknown**; `ramGiB` records installed capacity, not currently free memory.
Role is descriptive metadata only; it does not enable services or change imports.
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
