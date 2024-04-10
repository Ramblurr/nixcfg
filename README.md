# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Hosts

| Hostname     | Purpose                                                             | Channel                     | Source                                                                                     |
|--------------|---------------------------------------------------------------------|-----------------------------|--------------------------------------------------------------------------------------------|
| quine        | [Primary workstation][workstation]                                  | ![NixOS Unstable][unstable] | [hosts/unstable/x86_64-linux/quine/](./hosts/unstable/x86_64-linux/quine/)                 |
| dewey        | [Home Prod Server][home-ops]                                        | ![NixOS Unstable][unstable] | [hosts/unstable/x86_64-linux/dewey/](./hosts/unstable/x86_64-linux/dewey/)                 |
| peirce       | [Kubernetes control plane][home-ops]                                | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/peirce/](./hosts/stable/x86_64-linux/peirce/)                   |
| debord       | [Kubernetes control plane][home-ops]                                | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/debord/](./hosts/stable/x86_64-linux/debord/)                   |
| mill         | [Kubernetes worker][home-ops]                                       | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/mill/](./hosts/stable/x86_64-linux/mill/)                       |
| ibnsina      | [Kubernetes worker][home-ops]                                       | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/ibnsina/](./hosts/stable/x86_64-linux/ibnsina/)                 | -->
| mali         | [Storage NAS][NAS]                                                  | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/mali/](./hosts/stable/x86_64-linux/mali/)                       |
| OVOS Kitchen | My [OpenVoiceOS][ovos] instance (replaces Mycroft) for the kitchen. | ![NixOS Unstable][unstable] | [hosts/unstable/aarch64-linux/ovos-kitchen/](./hosts/unstable/aarch64-linux/ovos-kitchen/) |
| Fairybox     | Raspberry PI RFID Jukebox for my children                           | ![NixOS Stable][stable]     | [hosts/stable/aarch64-linux/fairybox/](./hosts/stable/aarch64-linux/fairybox/)             |
| VPN          | A VPN gateway  for my LAN                                           | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/vpn](./hosts/stable/x86_64-linux/vpn)                           |


Example build commands:

```sh
task build host=quine
task test host=quine
task switch host=quine
task image host=ovos-kitchen
```

---

# License and Inspiration


I got help from some cool configs like:

* [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
* [colemickens/nixcfg](https://github.com/colemickens/nixcfg)
* [huantianad/nixos-config](https://github.com/huantianad/nixos-config)
* [oddlama/nix-config](https://github.com/oddlama/nix-config)


[Licensed under MIT](./LICENSE), have at it!


[ovos]: https://github.com/OpenVoiceOS/
[unstable]: https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white
[stable]: https://img.shields.io/badge/NixOS-stable-green.svg?style=flat-square&logo=NixOS&logoColor=white
[NAS]: https://notes.binaryelysium.com/HomeOps/NAS/
[workstation]: https://notes.binaryelysium.com/HomeOps/PrimaryWorkstation/
[home-ops]: https://notes.binaryelysium.com/HomeOps/
[home-ops-git]: https://github.com/ramblurr/home-ops
