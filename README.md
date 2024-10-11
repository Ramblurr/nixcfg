# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Hosts

| Hostname     | Purpose                                                             | Channel                     | Source                                       |   |
|--------------|---------------------------------------------------------------------|-----------------------------|----------------------------------------------|---|
| quine        | [Primary workstation][workstation]                                  | ![NixOS Unstable][unstable] | [hosts/quine/](./hosts/quine/)               |   |
| aquinas      | Travel laptop (Dell XPS 15)                                         | ![NixOS Unstable][unstable] | [hosts/aquinas/](./hosts/aquinas/)           |   |
| witt         | Travel laptop (Framework 13)                                        | ![NixOS Unstable][unstable] | [hosts/witt/](./hosts/witt/)                 |   |
| dewey        | [Home Prod Server][home-ops]                                        | ![NixOS Unstable][unstable] | [hosts/dewey/](./hosts/dewey/)               |   |
| debord       | [Home Prod Server][home-ops] & Home Assistant                       | ![NixOS Unstable][unstable] | [hosts/debord/](./hosts/debord/)             |   |
| mali         | [Storage NAS][NAS]                                                  | ![NixOS Stable][stable]     | [hosts/mali/](./hosts/mali/)                 |   |
| OVOS Kitchen | My [OpenVoiceOS][ovos] instance (replaces Mycroft) for the kitchen. | ![NixOS Unstable][unstable] | [hosts/ovos-kitchen/](./hosts/ovos-kitchen/) |   |
| Fairybox     | Raspberry PI RFID Jukebox for my children                           | ![NixOS Stable][stable]     | [hosts/fairybox/](./hosts/fairybox/)         |   |
| VPN          | A VPN gateway  for my LAN                                           | ![NixOS Stable][stable]     | [hosts/vpn](./hosts/vpn)                     |   |


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
