# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Hosts

| Hostname | Purpose                                       | Channel                     | Source                           |   |
|----------|-----------------------------------------------|-----------------------------|----------------------------------|---|
| debord   | [Home Prod Server][home-ops] & Home Assistant | ![NixOS Unstable][unstable] | [hosts/debord/](./hosts/debord/) |   |
| dewey    | [Home Prod Server][home-ops]                  | ![NixOS Unstable][unstable] | [hosts/dewey/](./hosts/dewey/)   |   |
| mali     | [Storage NAS][NAS]                            | ![NixOS Stable][stable]     | [hosts/mali/](./hosts/mali/)     |   |
| quine    | [Primary workstation][workstation]            | ![NixOS Unstable][unstable] | [hosts/quine/](./hosts/quine/)   |   |
| witt     | Travel laptop (Framework 13)                  | ![NixOS Unstable][unstable] | [hosts/witt/](./hosts/witt/)     |   |
| addams   | Router                                        | ![NixOS Unstable][unstable] | [hosts/addams/](./hosts/addams/) |   |
| james    | Hetzner VPS                                   | ![NixOS Unstable][unstable] | [hosts/james/](./hosts/james/) |   |


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
