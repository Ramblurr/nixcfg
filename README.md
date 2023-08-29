# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

In the processes of nixifying everything.

## Hosts

### Quine - workstation

[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

-> [hosts/unstable/x86_64-linux/quine/README.md](./hosts/unstable/x86_64-linux/quine/README.md)

``` sh
sudo nixos-rebuild --flake  .#quine build
```

### Mali - workstation

[![NixOS Stable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

-> [hosts/stable/x86_64-linux/mali/README.md](./hosts/stable/x86_64-linux/mali/README.md)

``` sh
sudo nixos-rebuild --flake  .#mali build
```

# License and Inspiration


I got help from some cool configs like:

* [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
* [colemickens/nixcfg](https://github.com/colemickens/nixcfg)
* [huantianad/nixos-config](https://github.com/huantianad/nixos-config)


[Licensed under MIT](./LICENSE), have at it!
