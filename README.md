# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

In the processes of nixifying everything (except maybe my [k8s@home cluster](https://github.com/ramblurr/home-ops))!

## Hosts

### Quine - workstation

[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

[hosts/unstable/x86_64-linux/quine/README.md](./hosts/unstable/x86_64-linux/quine/README.md)

```sh
task build:quine
task test:quine
task switch:quine
```

### Mali - NAS

[![NixOS Stable](https://img.shields.io/badge/NixOS-stable-green.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

[hosts/stable/x86_64-linux/mali/README.md](./hosts/stable/x86_64-linux/mali/README.md)


Deploy remotely!

```sh
task build:mali
task test:mali
task switch:mali
```

### OVOS Kitchen

[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

[hosts/unstable/aarch64-linux/ovos-kitchen/README.md](./hosts/unstable/aarch64-linux/ovos-kitchen/README.md)

My [OpenVoiceOS][ovos] instance (replaces Mycroft) for the kitchen.

### OVOS Kitchen Satellite

[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

[hosts/unstable/aarch64-linux/ovos-kitchen-sat/README.md](./hosts/unstable/aarch64-linux/ovos-kitchen-sat/README.md)

The microphone satellite that feeds audio back to OVOS Kitchen.


## Future

```
35ffd069f08245019cee53cbc850bf7e
fa286d89865747e9a9208e13beb24c59
f0c2388e13fb4818ac67d59baa93d578
```

# License and Inspiration


I got help from some cool configs like:

* [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
* [colemickens/nixcfg](https://github.com/colemickens/nixcfg)
* [huantianad/nixos-config](https://github.com/huantianad/nixos-config)


[Licensed under MIT](./LICENSE), have at it!


[ovos]: https://github.com/OpenVoiceOS/
