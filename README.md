# nixcfg
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

In the processes of nixifying everything (except maybe my [k8s@home cluster](https://github.com/ramblurr/home-ops))!

## Hosts

| Hostname               | Purpose                                                             | Channel                     | Source                                                                                             |
|------------------------|---------------------------------------------------------------------|-----------------------------|----------------------------------------------------------------------------------------------------|
| Quine                  | My [primary workstation][workstation]                                              | ![NixOS Unstable][unstable] | [hosts/unstable/x86_64-linux/quine/](./hosts/unstable/x86_64-linux/quine/)                         |
| Mali                   | [Home office NAS][NAS]                                              | ![NixOS Stable][stable]     | [hosts/stable/x86_64-linux/mali/](./hosts/stable/x86_64-linux/mali/)                               |
| OVOS Kitchen           | My [OpenVoiceOS][ovos] instance (replaces Mycroft) for the kitchen. | ![NixOS Unstable][unstable] | [hosts/unstable/aarch64-linux/ovos-kitchen/](./hosts/unstable/aarch64-linux/ovos-kitchen/)         |
| OVOS Kitchen Satellite | The microphone satellite that feeds audio back to OVOS Kitchen.     | ![NixOS Unstable][unstable] | [hosts/unstable/aarch64-linux/ovos-kitchen-sat/](./hosts/unstable/aarch64-linux/ovos-kitchen-sat/) |


Example build commands:

```sh
task build:quine
task test:quine
task switch:quine
task image:ovos-kitchen
```

---

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
[unstable]: https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white
[stable]: https://img.shields.io/badge/NixOS-stable-green.svg?style=flat-square&logo=NixOS&logoColor=white
[NAS]: https://notes.binaryelysium.com/HomeOps/NAS/
[workstation]: https://notes.binaryelysium.com/HomeOps/PrimaryWorkstation/
