# Public host facts only. Do not import host modules, site data, or secrets here.
# Hardware values describe the machine, not desired NixOS settings. Null means
# unknown; fill it from verified hardware information rather than guessing.
let
  hosts = {
    addams = {
      # Jane Addams - https://en.wikipedia.org/wiki/Jane_Addams
      purpose = "Router";
      system = "x86_64-linux";
      channel = "unstable";
      role = "server";
      board = "Lenovo ThinkCentre M720q (312D)";
      cpu = "Intel Core i5-8400T";
      ramMiB = 65536;
      gpu = "Intel UHD Graphics 630";
    };
    debord = {
      # Guy Debord - https://en.wikipedia.org/wiki/Guy_Debord
      purpose = "Home production server and Home Assistant";
      documentation = "https://notes.binaryelysium.com/HomeOps/";
      system = "x86_64-linux";
      channel = "unstable";
      role = "server";
      board = "Intel NUC12WSBi5 (NUC12WSHi5)";
      cpu = "Intel Core i5-1240P";
      ramMiB = 65536;
      gpu = "Intel Iris Xe Graphics";
    };
    dewey = {
      # John Dewey - https://en.wikipedia.org/wiki/John_Dewey
      purpose = "Home production server";
      documentation = "https://notes.binaryelysium.com/HomeOps/";
      system = "x86_64-linux";
      channel = "unstable";
      role = "server";
      board = "Intel NUC10i5FNB (NUC10i5FNH)";
      cpu = "Intel Core i5-10210U";
      ramMiB = 65536;
      gpu = "Intel UHD Graphics (Comet Lake-U)";
    };
    james = {
      # William James - https://en.wikipedia.org/wiki/William_James
      purpose = "Hetzner VPS";
      system = "x86_64-linux";
      channel = "unstable";
      role = "cloud";
      board = "KVM Q35 (Hetzner vServer)";
      cpu = "AMD EPYC-Milan (2 vCPUs)";
      ramMiB = 8000;
      gpu = "Virtio 1.0 GPU";
    };
    mali = {
      purpose = "Storage NAS";
      documentation = "https://notes.binaryelysium.com/HomeOps/NAS/";
      system = "x86_64-linux";
      channel = "stable";
      role = "server";
      board = "Supermicro X11SCH-F";
      cpu = "Intel Core i3-8100";
      ramMiB = 65536;
      gpu = "ASPEED Graphics Family";
    };
    octoprint = {
      purpose = "3D printer controller";
      system = "aarch64-linux";
      channel = "stable";
      role = "server";
      isRpi = true;
      board = "Raspberry Pi 4 Model B Rev 1.5";
      cpu = "ARM Cortex-A72 (4 cores)";
      ramMiB = 2048;
      gpu = "Broadcom BCM2711 V3D";
    };
    peirce = {
      # Charles Sanders Peirce - https://en.wikipedia.org/wiki/Charles_Sanders_Peirce
      purpose = "Home production server";
      system = "x86_64-linux";
      channel = "unstable";
      role = "server";
      board = "Gigabyte Z390 I AORUS PRO WIFI-CF";
      cpu = "Intel Core i5-9600K";
      ramMiB = 65536;
      gpu = "NVIDIA GeForce GTX 1070 Ti";
    };
    quine = {
      # Willard Van Orman Quine - https://en.wikipedia.org/wiki/Willard_Van_Orman_Quine
      purpose = "Primary workstation";
      documentation = "https://notes.binaryelysium.com/HomeOps/PrimaryWorkstation/";
      system = "x86_64-linux";
      channel = "unstable";
      role = "desktop";
      board = "ASUS ProArt X670E-CREATOR WIFI";
      cpu = "AMD Ryzen 9 7950X3D";
      ramMiB = 65536;
      gpu = "NVIDIA GeForce RTX 4090";
    };
    thinkpad1 = {
      purpose = "Family laptop";
      system = "x86_64-linux";
      channel = "stable";
      role = "laptop";
      board = "Lenovo ThinkPad X13 Yoga Gen 1";
      cpu = "Intel Core i5-10310U";
      ramMiB = 16384;
      gpu = "Intel UHD Graphics";
    };
    witt = {
      # Ludwig Wittgenstein - https://en.wikipedia.org/wiki/Ludwig_Wittgenstein
      purpose = "Travel laptop";
      system = "x86_64-linux";
      channel = "unstable";
      role = "laptop";
      board = "Framework 13 (AMD 7040 series)"; # hosts/witt/hardware.nix
      cpu = "AMD Ryzen 5 7640U";
      ramMiB = 32768;
      gpu = "AMD Radeon 760M";
    };
    wyoming-satellite-bedroom = {
      purpose = "Bedroom voice satellite";
      system = "aarch64-linux";
      channel = "stable";
      role = "server";
      showInReadme = false;
      isRpi = true;
      board = "Raspberry Pi 4"; # hosts/wyoming-satellite-bedroom/hardware.nix
    };
  };
in
builtins.mapAttrs (
  _: host:
  {
    os = "nixos";
    isRpi = false;
    board = null;
    cpu = null;
    ramMiB = null;
    gpu = null;
  }
  // host
) hosts
