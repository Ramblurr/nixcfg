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
    };
    debord = {
      # Guy Debord - https://en.wikipedia.org/wiki/Guy_Debord
      purpose = "Home production server and Home Assistant";
      documentation = "https://notes.binaryelysium.com/HomeOps/";
      system = "x86_64-linux";
      channel = "unstable";
      role = "server";
    };
    dewey = {
      # John Dewey - https://en.wikipedia.org/wiki/John_Dewey
      purpose = "Home production server";
      documentation = "https://notes.binaryelysium.com/HomeOps/";
      system = "x86_64-linux";
      channel = "unstable";
      role = "server";
    };
    james = {
      # William James - https://en.wikipedia.org/wiki/William_James
      purpose = "Hetzner VPS";
      system = "x86_64-linux";
      channel = "unstable";
      role = "cloud";
      board = "QEMU virtual machine"; # hosts/james/hardware.nix
    };
    mali = {
      purpose = "Storage NAS";
      documentation = "https://notes.binaryelysium.com/HomeOps/NAS/";
      system = "x86_64-linux";
      channel = "stable";
      role = "server";
    };
    octoprint = {
      purpose = "3D printer controller";
      system = "aarch64-linux";
      channel = "stable";
      role = "server";
      isRpi = true;
      board = "Raspberry Pi 4"; # hosts/octoprint/hardware.nix
    };
    quine = {
      # Willard Van Orman Quine - https://en.wikipedia.org/wiki/Willard_Van_Orman_Quine
      purpose = "Primary workstation";
      documentation = "https://notes.binaryelysium.com/HomeOps/PrimaryWorkstation/";
      system = "x86_64-linux";
      channel = "unstable";
      role = "desktop";
    };
    witt = {
      # Ludwig Wittgenstein - https://en.wikipedia.org/wiki/Ludwig_Wittgenstein
      purpose = "Travel laptop";
      system = "x86_64-linux";
      channel = "unstable";
      role = "laptop";
      board = "Framework 13 (AMD 7040 series)"; # hosts/witt/hardware.nix
    };
    wyoming-satellite-bedroom = {
      purpose = "Bedroom voice satellite";
      system = "aarch64-linux";
      channel = "stable";
      role = "server";
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
    ramGiB = null;
    gpu = null;
  }
  // host
) hosts
