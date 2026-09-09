{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  inherit (inputs.self.lib.nixcfg) hosts hostInventory;
  names = builtins.attrNames hostInventory;
  fields = [
    "purpose"
    "documentation"
    "system"
    "channel"
    "isRpi"
    "board"
    "cpu"
    "ramGiB"
    "gpu"
    "role"
    "os"
  ];
  validHost =
    name:
    let
      host = hostInventory.${name};
      builder = hosts.${name};
    in
    lib.assertMsg (
      lib.subtractLists fields (builtins.attrNames host) == [ ]
    ) "${name}: unexpected public inventory fields"
    && lib.assertMsg (builtins.pathExists (
      ../hosts + "/${name}/default.nix"
    )) "${name}: missing host source directory"
    && lib.assertMsg (
      builtins.isBool host.isRpi && builtins.isString host.system
    ) "${name}: invalid host platform or Raspberry Pi flag"
    && lib.assertMsg (
      builder.system == host.system
      && builder.isStable == (host.channel == "stable")
      && builder.isRpi == host.isRpi
    ) "${name}: builder facts disagree with the public inventory";
in
assert lib.assertMsg (builtins.attrNames hosts == names) "Builder and inventory host names differ";
assert builtins.all validHost names;
pkgs.runCommand "check-host-inventory"
  {
    nativeBuildInputs = [ pkgs.python3 ];
    inventoryJSON = pkgs.writeText "host-inventory.json" (builtins.toJSON hostInventory);
  }
  ''
    mkdir -p scripts/tests
    cp ${../scripts/generate-readme.py} scripts/generate-readme.py
    cp ${../scripts/tests/test_generate_readme.py} scripts/tests/test_generate_readme.py
    python3 -B scripts/tests/test_generate_readme.py
    python3 -B scripts/generate-readme.py \
      --inventory-json "$inventoryJSON" --readme ${../README.md} --check
    touch "$out"
  ''
