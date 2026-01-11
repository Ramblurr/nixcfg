{
  lib,
  writers,
  nushell,
  ...
}:
rec {
  writeNuWith =
    {
      packages ? [ ],
      plugins ? [ ],
      extraMakeWrapperArgs ? [ ],
    }:
    writers.makeScriptWriter {
      interpreter = lib.concatStringsSep " " [
        (lib.getExe nushell)
        "--no-config-file"
        "--plugins [${lib.concatStringsSep " " (map (p: lib.getExe p) plugins)}]"
      ];

      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath packages)
      ]
      ++ extraMakeWrapperArgs;
    };

  writeNuBinWith = args: name: writeNuWith args "/bin/${name}";

  nixUpdateScript =
    {
      packageToUpdate,
      version ? null,
    }:
    writeNuBinWith
      {
        packages = [
          # Note: these need to be passed in from the caller
          # since we don't have pkgs in scope here
        ];
      }
      "update-${packageToUpdate}"
      ''
        (nix-update
          --flake
          --format
          ${lib.concatStringsSep " " (lib.optional (version != null) "--version=${version}")}
          ${packageToUpdate})
      '';

  writeUpdateScript =
    {
      script,
      packageToUpdate,
      utils ? [ ],
      nushellPlugins ? [ ],
    }:
    writeNuBinWith {
      packages = utils;
      plugins = nushellPlugins;
    } "update-${packageToUpdate}" script;
}
