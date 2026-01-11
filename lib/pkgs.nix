{
  lib,
  writers,
  babashka,
  ...
}:
rec {
  writeBbWith =
    {
      packages ? [ ],
      extraMakeWrapperArgs ? [ ],
    }:
    writers.makeScriptWriter {
      interpreter = lib.getExe babashka;

      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath packages)
      ]
      ++ extraMakeWrapperArgs;
    };

  writeBbBinWith = args: name: writeBbWith args "/bin/${name}";

  writeUpdateScript =
    {
      script,
      packageToUpdate,
      utils ? [ ],
    }:
    writeBbBinWith {
      packages = utils;
    } "update-${packageToUpdate}" script;
}
