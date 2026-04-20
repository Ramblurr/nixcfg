_inputs: final: prev: {
  lib = prev.lib // {
    my = {
      mkCompatibleDtsFile =
        dtbo:
        let
          drv =
            final.runCommand "fix-dts"
              {
                nativeBuildInputs = with final; [
                  dtc
                  gnused
                ];
              }
              ''
                mkdir "$out"
                dtc -I dtb -O dts ${dtbo} | sed -e 's/bcm2835/bcm2711/' > $out/overlay.dts
              '';
        in
        "${drv}/overlay.dts";
    };
  };

}
