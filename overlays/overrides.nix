final: prev: {
  roon-server = prev.roon-server.overrideAttrs (
    _old:
    let
      version = "2.71.1683";
      urlVersion = builtins.replaceStrings [ "." ] [ "0" ] version;
    in
    {
      inherit version;
      src = final.fetchurl {
        url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_${urlVersion}.tar.bz2";
        hash = "sha256-z/8rORTsDUhgh2hfep62jMVeAvX/c5HW4vBkVnvhcQc=";
      };

      # Roon now ships self-contained binaries instead of a shared .NET runtime.
      # See https://github.com/NixOS/nixpkgs/issues/552889.
      installPhase =
        let
          wrapBin = binPath: ''
            (
              binDir="$(dirname "${binPath}")"
              binName="$(basename "${binPath}")"
              actualBin="$binDir/$binName.exe"

              rm "${binPath}"
              makeWrapper "$actualBin" "${binPath}" \
                --argv0 "$binName" \
                --prefix LD_LIBRARY_PATH : "${
                  final.lib.makeLibraryPath [
                    final.alsa-lib
                    final.icu66
                    final.ffmpeg
                    final.openssl
                  ]
                }" \
                --prefix PATH : "$binDir" \
                --prefix PATH : "${
                  final.lib.makeBinPath [
                    final.alsa-utils
                    final.cifs-utils
                    final.ffmpeg
                  ]
                }" \
                --chdir "$binDir"
            )
          '';
        in
        ''
          runHook preInstall
          mkdir -p $out
          mv * $out
          rm $out/check.sh
          rm $out/start.sh
          rm $out/VERSION

          ${wrapBin "$out/Appliance/RAATServer"}
          ${wrapBin "$out/Appliance/RoonAppliance"}
          ${wrapBin "$out/Server/RoonServer"}

          mkdir -p $out/bin
          makeWrapper "$out/Server/RoonServer" "$out/bin/RoonServer" --chdir "$out"

          runHook postInstall
        '';
    }
  );

  #quickemu = prev.quickemu.overrideAttrs (oldAttrs: {
  #  postPatch =
  #    (oldAttrs.postPatch or "")
  #    + ''
  #      substituteInPlace quickemu \
  #        --replace-fail 'args+=(-nic bridge,br=''${network},model=virtio-net-pci''${MAC})' \
  #                       'args+=(-nic bridge,br=''${network},helper=/run/wrappers/bin/qemu-bridge-helper,model=virtio-net-pci''${MAC})'
  #    '';
  #});
}
