final: prev: {
  bookorbit = prev.bookorbit.overrideAttrs (
    finalAttrs: oldAttrs: {
      version = "3.3.0";
      # Disable Nest's spinner, which loops on Nix's zero-width build terminal.
      env = (oldAttrs.env or { }) // {
        CI = "true";
      };
      # SWC's native loader rejects the Nix sandbox root's ownership.
      buildPhase =
        builtins.replaceStrings
          [ "pnpm --filter server run build" ]
          [ "pnpm --filter server run build --builder tsc" ]
          oldAttrs.buildPhase;
      # Keep tsc's output layout compatible with the service and migration wrappers.
      postPatch = (oldAttrs.postPatch or "") + ''
        substituteInPlace server/tsconfig.build.json \
          --replace-fail '"extends": "./tsconfig.json",' \
            '"extends": "./tsconfig.json", "compilerOptions": { "rootDir": "./src" }, "include": ["src/**/*.ts"],'
      '';
      postInstall = (oldAttrs.postInstall or "") + ''
        test -f "$out/lib/dist/main.js"
        test -f "$out/lib/dist/scripts/migrate.js"
      '';
      src = final.fetchFromGitHub {
        owner = "bookorbit";
        repo = "bookorbit";
        tag = "v${finalAttrs.version}";
        hash = "sha256-cvTI1riCQpJDtlbZH+BKcIJnoXU+cufVD9BuZtK0qPc=";
      };
      pnpmDeps = oldAttrs.pnpmDeps.override {
        inherit (finalAttrs) version src;
        hash = "sha256-VW5h6WRizzJg88BZgYd8xR5l9n+hxtyHOldC9ojbiZ0=";
      };
    }
  );

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
