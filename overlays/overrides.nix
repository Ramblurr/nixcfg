final: prev: {
  nginxQuic = prev.nginxQuic.override {
    modules = prev.lib.unique (
      prev.nginxQuic.modules
      ++ [
        prev.nginxModules.brotli
        prev.nginxModules.zstd
      ]
    );
  };

  #roon-server = prev.roon-server.overrideAttrs (
  #  old:
  #  let
  #    version = "2.50.1528";
  #    urlVersion = builtins.replaceStrings [ "." ] [ "0" ] version;
  #  in
  #  {
  #    version = version;
  #    src = prev.fetchurl {
  #      url = "https://download.roonlabs.com/updates/production/RoonServer_linuxx64_${urlVersion}.tar.bz2";
  #      hash = "sha256-8Dxxjj/5JSuXeTxTV2l37ZAmf6BDdhykPSinmgZeEVY=";
  #    };
  #  }
  #);

  logseq = prev.logseq.overrideAttrs (oldAttrs: {
    postFixup = ''
      makeWrapper ${prev.electron}/bin/electron $out/bin/${oldAttrs.pname} \
        --set "LOCAL_GIT_DIRECTORY" ${prev.git} \
        --add-flags $out/share/${oldAttrs.pname}/resources/app \
        --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations}}" \
        --prefix LD_LIBRARY_PATH : "${prev.lib.makeLibraryPath [ prev.stdenv.cc.cc.lib ]}"
    '';
  });

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
