# https://nixos.wiki/wiki/Logseq
# https://github.com/logseq/logseq/issues/10851
final: prev: {
  logseq = prev.logseq.overrideAttrs (oldAttrs: {
    postFixup = ''
      makeWrapper ${prev.electron_30}/bin/electron $out/bin/${oldAttrs.pname} \
        --set "LOCAL_GIT_DIRECTORY" ${prev.git} \
        --add-flags $out/share/${oldAttrs.pname}/resources/app \
        --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations}}" \
        --prefix LD_LIBRARY_PATH : "${prev.lib.makeLibraryPath [ prev.stdenv.cc.cc.lib ]}"
    '';
  });
}
