final: prev: {
  # https://nixpk.gs/pr-tracker.html?pr=325059
  #qemu = prev.qemu.override { cephSupport = false; };
  #qemu_full = prev.qemu_full.override { cephSupport = false; };
  quickemu = prev.quickemu.overrideAttrs (oldAttrs: {
    postPatch =
      (oldAttrs.postPatch or "")
      + ''
        substituteInPlace quickemu \
          --replace-fail 'args+=(-nic bridge,br=''${network},model=virtio-net-pci''${MAC})' \
                         'args+=(-nic bridge,br=''${network},helper=/run/wrappers/bin/qemu-bridge-helper,model=virtio-net-pci''${MAC})'
      '';
  });
}
