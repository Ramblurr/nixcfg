final: prev: {
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
