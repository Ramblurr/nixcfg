_final: prev: {
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

  #quickemu = prev.quickemu.overrideAttrs (oldAttrs: {
  #  postPatch =
  #    (oldAttrs.postPatch or "")
  #    + ''
  #      substituteInPlace quickemu \
  #        --replace-fail 'args+=(-nic bridge,br=''${network},model=virtio-net-pci''${MAC})' \
  #                       'args+=(-nic bridge,br=''${network},helper=/run/wrappers/bin/qemu-bridge-helper,model=virtio-net-pci''${MAC})'
  #    '';
  #});

  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (pyFinal: pyPrev: {
      django-tenants = pyPrev.django-tenants.overridePythonAttrs (old: {
        # ref: https://github.com/NixOS/nixpkgs/issues/516785
        postInstall = (old.postInstall or "") + ''
          rm -rf "$out/${pyFinal.python.sitePackages}/docs"
        '';
      });
    })
  ];
}
