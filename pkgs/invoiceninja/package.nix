{
  stdenv,
  lib,
  fetchurl,
  dataDir ? "/var/lib/invoiceninja",
  runtimeDir ? "/run/invoiceninja",
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "invoiceninja";
  version = "5.10.43";

  src = fetchurl {
    url = "https://github.com/invoiceninja/invoiceninja/releases/download/v${finalAttrs.version}/invoiceninja.tar";
    hash = "sha256-nFO9nZOy3gFP+gm2JpGqwMWhMQ2gaLrJCmXUj8RVp6c=";
  };

  sourceRoot = ".";

  patchFlags = [ "-p0" ];
  patches = [
    # support connection redis with unix domain sockets
    # upstream has not yet accepted this patch
    ./invoiceninja-redis.patch
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/invoiceninja
    cp -r . $out/share/invoiceninja

    pushd $out/share/invoiceninja
    chmod +x artisan
    mv bootstrap bootstrap-static
    mv storage storage-static
    mv resources resources-static
    ln -s ${dataDir}/.env $out/share/invoiceninja
    ln -s ${dataDir}/storage $out/share/invoiceninja
    ln -s ${dataDir}/storage-public $out/share/invoiceninja/public/storage
    ln -s ${runtimeDir}/bootstrap $out/share/invoiceninja/bootstrap
    ln -s ${runtimeDir}/resources $out/share/invoiceninja/resources
    popd

    # Create standard directory structure
    #mkdir -p $out/bin

    # Create wrapper script
    #cat > $out/bin/invoiceninja <<EOF
    ##!$ {stdenv.shell}
    #exec $ {php}/bin/php $out/share/invoiceninja/artisan "\$@"
    #EOF
    #chmod +x $out/bin/invoiceninja

    runHook postInstall
  '';

  meta = {
    changelog = "https://github.com/invoiceninja/invoiceninja/releases/tag/v${finalAttrs.version}";
    homepage = "https://github.com/invoiceninja/invoiceninja";
    description = "Free invoicing software for small businesses";
    license = lib.licenses.elastic20;
    maintainers = with lib.maintainers; [ ramblurr ];
  };
})
