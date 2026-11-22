{
  lib,
  buildGoModule,
}:
buildGoModule {
  pname = "ts-egress";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./go.mod
      ./go.sum
      ./cmd
      ./internal
    ];
  };

  # tsnet and netstack need patches/ for a policed exit node, see README;
  # scripts/vendor.sh applies them the same way outside Nix.
  modPostBuild = ''
    for p in ${./patches}/*.patch; do
      patch -p1 -d vendor/tailscale.com <"$p"
    done
  '';
  vendorHash = "sha256-7Mk9x6+ybA/9HeT9RpMzq9YvaN/bNfRo2E9tZvGrNxA=";

  meta = {
    description = "Tailscale exit-node egress that decides every flow from grant capabilities";
    license = lib.licenses.asl20;
    mainProgram = "ts-egress";
    platforms = lib.platforms.linux;
  };
}
