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

  # tsnet, netstack and gVisor need patches/ for a policed exit node, see
  # README; scripts/vendor.sh applies them the same way outside Nix.
  modPostBuild = ''
    for p in ${./patches}/*.patch; do
      patch -p1 -d vendor/tailscale.com <"$p"
    done
    for p in ${./patches}/gvisor/*.patch; do
      patch -p1 -d vendor/gvisor.dev/gvisor <"$p"
    done
  '';
  vendorHash = "sha256-6YHf8OoxzJunvx++zc+VhkOJ1ZjWhBR6UOPzMRPPRyY=";

  meta = {
    description = "Tailscale exit-node egress that decides every flow from grant capabilities";
    license = lib.licenses.asl20;
    mainProgram = "ts-egress";
    platforms = lib.platforms.linux;
  };
}
