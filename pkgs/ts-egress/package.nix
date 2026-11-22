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

  vendorHash = "sha256-Kg6V2jx+TD8MUCo/pyUWat/HlULcXnsoH5Pw6OEJh38=";

  meta = {
    description = "Tailscale exit-node egress that decides every flow from grant capabilities";
    license = lib.licenses.asl20;
    mainProgram = "ts-egress";
    platforms = lib.platforms.linux;
  };
}
