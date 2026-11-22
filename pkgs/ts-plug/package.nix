{
  lib,
  buildGoModule,
}:
buildGoModule {
  pname = "ts-plug";
  version = "0-unstable-2026-11-21-t3";

  src = ./src;

  vendorHash = "sha256-v/UpoJIh18uOKEyOZzD1jlwlaF/bQNmGFdymt8Va1PU=";
  subPackages = [ "cmd/ts-multi-plug" ];

  postInstall = ''
    mv $out/bin/ts-multi-plug $out/bin/ts-plug
  '';

  meta = {
    description = "Expose a local service to a tailnet and proxy local clients into it";
    homepage = "https://github.com/tailscale/ts-plug";
    license = lib.licenses.bsd3;
    mainProgram = "ts-plug";
  };
}
