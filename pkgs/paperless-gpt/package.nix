{
  lib,
  buildGoModule,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_22,
  nix-update,
  python3,
  runCommand,
  writeShellScript,
}:
let
  pname = "paperless-gpt";
  version = "0.27.0-unstable-2026-09-11";
  src = fetchFromGitHub {
    owner = "icereed";
    repo = "paperless-gpt";
    rev = "7e736115b567ae2a95afbe6022e553a6ec2ab33a";
    hash = "sha256-1QPS7m5LNEb+wtvSFZjEaK8KDYmz8P2juT/QH9AzaJY=";
  };
  frontend = buildNpmPackage {
    pname = "${pname}-frontend";
    inherit version src;
    sourceRoot = "${src.name}/web-app";
    nodejs = nodejs_22;
    npmDepsHash = "sha256-7PxH8kS28x8Sv5tD+Kohdv1CakKh8gIA9e9LGcWA960=";
    npm_config_ignore_scripts = "true";
    installPhase = ''
      runHook preInstall
      cp -r dist "$out"
      runHook postInstall
    '';
  };
in
buildGoModule (finalAttrs: {
  inherit pname version src;
  vendorHash = "sha256-81a3B16v4rF8ut2HcT0w/KgT8XaZahQ3GPv/oEenXPk=";
  patches = [ ./unix-socket.patch ];
  subPackages = [ "." ];
  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
    "-X main.commit=${src.rev}"
  ];
  # Focused upstream tests use a local HTTP fixture, including PDF upload cleanup.
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    go test ./ocr -run '^Test(NewMistral|Mistral)' -count=1 -v
    runHook postCheck
  '';
  preBuild = ''
    mkdir -p web-app/dist
    cp -r ${frontend}/. web-app/dist/
  '';
  postInstall = ''
    mkdir -p "$out/share/paperless-gpt"
    cp -r default_prompts "$out/share/paperless-gpt/"
  '';
  passthru = {
    inherit frontend;
    tests.unix-socket = runCommand "paperless-gpt-unix-socket-test" { } ''
      ${python3}/bin/python ${./test-unix-socket.py} ${finalAttrs.finalPackage}
      touch "$out"
    '';
    updateScript = writeShellScript "update-paperless-gpt" ''
      set -euo pipefail
      exec ${lib.getExe nix-update} --flake --version=branch=main \
        --subpackage frontend paperless-gpt "$@"
    '';
  };
  meta = {
    description = "LLM metadata and OCR sidecar for Paperless-ngx";
    homepage = "https://github.com/icereed/paperless-gpt";
    license = lib.licenses.mit;
    mainProgram = "paperless-gpt";
    platforms = lib.platforms.linux;
  };
})
