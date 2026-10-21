{
  lib,
  buildGo127Module,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_22,
  nix-update,
  python3,
  runCommand,
  writeShellScriptBin,
}:
let
  pname = "paperless-gpt";
  version = "0.31.0-unstable-2026-10-16";
  src = fetchFromGitHub {
    owner = "icereed";
    repo = "paperless-gpt";
    rev = "1d07cedfe83b2a06994d535d2c64e256e6fd8a3e";
    hash = "sha256-OFKmKotOMSrz/Z0KQUhfF7j0AcjN7YJh0gFN2ekrPrg=";
  };
  frontend = buildNpmPackage {
    pname = "${pname}-frontend";
    inherit version src;
    sourceRoot = "${src.name}/web-app";
    nodejs = nodejs_22;
    npmDepsHash = "sha256-6ZkO5pVQfJKYGpgUEKS/AXy/bfuot8cOJ2o+Z087HWM=";
    npm_config_ignore_scripts = "true";
    # The default React SWC plugin uses esbuild for production, but importing it
    # loads SWC's native cache loader, which rejects Nix's sandbox ownership.
    postPatch = ''
      substituteInPlace vite.config.ts \
        --replace-fail "import react from '@vitejs/plugin-react-swc';" "" \
        --replace-fail "plugins: [react()]," \
          'esbuild: { jsx: "automatic", jsxImportSource: "react", tsconfigRaw: { compilerOptions: { useDefineForClassFields: true } } },'
    '';
    installPhase = ''
      runHook preInstall
      cp -r dist "$out"
      runHook postInstall
    '';
  };
in
buildGo127Module (finalAttrs: {
  inherit pname version src;
  vendorHash = "sha256-PAGZjn8xOsEVNi+JbdgILmDLo1GjK4bBbri/gDKGEh0=";
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
    updateScript = writeShellScriptBin "update-paperless-gpt" ''
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
