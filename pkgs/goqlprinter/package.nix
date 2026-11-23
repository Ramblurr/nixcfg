{
  lib,
  buildGoModule,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_22,
}:
let
  pname = "goqlprinter";
  version = "1.1.1";
  src = fetchFromGitHub {
    owner = "mhavo";
    repo = "goqlprinter";
    tag = "v${version}";
    hash = "sha256-MbCY9CxXjp/LNJm9IsIzNBr3S1P2W3nfdkG5EfAIc1s=";
  };
  frontend = buildNpmPackage {
    pname = "${pname}-frontend";
    inherit version src;
    sourceRoot = "${src.name}/frontend";
    nodejs = nodejs_22;
    npmDepsHash = "sha256-pd3EebpPb7DGVuy2Jszxj1vEgcrClQzLPCiMNM9uopk=";
    npm_config_ignore_scripts = "true";
    installPhase = ''
      runHook preInstall
      cp -r dist "$out"
      runHook postInstall
    '';
  };
in
buildGoModule {
  inherit pname version src;
  patches = [ ./tcp-backend.patch ];
  vendorHash = "sha256-7fpaFgUyrqQ3C6vHE0rlmt2WowmSUrYVwwf0gGaMLWo=";
  subPackages = [ "." ];
  # Default tags select the pure-Go native backend; `usb` (gousb) and `gui`
  # (Wails) stay off.
  env.CGO_ENABLED = 0;
  ldflags = [
    "-s"
    "-w"
  ];
  # subPackages limits the default check to the root package, which has no tests.
  checkPhase = ''
    runHook preCheck
    go test ./... -count=1
    runHook postCheck
  '';
  preBuild = ''
    rm -rf frontend/dist
    cp -r ${frontend} frontend/dist
  '';
  postInstall = ''
    mkdir -p "$out/share/goqlprinter"
    cp -r fonts "$out/share/goqlprinter/"
  '';
  passthru = { inherit frontend; };
  meta = {
    description = "Web UI, API and CLI for Brother QL label printers";
    homepage = "https://github.com/mhavo/goqlprinter";
    license = lib.licenses.mit;
    mainProgram = "goqlprinter";
    platforms = lib.platforms.linux;
  };
}
