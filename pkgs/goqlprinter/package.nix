{
  lib,
  buildGoModule,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  nodejs_22,
  resvg,
}:
let
  pname = "goqlprinter";
  version = "1.1.1-unstable-2026-11-23";
  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "goqlprinter";
    rev = "9d5f4d147ff4ccfff4c73e187731378ab3deebba";
    hash = "sha256-DU8pdOqu24tjOepiUxJ0wjMhU0DMFwIG9jL4PpINado=";
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
  vendorHash = "sha256-RCvU6TEfKcg+v9s46WMao0gHUuCeWHdSDvR4y1Q8WPw=";
  subPackages = [ "." ];
  nativeBuildInputs = [ makeWrapper ];
  nativeCheckInputs = [ resvg ];
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
    wrapProgram "$out/bin/goqlprinter" --prefix PATH : ${lib.makeBinPath [ resvg ]}
  '';
  passthru = { inherit frontend; };
  meta = {
    description = "Web UI, API and CLI for Brother QL label printers";
    homepage = "https://github.com/Ramblurr/goqlprinter";
    license = lib.licenses.mit;
    mainProgram = "goqlprinter";
    platforms = lib.platforms.linux;
  };
}
