{
  lib,
  stdenv,
  fetchurl,
  installShellFiles,
  ncurses,
  pam,
  pkgs-lib,
  git,
  nix,
}:

let
  version = "0.1.0-preview.22";
  rev = "a086db17db8d464627c77e3bb0be6d462668b0ac";
  hashes = {
    x86_64-linux = "sha256-kO+ILHPVyPoVRYbODMNqg1pvC4DBUOs4080SexhHbjw=";
    aarch64-linux = "sha256-/YrzcrYRTwuDL0I/vs2ewUoLqwRRt6IAOyr5pJeBXlw=";
  };
in
stdenv.mkDerivation {
  pname = "rex";
  inherit version;

  src = fetchurl {
    url = "https://blob.superlogical.com/download/rex/preview/${rev}/rex-linux-${stdenv.hostPlatform.go.GOARCH}.tar.gz";
    hash = hashes.${stdenv.hostPlatform.system};
  };
  sourceRoot = ".";

  nativeBuildInputs = [
    installShellFiles
    ncurses
  ];

  dontConfigure = true;
  dontBuild = true;
  # Rex dlopens libpam by name, so shrinking the rpath would drop it.
  dontPatchELF = true;
  # Stripping the patched binary breaks its symbol versions.
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 rex $out/bin/rex
    # Set the rpath before the interpreter: the other order corrupts this binary (NixOS/patchelf#674).
    patchelf --set-rpath ${lib.makeLibraryPath [ pam ]} $out/bin/rex
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/bin/rex

    export HOME=$TMPDIR
    rex="$out/bin/rex --autostart=false"
    $rex terminfo show > xterm-rex.src
    mkdir -p $out/share/terminfo
    tic -x -o $out/share/terminfo xterm-rex.src
    installShellCompletion --cmd rex \
      --bash <($rex completion bash) \
      --fish <($rex completion fish) \
      --zsh <($rex completion zsh)

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    TERMINFO_DIRS=$out/share/terminfo infocmp xterm-rex > /dev/null
    $out/bin/rex --autostart=false --help > /dev/null
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "rex";
    utils = [
      git
      nix
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Rex terminal multiplexer by Superlogical (preview channel)";
    homepage = "https://superlogical.com";
    license = lib.licenses.unfree;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = builtins.attrNames hashes;
    mainProgram = "rex";
  };
}
