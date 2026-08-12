{
  lib,
  fetchFromGitHub,
  git,
  nix,
  nix-update,
  openssl,
  pkgs-lib,
  pkg-config,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "hindsight-cli";
  version = "0.9.0";

  src = fetchFromGitHub {
    owner = "vectorize-io";
    repo = "hindsight";
    rev = "v${version}";
    hash = "sha256-o74lzvW15bekJKoRTMvi5tM5EoTjh+DlIgKqo2fVohk=";
  };

  cargoRoot = "hindsight-cli";
  buildAndTestSubdir = "hindsight-cli";
  cargoLock.lockFile = ./Cargo.lock;

  postPatch = ''
    cp ${./Cargo.lock} hindsight-cli/Cargo.lock
    substituteInPlace hindsight-cli/src/config.rs \
      --replace-fail \
        'dirs::home_dir().map(|home| home.join(CONFIG_DIR_NAME))' \
        'env::var_os("HINDSIGHT_CONFIG_DIR").map(PathBuf::from).or_else(|| dirs::home_dir().map(|home| home.join(CONFIG_DIR_NAME)))'
  '';

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ openssl ];

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "hindsight-cli";
    utils = [
      git
      nix
      nix-update
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Command-line interface for the Hindsight memory system";
    homepage = "https://github.com/vectorize-io/hindsight";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.ramblurr ];
    mainProgram = "hindsight";
  };
}
