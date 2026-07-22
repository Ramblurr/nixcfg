{
  lib,
  stdenv,
  autoPatchelfHook,
  python3Packages,
  fetchFromGitHub,
  fetchPypi,
  ...
}:

let
  # pymicro-features v2.0.2 as required by pymicro-wakeword
  pymicro-features = python3Packages.buildPythonPackage rec {
    pname = "pymicro-features";
    version = "2.0.2";
    pyproject = true;

    src = fetchPypi {
      pname = "pymicro_features";
      inherit version;
      hash = "sha256-DQvteEPseLbO2C0aLc3etP5d9hs6+AooHQhoyOJ5xyc=";
    };

    build-system = [
      python3Packages.setuptools
      python3Packages.pybind11
    ];

    dependencies = [ python3Packages.numpy ];

    pythonImportsCheck = [ "pymicro_features" ];

    meta = {
      description = "Audio feature extraction for wake word detection";
      homepage = "https://github.com/rhasspy/pymicro-features";
      license = lib.licenses.asl20;
    };
  };

  # pymicro-wakeword v2.1.0
  # Must use wheel to get bundled libtensorflowlite_c.so for each platform
  pymicro-wakeword = python3Packages.buildPythonPackage rec {
    pname = "pymicro-wakeword";
    version = "2.1.0";
    format = "wheel";

    src = fetchPypi {
      pname = "pymicro_wakeword";
      inherit version;
      format = "wheel";
      dist = "py3";
      python = "py3";
      abi = "none";
      platform =
        if stdenv.hostPlatform.isAarch64 then "manylinux_2_35_aarch64" else "manylinux_2_35_x86_64";
      hash =
        if stdenv.hostPlatform.isAarch64 then
          "sha256-XueUjgQKvouj527V5ke47+NGN7e0hsgITSGDWI+UC8Y="
        else
          "sha256-VZlPhQN4LXskrn0Fzl5jiu3S1MV36KOIeMyJSg2wyus=";
    };

    nativeBuildInputs = [ autoPatchelfHook ];
    buildInputs = [ stdenv.cc.cc.lib ];

    dependencies = [
      python3Packages.numpy
      pymicro-features
    ];

    pythonImportsCheck = [ "pymicro_wakeword" ];

    meta = {
      description = "Lightweight wake word detection using TensorFlow Lite";
      homepage = "https://github.com/rhasspy/pymicro-wakeword";
      license = lib.licenses.asl20;
    };
  };

  # aioesphomeapi 45.3.1 requires zeroconf 0.149.16, which is newer than
  # the version in stable nixpkgs.
  zeroconf = python3Packages.zeroconf.overridePythonAttrs rec {
    version = "0.149.16";

    src = fetchFromGitHub {
      owner = "jstasiak";
      repo = "python-zeroconf";
      rev = version;
      hash = "sha256-l/F+Cz0HEtsgfQj01ayl+FQYoQbZVpMfRhNs27BqThI=";
    };
  };

  # LVA 1.1.13 uses ESPHome's multi-channel audio API from aioesphomeapi 45.3.1.
  # Pin it here because stable nixpkgs still carries the incompatible 44.x API.
  aioesphomeapi = python3Packages.aioesphomeapi.overridePythonAttrs rec {
    version = "45.3.1";

    src = fetchFromGitHub {
      owner = "esphome";
      repo = "aioesphomeapi";
      rev = "v${version}";
      hash = "sha256-+8P6OL+4Y+qrKLYqXtjBL2ylcamsF24Ccn00Vt9ohD0=";
    };

    postPatch = ''
      substituteInPlace pyproject.toml \
        --replace-fail "setuptools>=82.0.1" setuptools \
        --replace-fail "Cython>=3.2.5" Cython
    '';

    pythonRelaxDeps = [
      "aiohappyeyeballs"
      "cryptography"
    ];

    dependencies = [
      python3Packages.aiohappyeyeballs
      python3Packages.async-interrupt
      python3Packages.chacha20poly1305-reuseable
      python3Packages.cryptography
      python3Packages.noiseprotocol
      python3Packages.protobuf
      python3Packages.tzdata
      python3Packages.tzlocal
      zeroconf
    ];
  };

  # Override pyopen-wakeword to disable tests on aarch64
  # The tests use TensorFlow Lite which crashes under QEMU emulation.
  # On native aarch64 the binary cache is used anyway.
  pyopen-wakeword = python3Packages.pyopen-wakeword.overridePythonAttrs {
    doCheck = !stdenv.hostPlatform.isAarch64;
  };

in
python3Packages.buildPythonPackage rec {
  pname = "linux-voice-assistant";
  version = "1.1.13";
  pyproject = true;

  disabled = python3Packages.pythonOlder "3.11";

  src = fetchFromGitHub {
    owner = "OHF-Voice";
    repo = "linux-voice-assistant";
    rev = "v${version}";
    hash = "sha256-1M1INDXhJJvkDhZD96yMe7xsuxmHELs6sMQ9fdd18Q0=";
  };

  # Upstream output-only mode still imports SoundCard and opens a microphone.
  patches = [ ./linux-voice-assistant-output-only.patch ];

  build-system = [
    python3Packages.setuptools
    python3Packages.setuptools-scm
  ];

  pythonRelaxDeps = [
    "getmac"
    "netifaces2"
    "numpy"
    "pymicro-wakeword"
    "pyopen-wakeword"
    "mpv"
    "soundcard"
    "websockets"
    "webrtc-noise-gain"
    "zeroconf"
  ];

  postPatch = ''
    # Match the distribution name used by nixpkgs' Python MPV package.
    substituteInPlace pyproject.toml \
      --replace-fail '"python-mpv>=1,<2"' '"mpv"'

    # GitHub source archives do not contain the Git metadata setuptools-scm uses.
    cat >> pyproject.toml << EOF

    [tool.setuptools_scm]
    fallback_version = "${version}"
    EOF

    # Read-only data is installed below; writable paths come from CLI arguments.
    substituteInPlace linux_voice_assistant/__main__.py \
      --replace-fail '_REPO_DIR = _MODULE_DIR.parent' '_REPO_DIR = Path("@out@/share/linux-voice-assistant")'
  '';

  postInstall = ''
    # Install data files
    mkdir -p $out/share/linux-voice-assistant
    cp -r wakewords $out/share/linux-voice-assistant/
    cp -r sounds $out/share/linux-voice-assistant/

    # Substitute the actual output path
    substituteInPlace $out/lib/python*/site-packages/linux_voice_assistant/__main__.py \
      --replace-fail '@out@' "$out"
  '';

  dependencies = [
    aioesphomeapi
    python3Packages.soundcard
    python3Packages.numpy
    zeroconf
    pyopen-wakeword
    python3Packages.mpv
    pymicro-wakeword
    python3Packages.netifaces2
    python3Packages.getmac
    python3Packages.types-protobuf
    python3Packages.websockets
    python3Packages.webrtc-noise-gain
  ];

  nativeCheckInputs = [
    python3Packages.pytest-asyncio
    python3Packages.pytestCheckHook
  ];

  # TensorFlow Lite crashes when these tests run under aarch64 QEMU emulation.
  doCheck = !stdenv.hostPlatform.isAarch64;

  pythonImportsCheck = [ "linux_voice_assistant" ];

  meta = {
    changelog = "https://github.com/OHF-Voice/linux-voice-assistant/releases/tag/v${version}";
    description = "Linux voice assistant for Home Assistant using the ESPHome protocol";
    homepage = "https://github.com/OHF-Voice/linux-voice-assistant";
    license = lib.licenses.asl20;
    mainProgram = "linux-voice-assistant";
  };
}
