# Living life on unstable can sometimes be... unstable
# This overlay is a way to pin down a known-good version of nixpkgs for certain packages
_final: prev:

{
  # TODO: Remove once nixpkgs includes https://github.com/maralorn/nix-output-monitor/pull/321.
  nix-output-monitor = prev.nix-output-monitor.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (prev.fetchpatch {
        url = "https://github.com/maralorn/nix-output-monitor/compare/c40cb120262d9e9b1b988898708b124f6ffc0913..d48ff7935da72a5a5764ee8c5358d38417b0f337.diff";
        stripLen = 1;
        hash = "sha256-JPC3AXycs5oTXe9J1g7DWNBEYYr60AM78R3sajP9cgo=";
      })
    ];
  });

  # TODO: Remove once pyopenjtalk-plus accepts Cython's newer TypeError wording.
  pythonPackagesExtensions = (prev.pythonPackagesExtensions or [ ]) ++ [
    (_pythonFinal: pythonPrev: {
      pyopenjtalk-plus = pythonPrev.pyopenjtalk-plus.overridePythonAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace tests/test_frontend.py \
            --replace-fail 'match="must be str"' 'match="must be str|Expected str"'
        '';
      });
    })
  ];

  # TODO: Remove once https://github.com/NixOS/nixpkgs/pull/575801 reaches our pin.
  open-webui = prev.open-webui.overridePythonAttrs {
    makeWrapperArgs = [
      "--set"
      "FRONTEND_BUILD_DIR"
      "${prev.open-webui.frontend}/share/open-webui"
    ];
  };

  # TODO: Remove once https://github.com/NixOS/nixpkgs/pull/576411 reaches our pin.
  # Qt 6.12's generated QML code must be excluded from MuseScore's unity builds.
  musescore = prev.musescore.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (prev.fetchurl {
        url = "https://raw.githubusercontent.com/NixOS/nixpkgs/1fb46d447369b06d38d12325b4bca4310ad2cfe5/pkgs/by-name/mu/musescore/qmlcachegen-unity-build.diff";
        hash = "sha256-cYZ5bh+BV81M+ycG2YUmRifBaFZMBF2LCf7+UpIWxT8=";
      })
    ];
  });

  # Quine's running CUDA/ONNX stack; these exact outputs are available in Flox.
  # TODO: wait for https://github.com/NixOS/nixpkgs/pull/575979
  # (CUDA patchelf workaround) only after verifying the exact CUDA/ONNX outputs and
  # their full closures are available in https://cache.flox.dev.
  # Root-cause fix: https://github.com/NixOS/patchelf/pull/672.
  cudaKnownGood =
    import
      (prev.fetchFromGitHub {
        owner = "ramblurr";
        repo = "nixpkgs";
        rev = "1eb89746d905ef0991bd6c776c6e2018adc66fb3";
        hash = "sha256-6hpi98s1lW47sN846OUQY+WPV25VbdFW03ii3LMwq3M=";
      })
      {
        inherit (prev.stdenv.hostPlatform) system;
        config = {
          allowUnfree = true;
          cudaSupport = true;
        };
      };

  # Standalone ghostty terminfo derivation that avoids building the full
  # ghostty GUI app (which pulls in gtk4, libadwaita, gstreamer, etc.).
  # Parses the terminfo definition from ghostty's Zig source and compiles
  # it with tic.
  ghostty-terminfo =
    prev.runCommand "ghostty-terminfo-${prev.ghostty.version}"
      {
        nativeBuildInputs = [ prev.ncurses ];
      }
      ''
        ${prev.python3.interpreter} ${prev.writeText "gen-ghostty-ti.py" ''
          import re, sys
          with open(sys.argv[1]) as f:
              content = f.read()
          names_match = re.search(r'\.names\s*=\s*&\.\{(.*?)\}', content, re.DOTALL)
          names = re.findall(r'"([^"]+)"', names_match.group(1))
          caps = []
          for line in content.split('\n'):
              m = re.search(r'\.name\s*=\s*"([^"]+)"', line)
              if not m:
                  continue
              name = m.group(1)
              if '.boolean' in line:
                  caps.append(f"\t{name},")
              elif '.canceled' in line:
                  caps.append(f"\t{name}@,")
              elif '.numeric' in line:
                  nm = re.search(r'\.numeric\s*=\s*(\d+)', line)
                  caps.append(f"\t{name}#{nm.group(1)},")
              elif '.string' in line:
                  sm = re.search(r'\.string\s*=\s*"([^"]*)"', line)
                  s = sm.group(1).replace('\\\\', '\\')
                  caps.append(f"\t{name}={s},")
          print('|'.join(names) + ',')
          print('\n'.join(caps))
        ''} ${prev.ghostty.src}/src/terminfo/ghostty.zig > ghostty.ti
        mkdir -p $out/share/terminfo
        tic -x -o $out/share/terminfo ghostty.ti
      '';
}
