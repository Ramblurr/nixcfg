# Living life on unstable can sometimes be... unstable
# This overlay is a way to pin down a known-good version of nixpkgs for certain packages
_final: prev:

{
  #inherit
  #  (nixpkgs {
  #    rev = "5e35f8875cfa5871cf7af29575dd9186b615a314";
  #    sha256 = "sha256-WOxcqjpSEh8tUGUNJ6CSal4qGCDWruKhBPvPF0sZYcg=";
  #  })
  #  authentik
  #  ;

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
