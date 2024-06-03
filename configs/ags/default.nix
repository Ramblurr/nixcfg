{
  inputs,
  writeShellScript,
  system,
  stdenv,
  cage,
  swww,
  esbuild,
  dart-sass,
  fd,
  fzf,
  brightnessctl,
  accountsservice,
  slurp,
  wf-recorder,
  wl-clipboard,
  wayshot,
  satty,
  hyprpicker,
  pavucontrol,
  networkmanager,
  gtk3,
  which,
  wlr-randr,
}:
let
  name = "ags-config";

  ags = inputs.ags.packages.${system}.default.override { extraPackages = [ accountsservice ]; };

  dependencies = [
    which
    dart-sass
    fd
    fzf
    brightnessctl
    swww
    inputs.matugen.packages.${system}.default
    slurp
    wf-recorder
    wl-clipboard
    wayshot
    satty
    hyprpicker
    pavucontrol
    networkmanager
    gtk3
    wlr-randr
  ];

  addBins = list: builtins.concatStringsSep ":" (builtins.map (p: "${p}/bin") list);

  greeter-inner = writeShellScript "greeter-inner" ''
    export PATH=$PATH:${addBins dependencies}
    ${wlr-randr}/bin/wlr-randr --output DP-1 --mode 5120x1440@59.977001Hz || true
    ${wlr-randr}/bin/wlr-randr --output DP-2 --mode 5120x1440@59.977001Hz || true
    ${wlr-randr}/bin/wlr-randr --output HDMI-A-1 --mode 3840x2160@60.000000Hz || true
    ${ags}/bin/ags -- -c ${config}/greeter.js
  '';
  greeter = writeShellScript "greeter" ''
     export PATH=$PATH:${addBins dependencies}
    ${cage}/bin/cage -ds -m last ${greeter-inner}
  '';

  desktop = writeShellScript name ''
    export PATH=$PATH:${addBins dependencies}
    ${ags}/bin/ags -b ${name} -c ${config}/config.js $@
  '';

  config = stdenv.mkDerivation {
    inherit name;
    src = ./.;

    buildPhase = ''
      ${esbuild}/bin/esbuild \
        --bundle ./main.ts \
        --outfile=main.js \
        --format=esm \
        --external:resource://\* \
        --external:gi://\* \

      ${esbuild}/bin/esbuild \
        --bundle ./greeter/greeter.ts \
        --outfile=greeter.js \
        --format=esm \
        --external:resource://\* \
        --external:gi://\* \
    '';

    installPhase = ''
      mkdir -p $out
      cp -r assets $out
      cp -r style $out
      cp -r greeter $out
      cp -r widget $out
      cp -f main.js $out/config.js
      cp -f greeter.js $out/greeter.js
    '';
  };
in
stdenv.mkDerivation {
  inherit name;
  src = config;

  installPhase = ''
    mkdir -p $out/bin
    cp -r . $out
    cp ${desktop} $out/bin/${name}
    cp ${greeter} $out/bin/greeter
  '';
}
