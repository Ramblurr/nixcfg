{
  lib,
  appimageTools,
  fetchurl,
}:
let
  pname = "t3code";
  version = "0.0.15";
  src = fetchurl {
    url = "https://github.com/pingdotgg/t3code/releases/download/v${version}/T3-Code-${version}-x86_64.AppImage";
    hash = "sha256-Z8y7SWH55+ZC7cRpgo0cdG273rbDiFS3pXQt3up7sDg=";
  };
  appimageContents = appimageTools.extract {
    inherit pname version src;
  };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/t3-code-desktop.desktop -t $out/share/applications
    substituteInPlace $out/share/applications/t3-code-desktop.desktop \
      --replace-fail 'Exec=AppRun' 'Exec=${pname}'
    cp -r ${appimageContents}/usr/share/icons $out/share
  '';

  meta = {
    description = "Desktop GUI for coding agents";
    homepage = "https://github.com/pingdotgg/t3code";
    license = lib.licenses.mit;
    mainProgram = "t3code";
    platforms = [ "x86_64-linux" ];
  };
}
