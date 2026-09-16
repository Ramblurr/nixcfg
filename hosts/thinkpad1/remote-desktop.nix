{ lib, pkgs, ... }:
{
  # Plasma already installs KRDP. tailscale0 is trusted by our Tailscale module;
  # do not open 3389 globally (in particular, not on the laptop's Wi-Fi).

  myhm =
    { config, ... }:
    let
      certificateDir = "${config.xdg.dataHome}/krdpserver";
    in
    {
      xdg.configFile."krdpserverrc".text = ''
        [General]
        SystemUserEnabled=true
        ListenPort=3389
        Certificate=${certificateDir}/krdp.crt
        CertificateKey=${certificateDir}/krdp.key
      '';
      systemd.user.services."app-org.kde.krdpserver" = {
        Unit = {
          Description = "KRDP desktop sharing over Tailscale";
          After = [
            "plasma-xdg-desktop-portal-kde.service"
            "plasma-core.target"
          ];
          PartOf = [ "plasma-workspace.target" ];
        };
        Service = {
          Type = "exec";
          ExecStartPre = lib.getExe (
            pkgs.writeShellApplication {
              name = "krdp-prepare";
              runtimeInputs = [
                pkgs.flatpak
                pkgs.openssl
              ];
              text = ''
                flatpak permission-set kde-authorized remote-desktop org.kde.krdpserver yes
                umask 077
                mkdir -p ${lib.escapeShellArg certificateDir}
                if [ ! -s ${lib.escapeShellArg "${certificateDir}/krdp.key"} ] || [ ! -s ${lib.escapeShellArg "${certificateDir}/krdp.crt"} ]; then
                  openssl req -new -x509 -nodes -days 3650 -subj /CN=thinkpad1 \
                    -keyout ${lib.escapeShellArg "${certificateDir}/krdp.key"} \
                    -out ${lib.escapeShellArg "${certificateDir}/krdp.crt"}
                fi
              '';
            }
          );
          ExecStart = "${pkgs.kdePackages.krdp}/bin/krdpserver";
          Restart = "on-abnormal";
        };
        Install.WantedBy = [ "plasma-workspace.target" ];
      };
    };
}
