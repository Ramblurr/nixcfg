{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.nomad;
in
{
  options.modules.services.nomad = {
    enable = lib.mkEnableOption "nomad";
  };
  config = lib.mkIf cfg.enable {
    services.nomad = {
      enable = true;
      package = pkgs.nomad_1_9;
      extraSettingsPlugins = [ pkgs.nomad-driver-podman ];
      enableDocker = false;
      dropPrivileges = false; # root-less nomad is not well supported: https://github.com/hashicorp/nomad/issues/13669
      settings = {
        client.enabled = true;
        server = {
          enabled = true;
          bootstrap_expect = 1;
        };
        plugin = [
          {
            nomad-driver-podman = {
              config = {
                socket_path =
                  # Rootfull Nomad on Rootfull containers
                  #if !dockerEnabled
                  #then "unix://run/user/1000/podman/podman.sock"
                  #else "unix://run/podman/podman.sock";
                  "unix://run/podman/podman.sock";
              };
            };
          }
        ];
      };
    };

    #systemd.services.nomad.wantedBy = lib.mkForce [ ];

    environment.systemPackages = with pkgs; [
      nomad-driver-podman # Podman driver plugin
      cni-plugins # Networking plugins, needed for bridge. Might not be needed?
      damon # TUI for Nomad
    ];
  };
}
