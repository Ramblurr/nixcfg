{ lib, ... }:
{
  modules.services.borgmatic = {
    # Enable after provisioning both repositories and adding the SOPS credentials.
    enable = false;
    name = "thinkpad1";
    repositories = [
      {
        label = "mali";
        path = "\${NAS_REPOSITORY}";
      }
      {
        label = "offsite2";
        path = "\${OFFSITE_REPOSITORY2}";
      }
    ];
    exclude-patterns = [
      "sh:home/*/.cache"
      "sh:home/*/.local/share/Trash"
      "sh:home/*/.local/share/flatpak/repo"
      "sh:home/*/.local/share/flatpak/app"
      "sh:home/*/.local/share/flatpak/runtime"
      "sh:home/*/.var/app/*/cache"
    ];
  };

  # This host has a persistent root, not quine's /persist layout.
  services.borgmatic.settings.source_directories = lib.mkForce [
    "/home"
    "/etc"
  ];
}
