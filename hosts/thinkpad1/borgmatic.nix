{ lib, ... }:
{
  modules.services.borgmatic = {
    enable = true;
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
      "sh:home/.snapshots"
      "sh:home/*/OneDrive"
      "sh:home/*/.cache"
      "sh:home/*/.local/share/Trash"
      "sh:home/*/.local/share/flatpak/repo"
      "sh:home/*/.local/share/flatpak/app"
      "sh:home/*/.local/share/flatpak/runtime"
      "sh:home/*/.var/app/*/cache"
    ];
  };

  # This host has a persistent root, no impermanence here.
  services.borgmatic.settings.source_directories = lib.mkForce [
    "/home"
    "/etc"
  ];
}
