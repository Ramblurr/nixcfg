{
  config,
  pkgs,
  ...
}: {
  imports = [
    #./pipewire-rnnoise.nix
    ./pipewire-wireplumber.nix
  ];
  config = {
    security.rtkit.enable = true;
    sound.enable = true;
    #nixpkgs.config.pulseaudio = true;
    hardware.pulseaudio.enable = pkgs.lib.mkForce false;

    environment.systemPackages = with pkgs; [
      alsa-utils #ignore for now cross-compile problem
      pipewire
      wireplumber
      pulseaudio
      pulsemixer
      libcamera
      pw-viz
    ];
    programs.dconf.enable = true;
    systemd.user.services.pipewire-pulse.path = [pkgs.pulseaudio];
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true; # ?
      pulse.enable = true;
      jack.enable = false;
      wireplumber.enable = true;
      lowLatency.enable = true;
      audio.enable = true;
    };
  };
}
