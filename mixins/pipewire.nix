{
  config,
  pkgs,
  ...
}: {
  security.rtkit.enable = true;
  sound.enable = true;
  #nixpkgs.config.pulseaudio = true;
  hardware.pulseaudio.enable = pkgs.lib.mkForce false;

  environment.systemPackages = with pkgs; [
    alsa-utils #ignore for now cross-compile problem
    pipewire
    pulseaudio
    pulsemixer
    rnnoise-plugin
    pw-viz
  ];
  programs.dconf.enable = true;
  systemd.user.services.pipewire-pulse.path = [pkgs.pulseaudio];
  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    home.file.".config/pipewire/pipewire.conf.d/99-input-denoising.conf" = {
      source = ../configs/pipewire.conf;
      force = true;
      recursive = true;
    };
  };
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true; # ?
    pulse.enable = true;
    jack.enable = false;

    # media-session.config.bluez-monitor.rules = [
    #   {
    #     # Matches all cards
    #     matches = [ { "device.name" = "~bluez_card.*"; } ];
    #     actions = {
    #       "update-props" = {
    #         "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
    #         # mSBC is not expected to work on all headset + adapter combinations.
    #         "bluez5.msbc-support" = true;
    #         # SBC-XQ is not expected to work on all headset + adapter combinations.
    #         "bluez5.sbc-xq-support" = true;
    #       };
    #     };
    #   }
    #   {
    #     matches = [
    #       # Matches all sources
    #       { "node.name" = "~bluez_input.*"; }
    #       # Matches all outputs
    #       { "node.name" = "~bluez_output.*"; }
    #     ];
    #     actions = {
    #       "node.pause-on-idle" = false;
    #     };
    #   }
    # ];
  };
}
