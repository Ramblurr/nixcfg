{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ovos-kitchen";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ../../../home-wifi.nix
    ../pipewire.nix
    ../hivemindsat.nix
    inputs.nixos-raspberrypi.nixosModules.base
    inputs.nixos-raspberrypi.nixosModules.hardware
    inputs.nixos-raspberrypi.inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  system.stateVersion = "23.11";
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "feedb33df4cc4cbf8e64e91cf837d8b2";

  raspberry-pi.hardware.hifiberry-dacplusadc.enable = true;
  raspberry-pi.hardware.platform.type = "rpi4";
  modules = {
    networking.default.enable = true;
    networking.default.hostName = hn;
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
    };
    hardware = {
      udisks2.enable = false;
      fwupd.enable = false;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    users.enable = true;
    users.primaryUser = {
      username = ramblurr.username;
      uid = 1001;
      name = ramblurr.name;
      homeDirectory = ramblurr.homeDirectory;
      signingKey = ramblurr.signingKey;
      email = ramblurr.email;
      passwordSecretKey = ramblurr.passwordSecretKey;
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
      ];
    };
  };
  myhm = {
    systemd.user.startServices = lib.mkForce false;
  };

  #ovos.password.enable = true;
  #ovos.gui.enable = false;
  #ovos.sshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzseqIeUgemCgd3/vxkcmJtpVGFS1P3ajBDYaGHHwziIUO/ENkWrEfv/33DvaaY3QQYnSMePRrsHq5ESanwEdjbMBu1quQZZWhyh/M5rQdbfwFoh2BYjCq5hFhaNUl9cjZk3xjQGHVKlTBdFfpuvWtY9wGuh1rf/0hSQauMrxAZsgXVxRhCbY+/+Yjjwm904BrWxXULbrc5yyfpgwHOHhHbpl8NIQIN6OAn3/qcVb7DlGJpLUjfolkdBTY8zGAJxEWecJzjgwwccuWdrzcWliuw0j4fu/MDOonpVQBCY9WcZeKInGHYAKu+eZ/swxAP+9vAR4mc+l/SBYyzCWvM6zG8ebbDK1mkwq2t0G183/0KSxAPJ7OykFD1a/ifb+cXNYJjshCDN+M95A3s6aMEU4VER/9SmQp3YCZvQEDKOBHlqMqlbw0IYAYE/FfU2se+gLI74JizoHBv2OJcduYdV0Ba97fvrb1lYM+tg0VmKUCwCvI9+ZbT2bJH3sM6SE9xt8+3nx6sKzV6h6FlpvDC60Rr2mANsuW3lbqac05Wnmxzk0C8OoJPCqWEmzjyWLJvPq98cG4obJiNlnp7/7xmmhOwyqcy7gDQum1QDwrUJyBKBsJPelJOZJC0pKkerv4LdSZDTSxEVxomstK/WDzmkPK9uUWTEH69VU/bUMuejTNVQ== cardno:000500006944";
  #services.ovos.services = {
  #  skill_roon = {
  #    enable = true;
  #    image = "ghcr.io/ramblurr/ovos-skill-roon";
  #    tag = "dev";
  #    requires = ["ovos_core"];
  #  };
  #};
}
# pw-link "roc-recv-source:receive_FR" "control.endpoint.speech:playback_FR"
# pw-link "roc-recv-source:receive_FL" "control.endpoint.speech:playback_FL"
# pw-link "output.music-to-speakers-bridge:output_FR" "control.endpoint.multimedia:playback_FR"
# pw-link "output.music-to-speakers-bridge:output_FL" "control.endpoint.multimedia:playback_FL"
# pw-link -d roc-recv-source:receive_FL input.music-to-speakers-bridge:input_FL
# pw-link -d roc-recv-source:receive_FR input.music-to-speakers-bridge:input_FR
# sudo podman images | awk '(NR>1) && ($2!~/none/) {print $1":"$2}' |grep docker.io | xargs -L1 sudo podman pull

