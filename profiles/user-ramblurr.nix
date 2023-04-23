{config, ...}: let
  user = "ramblurr";
  name = "Ramblurr";
  idno = 1000;
in {
  config = {
    nix.settings.trusted-users = [user];

    users.extraGroups."${user}".gid = idno;
    users.extraUsers."${user}" = {
      isNormalUser = true;
      home = "/home/${user}";
      description = name;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzseqIeUgemCgd3/vxkcmJtpVGFS1P3ajBDYaGHHwziIUO/ENkWrEfv/33DvaaY3QQYnSMePRrsHq5ESanwEdjbMBu1quQZZWhyh/M5rQdbfwFoh2BYjCq5hFhaNUl9cjZk3xjQGHVKlTBdFfpuvWtY9wGuh1rf/0hSQauMrxAZsgXVxRhCbY+/+Yjjwm904BrWxXULbrc5yyfpgwHOHhHbpl8NIQIN6OAn3/qcVb7DlGJpLUjfolkdBTY8zGAJxEWecJzjgwwccuWdrzcWliuw0j4fu/MDOonpVQBCY9WcZeKInGHYAKu+eZ/swxAP+9vAR4mc+l/SBYyzCWvM6zG8ebbDK1mkwq2t0G183/0KSxAPJ7OykFD1a/ifb+cXNYJjshCDN+M95A3s6aMEU4VER/9SmQp3YCZvQEDKOBHlqMqlbw0IYAYE/FfU2se+gLI74JizoHBv2OJcduYdV0Ba97fvrb1lYM+tg0VmKUCwCvI9+ZbT2bJH3sM6SE9xt8+3nx6sKzV6h6FlpvDC60Rr2mANsuW3lbqac05Wnmxzk0C8OoJPCqWEmzjyWLJvPq98cG4obJiNlnp7/7xmmhOwyqcy7gDQum1QDwrUJyBKBsJPelJOZJC0pKkerv4LdSZDTSxEVxomstK/WDzmkPK9uUWTEH69VU/bUMuejTNVQ== cardno:000500006944"
        "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBGIiONt3u8BdzaI/ndZLXuDnFBXwxbQ7Nr+Yq4BQNBuri49dknMrwjkPIqsUVUFo+hrfqEYnXlBLaKdof1ZpBdwwY2yHUO01EO18fIFkCswAoiHFf7TZO3a1Ekhnmuc1nw== JuiceSSH"
        "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJ6XVIQ8DU60p0cjBti+kJgd/UqM1tV4M5gFIoR+I0tQ5XmWU65V91uxfeJMBG0Owoweod2q1qKhH3xic9tIHHA= casey@ipad"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF7TTPusOHuyKotZBcwWqIH3B+cRRDQM46+lvATGNAWO disaster recovery 2021"
      ];
      passwordFile = config.sops.secrets.ramblurr-password.path;
      extraGroups = [
        "wheel"
        "kvm"
        "libvirtd"
        "qemu-libvirtd"
        "docker"
        "audio"
        "video"
        "sound"
        "pulse"
        "input"
        "render"
        "dialout"
        "keys"
        "ipfs"
        "plugdev"
        "networkmanager"
        "scard"
        "tty"
        "users"
        "netdev" # actually networkctl
        "lxd" # lxd lxc waydroid
      ];
      uid = idno;
      group = user;
    };
    sops.secrets.ramblurr-password = {
      sopsFile = ../secrets/secrets.sops.yaml;
      neededForUsers = true;
    };
  };
}
