{
  config,
  lib,
  ...
}:

let
  inherit (config.networking) hostName;
  inherit (config.repo.secrets.global) domain;
  inherit (config.modules.users.primaryUser) username uid;
  cfg = config.modules.microvm-guest;
  SOCKLOCATION = "/var/lib/microvms/${hostName}/terminal.sock";
  mkShare = source: dir: {
    inherit source;
    tag = builtins.replaceStrings [ "/" ] [ "_" ] dir;
    mountPoint = dir;
    proto = "virtiofs";
  };
  shareHomeDir = source: (mkShare "/home/${username}/${source}" "/home/${username}/${source}");
  shareDir =
    dir:
    if lib.isString dir then
      if lib.hasPrefix "/" dir then (mkShare dir dir) else shareHomeDir dir
    else
      dir;
in
lib.mkIf cfg.devSandbox.enable {
  microvm = {
    hypervisor = "qemu";
    qemu = {
      extraArgs = [
        "-serial"
        "unix:${SOCKLOCATION},server,nowait"
      ];
      serialConsole = false;
    };
    shares = map shareDir cfg.devSandbox.sharedDirs;
  };

  modules.users.enable = true;
  modules.users.root.enable = false;
  modules.users.primaryUser.password.enable = false;
  modules.microvm-guest = {
    bootstrapSops.enable = true;
    writableStoreOverlay.enable = true;
    #homeManager = {
    #  enable = true;
    #  username = "${username}";
    #  uid = 1000;
    #  gid = 1000;
    #};
  };
  # sudo socat STDIO,cfmakeraw,escape=0x1D unix:/var/lib/microvms/claude-test/terminal.sock
  #  microvm.qemu = {
  #    extraArgs = [
  #      "-serial"
  #      "unix:${SOCKLOCATION},server,nowait"
  #    ];
  boot.kernelParams = [
    "console=ttyS0,38400n8"
    "earlyprint=serial,ttyS0,38400n8"
  ];
  nix = {
    extraOptions = "experimental-features = nix-command flakes";
  };
  networking.hosts = {
    "172.20.20.3" = [
      "dewey.prim.${domain.home}"
      "dewey"
    ];
  };
  security = {
    sudo.enable = true;
    sudo.wheelNeedsPassword = false;
    sudo.extraRules =
      let
        # systemPath is the path where the system being activated is uploaded by `deploy`.
        nopasswd = command: {
          inherit command;
          options = [
            "NOPASSWD"
            "SETENV"
          ];
        };
      in
      [
        {
          groups = [ "wheel" ];
          runAs = "root";
          commands = [ (nopasswd "/run/current-system/sw/bin/systemctl reboot") ];
        }
      ];
  };

  users.users.${username} = {
    openssh.authorizedKeys.keys = config.repo.secrets.global.pubKeys;
    extraGroups = [
      "wheel"
      "docker"
      "video"
      "audio"
      "networkmanager"
    ];
    createHome = true;
    autoSubUidGidRange = true;
    linger = true;
  };
  services.getty.autologinUser = "${username}";

  systemd.tmpfiles.rules = [
    "d /home/${username}/src 0750 ${username} ${username}"
    "d /home/${username}/.config 0750 ${username} ${username}"
    "d /home/${username}/.cache 0750 ${username} ${username}"
    "d /home/${username}/.local 0750 ${username} ${username}"
    "d /home/${username}/.local/share 0750 ${username} ${username}"
    "d /home/${username}/.local/state 0750 ${username} ${username}"
  ];

  modules.shell.atuin = {
    enable = true;
    autoLogin.enable = false;
    syncing.enable = false;
  };
  modules.shell = {
    zsh.enable = true;
    zsh.starship.enable = false;
    zsh.powerlevel10k.enable = false;
  };

  networking.firewall.allowedTCPPorts = [
    8080
    8000
    3000
    3001
  ];
  home-manager.users.${username} =
    { pkgs, ... }:
    {
      #systemd.user.startServices = "sd-switch";
      systemd.user.startServices = lib.mkForce true;
      manual.manpages.enable = true;
      programs = {
        home-manager.enable = true;
      };
      home.sessionVariables = {
        TERM = "xterm-256color";
        EDITOR = "vim";
        DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString uid}/bus";
        XDG_RUNTIME_DIR = "/run/user/${toString uid}";
      };
      #programs.zsh = {
      #  shellAliases = {
      #    "claude" = "claude --dangerously-skip-permissions";
      #    "start-local" = "tmux new-session -d -s claude-session 'claude --dangerously-skip-permissions'";
      #    "start" =
      #      "ttyd -p 8080 --writable -t titleFixed=$(hostname) -t fontSize=32 -t theme=dark -t enableSixel=true tmux attach-session -t claude-session";
      #    "attach" = "tmux attach-session -t claude-session";
      #  };
      #};
      home.packages = with pkgs; [
        strace
        #claude-code
        age
        babashka
        bandwhich
        difftastic
        doggo
        dust
        fd
        file
        findutils
        fzf
        glow
        gost
        go-task
        htop
        httpie
        iotop
        jet
        jless
        jq
        just
        lsof
        mosh
        ncdu
        nix-du
        nix-output-monitor
        nix-prefetch
        nix-tree
        p7zip
        rclone
        repomix
        rsync
        sops
        tmux
        tree
        treefmt
        ttyd
        unar
        unzip
        usbutils
        vifm
        watchexec
        watchman
        wget
        which
        zip
      ];
    };
}
