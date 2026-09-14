{
  config,
  inputs,
  lib,
  pkgs,
  unstable,
  ...
}:
let
  chatgpt = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.chatgpt;
  # Dedicated 1Password SSH identity; authorized on this laptop only.
  laptopSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEnOBbCW8iP5o45Jc0Y3jpl2Xg9nd6G0SzQzglMBFJUk";
  # Choose latte, frappe, macchiato, or mocha.
  catppuccinVariant = "latte";
  # Keep the boot passphrase prompt on a dark background, including with Latte KDE.
  plymouthVariant = "mocha";
  # Choose mauve, lavender, teal, sapphire, or sky.
  catppuccinAccent = "teal";
  catppuccinAccentName =
    {
      mauve = "Mauve";
      lavender = "Lavender";
      teal = "Teal";
      sapphire = "Sapphire";
      sky = "Sky";
    }
    .${catppuccinAccent};
  catppuccinName =
    {
      latte = "Latte";
      frappe = "Frappe";
      macchiato = "Macchiato";
      mocha = "Mocha";
    }
    .${catppuccinVariant};
  kdeTheme = pkgs.catppuccin-kde.override {
    flavour = [ catppuccinVariant ];
    accents = [ catppuccinAccent ];
  };
in
{
  imports = [
    ../../config/common.nix
    ./hardware.nix
    ./borgmatic.nix
    ./syncthing.nix
    ./pinpam.nix
    ./kwallet.nix
    ./tpm-deploy.nix
    ./disk-config.nix
  ];

  system.stateVersion = "26.05";
  virtualisation.vmVariant = import ./vm.nix;

  time.timeZone = "Europe/Vienna";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.supportedLocales = [
    "de_AT.UTF-8/UTF-8"
    "en_US.UTF-8/UTF-8"
    "es_ES.UTF-8/UTF-8"
  ];
  console.keyMap = "de";

  sops.defaultSopsFile = ./secrets.sops.yaml;
  modules.users.primaryUser = {
    username = "viki";
    name = config.repo.secrets.local.primaryUserName;
    homeDirectory = "/home/viki";
    uid = 1000;
    shell = pkgs.bashInteractive;
    passwordSecretKey = "viki-password";
    authorizedKeys = [ ];
    extraGroups = [ "networkmanager" ];
  };

  sops.secrets.ramblurr-password.neededForUsers = true;
  users.users.root.openssh.authorizedKeys.keys = [ laptopSshKey ];
  users.users.ramblurr = {
    isNormalUser = true;
    uid = 1001;
    group = "ramblurr";
    extraGroups = [ "wheel" ];
    shell = pkgs.bashInteractive;
    hashedPasswordFile = config.sops.secrets.ramblurr-password.path;
    openssh.authorizedKeys.keys = config.repo.secrets.global.pubKeys ++ [ laptopSshKey ];
  };
  users.groups.ramblurr.gid = 1001;

  modules = {
    users.enable = true;
    desktop.programs.onepassword.enable = true;
    desktop.programs.opencloud.enable = true;
    desktop.kde = {
      enable = true;
      krohnkite.enable = false;
    };
    services = {
      printing.enable = true;
      printing.drivers = [ pkgs.cups-brother-mfcl2750dw ];
      flatpak = {
        enable = true;
        autoUpdate.enable = false;
        userFlathub.enable = true;
      };
      sshd = {
        enable = true;
        openFirewall = false;
      };
    };
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    hardware.pipewire.enable = true;
  };

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;
  # Expose the Yoga's accelerometer to Plasma for automatic screen rotation.
  hardware.sensor.iio.enable = true;
  services = {
    xserver.xkb.layout = "at";
    openssh.settings.AllowUsers = [
      "root"
      "ramblurr"
    ];
    timesyncd.enable = true;
    # Use ordinary OpenSSH over the VPN, not Tailscale SSH's separate user policy.
    tailscale.extraSetFlags = [ "--ssh=false" ];
    fprintd.enable = true;
    fwupd.enable = true;
    power-profiles-daemon.enable = true;
    displayManager = {
      sddm.enable = lib.mkForce false;
      plasma-login-manager = {
        enable = true;
        settings.Greeter.PreselectedUser = "viki";
      };
    };
  };

  # Plasma supplies its portals, Dolphin, Discover, and ordinary desktop utilities.
  # Avoid the shared XDG module's personal directory layout and browser profiles.
  xdg.portal.xdgOpenUsePortal = true;
  programs = {
    firefox.enable = true;
    chromium.enable = true;
    kde-pim.enable = false;
    kdeconnect.enable = true;
    appimage = {
      enable = true;
      binfmt = true;
    };
  };
  fonts.packages = [
    pkgs.noto-fonts
    pkgs.noto-fonts-color-emoji
    pkgs.dejavu_fonts
  ];

  boot.plymouth = {
    enable = true;
    theme = "catppuccin-${plymouthVariant}";
    themePackages = [ (pkgs.catppuccin-plymouth.override { variant = plymouthVariant; }) ];
  };
  environment = {
    # Ordinary optional applications belong to the user's Flathub installation.
    plasma6.excludePackages = [
      pkgs.kdePackages.elisa
      pkgs.kdePackages.kate
      pkgs.kdePackages.ktexteditor
      pkgs.kdePackages.okular
      pkgs.kdePackages.gwenview
    ];
    systemPackages = [
      kdeTheme
      pkgs.maliit-keyboard
      # Remove once the llm-agents input includes https://github.com/numtide/llm-agents.nix/pull/9282.
      (chatgpt.override {
        chatgpt-unwrapped = chatgpt.unwrapped.overrideAttrs (old: {
          postFixup = (old.postFixup or "") + ''
            wrapProgram "$out/lib/chatgpt/ChatGPT" \
              --unset QT_PLUGIN_PATH \
              --unset QT_QPA_PLATFORM_PLUGIN_PATH
          '';
        });
      })
      inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.codex
    ];
    # KWin handles the Yoga's tablet-mode switch; select its Wayland keyboard.
    # Defaults only: users can still change these in Plasma System Settings.
    etc."xdg/kwinrc".text = ''
      [Wayland]
      InputMethod=${pkgs.maliit-keyboard}/share/applications/com.github.maliit.keyboard.desktop
      [Input]
      TabletMode=auto
    '';
    # System defaults only: ~/.config/kdeglobals remains owned by the user.
    etc."xdg/kdeglobals".text = ''
      [KDE]
      LookAndFeelPackage=Catppuccin-${catppuccinName}-${catppuccinAccentName}
      [General]
      ColorScheme=Catppuccin${catppuccinName}${catppuccinAccentName}
    '';
  };
  # disabled temporarily until entra id admin allows the app
  services.onedrive.enable = true;
  services.onedrive.package = unstable.onedrive;
  # create config file in ~/.config
  myhm = {
    xdg.configFile."autostart/1password.desktop".source =
      "${config.programs._1password-gui.package}/share/applications/1password.desktop";
    xdg.configFile."onedrive/config".text = ''
      sync_dir = "/home/viki/OneDrive"
      skip_file = "~*|.~*|*.tmp"
    '';
  };
}
