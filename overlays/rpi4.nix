final: prev: {
  #raspberrypifw = prev.raspberrypifw.overrideAttrs (old: {
  #  version = "stable_20240124";
  #  src = prev.fetchFromGitHub {
  #    owner = "raspberrypi";
  #    repo = "firmware";
  #    rev = "d4a1760a76873c467e99a5a27f98815e65fb9a55";
  #    hash = "sha256-uJvffgqeaet219tiWlh9rdOauXxgwoLfQSYCjn8LT0U=";
  #  };
  #});

  #raspberrypifw = prev.raspberrypifw.overrideAttrs (old: {
  #  version = "unstable-2024-01-17";
  #  srcs = [
  #    (prev.fetchFromGitHub {
  #      name = "bluez-firmware";
  #      owner = "RPi-Distro";
  #      repo = "bluez-firmware";
  #      rev = "d9d4741caba7314d6500f588b1eaa5ab387a4ff5";
  #      hash = "sha256-CjbZ3t3TW/iJ3+t9QKEtM9NdQU7SwcUCDYuTmFEwvhU=";
  #    })
  #    (prev.fetchFromGitHub {
  #      name = "firmware-nonfree";
  #      owner = "RPi-Distro";
  #      repo = "firmware-nonfree";
  #      rev = "3db4164cfd89e6d9afb7ebc87607b792651512df";
  #      hash = "sha256-Qu96GKezjF39bBlYsWhEv6CoIpap1jtHTvcrszZOzzE=";
  #    })
  #  ];
  #});

  #linux_rpi4 = prev.linux_rpi4.override {
  #  argsOverride = rec {
  #    version = "6.1.73-stable_20240124";
  #    modDirVersion = "6.1.73";
  #    src = prev.fetchFromGitHub {
  #      owner = "raspberrypi";
  #      repo = "linux";
  #      rev = "refs/tags/stable_20240124";
  #      hash = "sha256-P4ExzxWqZj+9FZr9U2tmh7rfs/3+iHEv0m74PCoXVuM=";
  #    };
  #  };
  #};

  #pylibgpiod_1 = prev.python311Packages.libgpiod.override {
  #  libgpiod = prev.libgpiod_1;
  #};
  #overrideAttrs (old: {
  #  pname = prev.libgpiod_1.pname;
  #  version = prev.libgpiod_1.version;
  #  src = prev.libgpiod_1.src;
  #  buildInputs = [prev.libgpiod_1];
  #});
  # NEEDED FOR FAIRYBOX
  # python311Packages = prev.python311Packages // {
  #   pylibgpiod_11 = prev.python311Packages.toPythonModule (
  #     prev.libgpiod_1.override {
  #       enablePython = true;
  #       python3 = prev.python311;
  #     }
  #   );
  # };
  # spidev_1 = prev.python311Packages.spidev.overrideAttrs (old: {
  #   version = "3.4";
  #   pname = "spidev";
  #   src = prev.fetchPypi {
  #     version = "3.4";
  #     pname = "spidev";
  #     hash = "sha256-QxTlL1c9lSM8kH8wdViJMxOopgbhl+d7txFSaw4XnoA=";
  #   };
  # });
  #rpi-gpio2_1 = prev.python311Packages.rpi-gpio2.overrideAttrs (old: {
  #  propagatedBuildInputs = [ prev.libgpiod_1 ];
  #});

}
