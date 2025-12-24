{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.hardware.keyboardio;
in
{
  options = {
    modules.hardware.keyboardio = {
      enable = lib.mkEnableOption "";
      undervolt = {
        enable = lib.mkEnableOption "";
        value = mkOption {
          type = types.str;
          default = "-p 0 -v 30 -f A8"; # Pstate 0, 1.25 voltage, 4200 clock speed
        };
      };
    };
  };

  config = mkIf cfg.enable {
    # cannot use services.udev.extraRules because it does not work with uaccess
    # ref: https://github.com/NixOS/nixpkgs/issues/308681
    # ref: https://github.com/NixOS/nixpkgs/issues/210856#issue-1533727709
    services.udev.packages = [
      (pkgs.writeTextFile {
        name = "keyboardio-udev-rules";
        #KERNEL=="ttyACM0", MODE:="666"
        #KERNEL=="ttyACM1", MODE:="666"
        text = ''
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2303", SYMLINK+="Atreus",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2302", SYMLINK+="Atreus",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2301", SYMLINK+="Model01",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2300", SYMLINK+="Model01",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0006", SYMLINK+="Model100",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0005", SYMLINK+="Model100",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="00a1", SYMLINK+="Preonic",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="00a3", SYMLINK+="Preonic",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="00a0", SYMLINK+="Preonic",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="00a3", SYMLINK+="Preonic",  ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
        '';
        destination = "/etc/udev/rules.d/70-keyboardio.rules";
      })
      (pkgs.writeTextFile {
        name = "esp32-udev-rules";
        text = ''
          KERNEL=="ttyACM0", MODE:="666"
          KERNEL=="ttyACM1", MODE:="666"
          # SPDX-License-Identifier: GPL-2.0-or-later

          # Copy this file to /etc/udev/rules.d/
          # If rules fail to reload automatically, you can refresh udev rules
          # with the command "udevadm control --reload"


          ACTION!="add|change", GOTO="openocd_rules_end"

          SUBSYSTEM=="gpio", MODE="0660", GROUP="input", TAG+="uacces", TAG+="seat"

          SUBSYSTEM!="usb|tty|hidraw", GOTO="openocd_rules_end"

          # Please keep this list sorted by VID:PID

          # opendous and estick
          ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="204f", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT232/FT245 VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT2232 VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT4232 VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6011", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT232H VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6014", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          # Original FT231XQ VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT2233HP VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6040", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT4233HP VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6041", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT2232HP VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6042", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT4232HP VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6043", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT233HP VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6044", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT232HP VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6045", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Original FT4232HA VID:PID
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6048", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # DISTORTEC JTAG-lock-pick Tiny 2
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="8220", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TUMPA, TUMPA Lite
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="8a98", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="8a99", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Marvell OpenRD JTAGKey FT2232D B
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="9e90", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # XDS100v2
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="a6d0", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          # XDS100v3
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="a6d1", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # OOCDLink
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="baf8", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Kristech KT-Link
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bbe2", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Xverve Signalyzer Tool (DT-USB-ST), Signalyzer LITE (DT-USB-SLITE)
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bca0", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bca1", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TI/Luminary Stellaris Evaluation Board FTDI (several)
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bcd9", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TI/Luminary Stellaris In-Circuit Debug Interface FTDI (ICDI) Board
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bcda", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # egnite Turtelizer 2
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bdc8", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Section5 ICEbear
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="c140", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="c141", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Amontec JTAGkey and JTAGkey-tiny
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="cff8", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # ASIX Presto programmer
          ATTRS{idVendor}=="0403", ATTRS{idProduct}=="f1a0", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Nuvoton NuLink
          ATTRS{idVendor}=="0416", ATTRS{idProduct}=="511b", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0416", ATTRS{idProduct}=="511c", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0416", ATTRS{idProduct}=="511d", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0416", ATTRS{idProduct}=="5200", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0416", ATTRS{idProduct}=="5201", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TI ICDI
          ATTRS{idVendor}=="0451", ATTRS{idProduct}=="c32a", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # STMicroelectronics ST-LINK V1
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3744", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # STMicroelectronics ST-LINK/V2
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3748", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # STMicroelectronics ST-LINK/V2.1
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="374b", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3752", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # STMicroelectronics STLINK-V3
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="374d", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="374e", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="374f", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3753", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3754", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3755", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3757", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Cypress SuperSpeed Explorer Kit
          ATTRS{idVendor}=="04b4", ATTRS{idProduct}=="0007", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Cypress KitProg in KitProg mode
          ATTRS{idVendor}=="04b4", ATTRS{idProduct}=="f139", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Cypress KitProg in CMSIS-DAP mode
          ATTRS{idVendor}=="04b4", ATTRS{idProduct}=="f138", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Infineon DAP miniWiggler v3
          ATTRS{idVendor}=="058b", ATTRS{idProduct}=="0043", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Hitex LPC1768-Stick
          ATTRS{idVendor}=="0640", ATTRS{idProduct}=="0026", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Hilscher NXHX Boards
          ATTRS{idVendor}=="0640", ATTRS{idProduct}=="0028", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Hitex STR9-comStick
          ATTRS{idVendor}=="0640", ATTRS{idProduct}=="002c", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Hitex STM32-PerformanceStick
          ATTRS{idVendor}=="0640", ATTRS{idProduct}=="002d", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Hitex Cortino
          ATTRS{idVendor}=="0640", ATTRS{idProduct}=="0032", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Altera USB Blaster
          ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6001", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          # Altera USB Blaster2
          ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6010", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6810", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Ashling Opella-LD
          ATTRS{idVendor}=="0B6B", ATTRS{idProduct}=="0040", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Amontec JTAGkey-HiSpeed
          ATTRS{idVendor}=="0fbb", ATTRS{idProduct}=="1000", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # SEGGER J-Link
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0101", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0102", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0103", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0104", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0105", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0107", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0108", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1010", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1011", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1012", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1013", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1014", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1015", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1016", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1017", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1018", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1020", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1051", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1055", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1061", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Raisonance RLink
          ATTRS{idVendor}=="138e", ATTRS{idProduct}=="9000", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Debug Board for Neo1973
          ATTRS{idVendor}=="1457", ATTRS{idProduct}=="5118", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # OSBDM
          ATTRS{idVendor}=="15a2", ATTRS{idProduct}=="0042", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="15a2", ATTRS{idProduct}=="0058", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="15a2", ATTRS{idProduct}=="005e", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Olimex ARM-USB-OCD
          ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="0003", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Olimex ARM-USB-OCD-TINY
          ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="0004", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Olimex ARM-JTAG-EW
          ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="001e", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Olimex ARM-USB-OCD-TINY-H
          ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="002a", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Olimex ARM-USB-OCD-H
          ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="002b", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # ixo-usb-jtag - Emulation of a Altera Bus Blaster I on a Cypress FX2 IC
          ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="06ad", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # USBprog with OpenOCD firmware
          ATTRS{idVendor}=="1781", ATTRS{idProduct}=="0c63", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TI/Luminary Stellaris In-Circuit Debug Interface (ICDI) Board
          ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TI XDS110 Debug Probe (Launchpads and Standalone)
          ATTRS{idVendor}=="0451", ATTRS{idProduct}=="bef3", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="0451", ATTRS{idProduct}=="bef4", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="02a5", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # TI Tiva-based ICDI and XDS110 probes in DFU mode
          ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00ff", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # isodebug v1
          ATTRS{idVendor}=="22b7", ATTRS{idProduct}=="150d", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # PLS USB/JTAG Adapter for SPC5xxx
          ATTRS{idVendor}=="263d", ATTRS{idProduct}=="4001", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Numato Mimas A7 - Artix 7 FPGA Board
          ATTRS{idVendor}=="2a19", ATTRS{idProduct}=="1009", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Ambiq Micro EVK and Debug boards.
          ATTRS{idVendor}=="2aec", ATTRS{idProduct}=="6010", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="2aec", ATTRS{idProduct}=="6011", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="2aec", ATTRS{idProduct}=="1106", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Espressif USB JTAG/serial debug units
          ATTRS{idVendor}=="303a", ATTRS{idProduct}=="1001", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="303a", ATTRS{idProduct}=="1002", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # ANGIE USB-JTAG Adapter
          ATTRS{idVendor}=="584e", ATTRS{idProduct}=="414f", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="584e", ATTRS{idProduct}=="424e", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="584e", ATTRS{idProduct}=="4255", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="584e", ATTRS{idProduct}=="4355", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="584e", ATTRS{idProduct}=="4a55", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Marvell Sheevaplug
          ATTRS{idVendor}=="9e88", ATTRS{idProduct}=="9e8f", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # Keil Software, Inc. ULink
          ATTRS{idVendor}=="c251", ATTRS{idProduct}=="2710", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"
          ATTRS{idVendor}=="c251", ATTRS{idProduct}=="2750", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          # CMSIS-DAP compatible adapters
          ATTRS{product}=="*CMSIS-DAP*", MODE="660", GROUP="input", TAG+="uacces", TAG+="seat"

          LABEL="openocd_rules_end"
        '';
        destination = "/lib/udev/rules.d/10-esp32.rules";
      })
    ];
  };
}
