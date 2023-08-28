{
  config,
  lib,
  pkgs,
  ...
}: {
  home-manager.users."${config.modules.users.primaryUser.username}" = {pkgs, ...} @ hm: {
    xdg.configFile."wireplumber/main.lua.d/51-disable-hdmi-sinks.lua".text = ''
      rule = {
        matches = {
          {
            { "device.name", "equals", "alsa_card.pci-0000_01_00.1" },
          },
        },
        apply_properties = {
          ["device.disabled"] = true,
        },
      }

      table.insert(alsa_monitor.rules,rule)
    '';
    xdg.configFile."wireplumber/main.lua.d/51-kef-lsx.lua".text = ''
      rule = {
        matches = {
          {
            { "node.name", "equals", "alsa_output.usb-XMOS_HIFI_DSD-00.analog-stereo" },
          },
        },
        apply_properties = {
          ["node.description"] = "KEF LSX"
        },
      }
      table.insert(alsa_monitor.rules,rule)
    '';

    xdg.configFile."wireplumber/main.lua.d/51-logitech-webcam.lua".text = ''
      rule = {
        matches = {
          {
            { "node.name", "equals", "alsa_input.usb-046d_Logitech_Webcam_C925e_0971115F-02.analog-stereo" },
          },
        },
        apply_properties = {
          ["node.description"] = "Logitech Webcam C925e Mic"
        },
      }
      table.insert(alsa_monitor.rules,rule)
    '';

    xdg.configFile."wireplumber/main.lua.d/51-yeti.lua".text = ''
      rule = {
        matches = {
          {
            { "node.name", "equals", "alsa_input.usb-Blue_Microphones_Yeti_X_2142SG0086U8_888-000316110306-00.analog-stereo" },
          },
        },
        apply_properties = {
          ["node.description"] = "Yeti X Mic"
        },
      }
      rule2 = {
        matches = {
          {
            { "node.name", "equals", "alsa_output.usb-Blue_Microphones_Yeti_X_2142SG0086U8_888-000316110306-00.analog-stereo" },
          },
        },
        apply_properties = {
          ["node.description"] = "Yeti X Headphone Output"
        },
      }
      table.insert(alsa_monitor.rules,rule)
      table.insert(alsa_monitor.rules,rule2)
    '';
    xdg.configFile."wireplumber/bluetooth.lua.d/51-rename.lua".text = ''
      rule = {
        matches = {
          {
            { "node.name", "equals", "bluez_output.F8_4E_17_84_59_D8.a2dp-sink" },
          },
        },
        apply_properties = {
          ["node.description"] = "Sony WF-1000XM4",
        },
      }

      table.insert(bluez_monitor.rules,rule)
    '';
  };
}
