{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.programs.voxtype;
  onnxruntime = if cfg.cudaSupport then pkgs.pkgsCuda.onnxruntime else pkgs.onnxruntime;
  # Pin feat/elevenlabs-developer-backend so builds remain reproducible.
  voxtypeSrc = pkgs.fetchFromGitHub {
    owner = "Ramblurr";
    repo = "voxtype";
    rev = "649079819d9b8c669f7f8022c4cfcd4c66bf8db3";
    hash = "sha256-tJtW5PWpgG5bihfmxsXyJuw5ZNFTJZM+nHYUBA7FZTg=";
  };
  # llm-agents packages only Voxtype's default Whisper backend. Enable the fork's
  # ElevenLabs backend and match upstream's ONNX package for Parakeet and Cohere,
  # enabling CUDA where modules.desktop.programs.voxtype.cudaSupport is set.
  voxtype =
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.voxtype.overrideAttrs
      (oldAttrs: {
        src = voxtypeSrc;
        cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
          src = voxtypeSrc;
          hash = "sha256-82aa6CyD+1b5H18AwOcwg77hVSi+VuxxhlulUfN+ouQ=";
        };
        cargoBuildFeatures =
          (oldAttrs.cargoBuildFeatures or [ ])
          ++ [
            "parakeet-load-dynamic"
            "cohere"
            "elevenlabs"
          ]
          ++ lib.optionals cfg.cudaSupport [
            "parakeet-cuda"
          ];
        nativeBuildInputs =
          (oldAttrs.nativeBuildInputs or [ ])
          ++ lib.optionals cfg.cudaSupport [
            pkgs.cudaPackages.cuda_nvcc
          ];
        buildInputs =
          (oldAttrs.buildInputs or [ ])
          ++ [ onnxruntime ]
          ++ lib.optionals cfg.cudaSupport [
            pkgs.cudaPackages.cudatoolkit
            pkgs.cudaPackages.cudnn
          ];
        env = (oldAttrs.env or { }) // {
          ORT_LIB_LOCATION = "${onnxruntime}/lib";
        };
        postFixup = (oldAttrs.postFixup or "") + ''
          wrapProgram $out/bin/voxtype \
            --set ORT_DYLIB_PATH "${onnxruntime}/lib/libonnxruntime.so" \
            --prefix LD_LIBRARY_PATH : "${onnxruntime}/lib"
        '';
      });
in
{
  options.modules.desktop.programs.voxtype = {
    enable = lib.mkEnableOption "";
    autostart.enable = lib.mkEnableOption "";
    cudaSupport = lib.mkEnableOption "CUDA acceleration for the Parakeet backend";
  };
  config = lib.mkIf cfg.enable {
    myhm = {
      home.packages = [
        voxtype
        pkgs.quickshell
      ];
      systemd.user.services = lib.mkIf cfg.autostart.enable {
        voxtype = lib.mkForce {
          Unit = {
            Description = "Voxtype push-to-talk voice-to-text daemon";
            Documentation = "https://voxtype.io";
            After = [
              "graphical-session.target"
              "pipewire.service"
              "pipewire-pulse.service"
            ];
            PartOf = [ "graphical-session.target" ];
          };
          Service = {
            Type = "simple";
            ExecStart = "${voxtype}/bin/voxtype daemon";
            #ExecStart = "/home/ramblurr/src/github.com/peteonrails/voxtype/target/debug/voxtype daemon";
            Restart = "on-failure";
            RestartSec = 5;
            Environment = [
              "VOXTYPE_OSD_QML_PATH=${voxtype.src}/quickshell"
            ];
            #EnvironmentFile = "/home/ramblurr/.llm-keys";
          };
          Install.WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
