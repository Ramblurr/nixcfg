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
  # llm-agents packages only Voxtype's default Whisper backend. Match
  # upstream's ONNX package for Parakeet and Cohere support, enabling CUDA
  # for Parakeet on hosts where modules.desktop.programs.voxtype.cudaSupport is set.
  voxtype =
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.voxtype.overrideAttrs
      (oldAttrs: {
        cargoBuildFeatures =
          (oldAttrs.cargoBuildFeatures or [ ])
          ++ [
            "parakeet-load-dynamic"
            "cohere"
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
      home.packages = [ voxtype ];
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
            Restart = "on-failure";
            RestartSec = 5;
          };
          Install.WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
