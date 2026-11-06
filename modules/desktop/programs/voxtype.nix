{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.programs.voxtype;
  # CUDA 13.4's sanitizer binaries currently crash patchelf during fixup.
  cudaPackages = pkgs.cudaPackages_12_9;
  onnxruntime =
    if cfg.cudaSupport then
      pkgs.pkgsCuda.onnxruntime.override { inherit cudaPackages; }
    else
      pkgs.onnxruntime;
  # Pin feat/elevenlabs-developer-backend so builds remain reproducible.
  voxtypeSrc = pkgs.fetchFromGitHub {
    owner = "Ramblurr";
    repo = "voxtype";
    rev = "deabf1e4106d8522b91d6eb0b7bba9dccad61a5a";
    hash = "sha256-vXLV0LlPG4ogIl8A5CS2NPmvNmacfptfYQfAcmQEuQc=";
  };
  # llm-agents packages only Voxtype's default Whisper backend. Enable the fork's
  # ElevenLabs backend and match upstream's ONNX package for Parakeet and Cohere,
  # enabling CUDA where modules.desktop.programs.voxtype.cudaSupport is set.
  voxtype =
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.voxtype.overrideAttrs
      (oldAttrs: {
        src = voxtypeSrc;
        # Match upstream's lockfile importer: fetchCargoVendor follows a recursive
        # symlink in the OpenVINO git dependency and fails while copying it.
        cargoDeps = pkgs.rustPlatform.importCargoLock {
          lockFile = "${voxtypeSrc}/Cargo.lock";
          outputHashes = {
            "openvino-finder-0.11.0" = "sha256-nQWeHNdLlRk+owh3B6VpArAG2dr66HeBI2RHfDhyvvU=";
            "openvino-genai-0.11.0" = "sha256-nQWeHNdLlRk+owh3B6VpArAG2dr66HeBI2RHfDhyvvU=";
            "openvino-genai-sys-0.11.0" = "sha256-nQWeHNdLlRk+owh3B6VpArAG2dr66HeBI2RHfDhyvvU=";
            "openvino-sys-0.11.0" = "sha256-nQWeHNdLlRk+owh3B6VpArAG2dr66HeBI2RHfDhyvvU=";
          };
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
            cudaPackages.cuda_nvcc
          ];
        buildInputs =
          (oldAttrs.buildInputs or [ ])
          ++ [ onnxruntime ]
          ++ lib.optionals cfg.cudaSupport [
            cudaPackages.cudatoolkit
            cudaPackages.cudnn
          ];
        env = (oldAttrs.env or { }) // {
          ORT_LIB_LOCATION = "${onnxruntime}/lib";
        };
        postFixup = (oldAttrs.postFixup or "") + ''
          wrapProgram $out/bin/voxtype \
            --set ORT_DYLIB_PATH "${onnxruntime}/lib/libonnxruntime.so" \
            --prefix LD_LIBRARY_PATH : "${
              lib.makeLibraryPath ([ onnxruntime ] ++ lib.optionals cfg.cudaSupport [ cudaPackages.cuda_cudart ])
            }"
        '';
      });
in
{
  options.modules.desktop.programs.voxtype = {
    enable = lib.mkEnableOption "";
    autostart.enable = lib.mkEnableOption "";
    cudaSupport = lib.mkEnableOption "CUDA acceleration for the Parakeet backend";
    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Runtime environment file for Voxtype credentials; never put secrets in the Nix store.";
    };
    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra global arguments for the Voxtype daemon. Do not pass credentials here.";
    };
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
            ExecStart = "${voxtype}/bin/voxtype${
              lib.optionalString (cfg.extraArgs != [ ]) " ${lib.escapeShellArgs cfg.extraArgs}"
            } daemon";
            #ExecStart = "/home/ramblurr/src/github.com/peteonrails/voxtype/target/debug/voxtype daemon";
            Restart = "on-failure";
            RestartSec = 5;
            Environment = [
              "VOXTYPE_OSD_QML_PATH=${voxtype.src}/quickshell"
            ];
            EnvironmentFile = lib.mkIf (cfg.environmentFile != null) cfg.environmentFile;
          };
          Install.WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
