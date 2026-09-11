{
  lib,
  pkgs,
  immich ? pkgs.immich,
}:
let
  # Keep the standard CUDA derivations so Flox substitutes remain usable.
  cuda = pkgs.cudaPackages_12_9;
  cudnn = pkgs.callPackage ./cudnn-pascal.nix { };
  python = pkgs.python312.override {
    packageOverrides =
      final: prev:
      let
        # Documentation snapshots fail on generated example formatting.
        # See docs/immich-pascal.md, "Documentation test exception", before removal.
        docsSnapshot = prev.inline-snapshot.overridePythonAttrs (old: {
          disabledTestPaths = (old.disabledTestPaths or [ ]) ++ [ "tests/test_docs.py" ];
        });
      in
      {
        fastapi = prev.fastapi.override { inline-snapshot = docsSnapshot; };
        rich-toolkit = prev.rich-toolkit.override { inline-snapshot = docsSnapshot; };
        onnxruntime = final.buildPythonPackage {
          pname = "onnxruntime-gpu";
          version = "1.23.2";
          format = "wheel";
          src = pkgs.fetchurl {
            url = "https://files.pythonhosted.org/packages/6c/d9/b7140a4f1615195938c7e358c0804bb84271f0d6886b5cbf105c6cb58aae/onnxruntime_gpu-1.23.2-cp312-cp312-manylinux_2_27_x86_64.manylinux_2_28_x86_64.whl";
            sha256 = "4f2d1f720685d729b5258ec1b36dee1de381b8898189908c98cbeecdb2f2b5c2";
          };
          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
          buildInputs = [
            pkgs.stdenv.cc.cc.lib
            cuda.cuda_cudart
            cuda.libcublas
            cuda.libcufft
            cuda.libcurand
            cudnn
          ];
          dependencies = [
            final.coloredlogs
            final.numpy
            final.packaging
            final.flatbuffers
            final.protobuf
            final.sympy
          ];
          # TensorRT 10 does not support Pascal; Immich uses CUDAExecutionProvider.
          postInstall = ''
            rm "$out/${python.sitePackages}/onnxruntime/capi/libonnxruntime_providers_tensorrt.so"
          '';
          # ORT dlopens its provider bridge by basename, outside ELF DT_NEEDED.
          postPhases = [ "addOrtRunpath" ];
          addOrtRunpath = ''
            for elf in "$out/${python.sitePackages}/onnxruntime/capi/"*.so*; do
              patchelf --add-rpath '$ORIGIN' "$elf"
            done
          '';
          pythonImportsCheck = [ "onnxruntime" ];
          meta = {
            description = "Upstream CUDA 12 ONNX Runtime wheel with Pascal kernels";
            license = lib.licenses.mit;
            platforms = [ "x86_64-linux" ];
            sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
          };
        };
        # These consumers declare the CPU distribution name, but import the same module.
        insightface = prev.insightface.overridePythonAttrs (old: {
          pythonRemoveDeps = (old.pythonRemoveDeps or [ ]) ++ [ "onnxruntime" ];
        });
        rapidocr = prev.rapidocr.overridePythonAttrs (old: {
          pythonRemoveDeps = (old.pythonRemoveDeps or [ ]) ++ [ "onnxruntime" ];
        });
      };
  };
in
(pkgs.immich-machine-learning.override {
  inherit immich;
  python3 = python;
}).overrideAttrs
  (old: {
    meta = old.meta // {
      description = "Immich ML with a Pascal-compatible CUDA runtime";
      platforms = [ "x86_64-linux" ];
    };
    passthru = (old.passthru or { }) // {
      inherit python cudnn;
      validationPython = python.withPackages (p: [
        p.onnx
        p.onnxruntime
        p.pillow
        p.requests
      ]);
    };
  })
