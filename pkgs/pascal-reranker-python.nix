{ lib, pkgs }:
assert lib.assertMsg (
  pkgs.stdenv.hostPlatform.system == "x86_64-linux"
) "The Pascal reranker binaries require x86_64-linux";
let
  ml = pkgs.callPackage ./immich-machine-learning-pascal.nix { };
  # Reuse the cached CUDA 12.9 libraries instead of building a second CUDA runtime.
  # The cu126 wheel uses CUDA 12 library names. GPU tests verified this combination.
  cuda = pkgs.cudaPackages_12_9 // {
    inherit (ml) cudnn;
    # Match the cu126 wheel's redistribution instead of compiling all NVSHMEM GPU targets.
    libnvshmem = pkgs.stdenv.mkDerivation {
      pname = "libnvshmem-bin";
      version = "3.4.5";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b5/09/6ea3ea725f82e1e76684f0708bbedd871fc96da89945adeba65c3835a64c/nvidia_nvshmem_cu12-3.4.5-py3-none-manylinux2014_x86_64.manylinux_2_17_x86_64.whl";
        sha256 = "042f2500f24c021db8a06c5eec2539027d57460e1c1a762055a6554f72c369bd";
      };
      nativeBuildInputs = [
        pkgs.unzip
        pkgs.autoPatchelfHook
        pkgs.autoAddDriverRunpath
      ];
      buildInputs = [
        pkgs.stdenv.cc.cc.lib
        pkgs.rdma-core
        pkgs.openmpi
        cuda.cuda_cudart
      ];
      autoPatchelfIgnoreMissingDeps = [ "libcuda.so.1" ];
      unpackPhase = ''unzip -q "$src"'';
      installPhase = ''
        mkdir -p "$out"
        cp -r nvidia/nvshmem/. "$out/"
      '';
      dontBuild = true;
      meta.license = lib.licenses.unfreeRedistributable;
      meta.platforms = [ "x86_64-linux" ];
      meta.sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    };
  };
  python = pkgs.python312.override {
    packageOverrides = final: prev: {
      fastapi = ml.python.pkgs.fastapi;
      cuda-bindings = final.buildPythonPackage {
        pname = "cuda-bindings";
        version = "12.9.4";
        format = "wheel";
        src = pkgs.fetchurl {
          url = "https://files.pythonhosted.org/packages/a9/c1/dabe88f52c3e3760d861401bb994df08f672ec893b8f7592dc91626adcf3/cuda_bindings-12.9.4-cp312-cp312-manylinux_2_24_x86_64.manylinux_2_28_x86_64.whl";
          sha256 = "fda147a344e8eaeca0c6ff113d2851ffca8f7dfc0a6c932374ee5c47caa649c8";
        };
        nativeBuildInputs = [ pkgs.autoPatchelfHook ];
        buildInputs = [ pkgs.stdenv.cc.cc.lib ];
        runtimeDependencies = map lib.getLib [
          cuda.cuda_cudart
          cuda.cuda_nvrtc
          cuda.libnvjitlink
        ];
        dependencies = [ final.cuda-pathfinder ];
        pythonImportsCheck = [ "cuda.bindings" ];
        meta.license = lib.licenses.asl20;
        meta.platforms = [ "x86_64-linux" ];
        meta.sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
      };
      torch = (prev.torch-bin.override { cudaPackages = cuda; }).overridePythonAttrs (old: {
        version = "2.14.0";
        src = pkgs.fetchurl {
          name = "torch-2.14.0+cu126-cp312-cp312-manylinux_2_28_x86_64.whl";
          url = "https://download-r2.pytorch.org/whl/cu126/torch-2.14.0%2Bcu126-cp312-cp312-manylinux_2_28_x86_64.whl";
          hash = "sha256-6ZIkQcsu0ml0KkYkaEDFNjTKlj+LCjjTAYLCi06DiNo=";
        };
        # Eager inference only: no torch.compile or distributed training.
        dependencies = [
          final.cuda-bindings
          final.filelock
          final.fsspec
          final.jinja2
          final.networkx
          final.numpy
          final.pyyaml
          final.requests
          final.setuptools
          final.sympy
          final.typing-extensions
        ];
        pythonRemoveDeps = [
          "triton"
          "cuda-toolkit"
          "nvidia-cuda-nvrtc-cu12"
          "nvidia-cuda-runtime-cu12"
          "nvidia-cuda-cupti-cu12"
          "nvidia-cudnn-cu12"
          "nvidia-cublas-cu12"
          "nvidia-cufft-cu12"
          "nvidia-curand-cu12"
          "nvidia-cusolver-cu12"
          "nvidia-cusparse-cu12"
          "nvidia-cusparselt-cu12"
          "nvidia-nccl-cu12"
          "nvidia-nvtx-cu12"
          "nvidia-nvjitlink-cu12"
          "nvidia-cufile-cu12"
          "nvidia-nvshmem-cu12"
        ];
        # nixpkgs torch-bin reports unsupported-cuda-version for its default CUDA 13 wheel.
        # This override selects a CUDA 12.6 wheel, so that warning does not apply.
        meta = old.meta // {
          platforms = [ "x86_64-linux" ];
          problems = builtins.removeAttrs (old.meta.problems or { }) [ "unsupported-cuda-version" ];
        };
      });
      sentence-transformers = final.buildPythonPackage {
        pname = "sentence-transformers";
        version = "5.7.0";
        format = "wheel";
        src = pkgs.fetchurl {
          url = "https://files.pythonhosted.org/packages/e8/c8/f63d99e354532f5b83e735dd1e001bda92495fbfde934f65d924abf2b071/sentence_transformers-5.7.0-py3-none-any.whl";
          sha256 = "b78141da3d8137e70d965866e2ca43190b9266f3d4d8752e250ded75e7136730";
        };
        dependencies = [
          final.huggingface-hub
          final.numpy
          final.scikit-learn
          final.scipy
          final.tokenizers
          final.torch
          final.tqdm
          final.transformers
          final.typing-extensions
        ];
        pythonImportsCheck = [ "sentence_transformers" ];
        meta.license = lib.licenses.asl20;
      };
    };
  };
in
python.withPackages (p: [
  p.sentence-transformers
  p.fastapi
  p.uvicorn
  p.requests
])
