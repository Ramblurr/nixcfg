{ lib, pkgs }:
let
  ml = pkgs.callPackage ./immich-machine-learning-pascal.nix { };
  cuda = pkgs.cudaPackages_12_6 // { cudnn = ml.cudnn; };
  python = pkgs.python312.override {
    packageOverrides = final: prev: {
      fastapi = ml.python.pkgs.fastapi;
      torch = (prev.torch-bin.override { cudaPackages = cuda; }).overridePythonAttrs (old: {
        version = "2.14.0";
        src = pkgs.fetchurl {
          url = "https://download-r2.pytorch.org/whl/cu126/torch-2.14.0%2Bcu126-cp312-cp312-manylinux_2_28_x86_64.whl";
          hash = "sha256-6ZIkQcsu0ml0KkYkaEDFNjTKlj+LCjjTAYLCi06DiNo=";
        };
        # Eager inference only: no torch.compile, distributed training or CUDA Python API.
        dependencies = [
          final.filelock final.fsspec final.jinja2 final.networkx final.numpy
          final.pyyaml final.requests final.setuptools final.sympy final.typing-extensions
        ];
        pythonRemoveDeps = [
          "triton" "cuda-bindings" "cuda-toolkit"
          "nvidia-cuda-nvrtc-cu12" "nvidia-cuda-runtime-cu12" "nvidia-cuda-cupti-cu12"
          "nvidia-cudnn-cu12" "nvidia-cublas-cu12" "nvidia-cufft-cu12" "nvidia-curand-cu12"
          "nvidia-cusolver-cu12" "nvidia-cusparse-cu12" "nvidia-cusparselt-cu12"
          "nvidia-nccl-cu12" "nvidia-nvtx-cu12" "nvidia-nvjitlink-cu12" "nvidia-cufile-cu12"
          "nvidia-nvshmem-cu12"
        ];
        # The inherited diagnostic describes the default CUDA-13 wheel, not this cu126 artifact.
        meta = builtins.removeAttrs old.meta [ "problems" ];
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
          final.huggingface-hub final.numpy final.scikit-learn final.scipy
          final.tokenizers final.torch final.tqdm final.transformers final.typing-extensions
        ];
        pythonImportsCheck = [ "sentence_transformers" ];
        meta.license = lib.licenses.asl20;
      };
    };
  };
in
python.withPackages (p: [ p.sentence-transformers p.fastapi p.uvicorn p.requests ])
