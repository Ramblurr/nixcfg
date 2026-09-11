{ lib, pkgs }:
let
  cuda = pkgs.cudaPackages_12_9;
in
pkgs.stdenv.mkDerivation {
  pname = "cudnn-pascal";
  version = "9.10.2.21";
  src = pkgs.fetchurl {
    url = "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-9.10.2.21_cuda12-archive.tar.xz";
    sha256 = "d0defcbc4c6dad711ff4cb66d254036a300c9071b07c7b64199aacab534313c1";
  };
  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  buildInputs = [
    pkgs.stdenv.cc.cc.lib
    pkgs.zlib
    cuda.cuda_cudart
    cuda.libcublas
  ];
  runtimeDependencies = map lib.getLib [
    cuda.cuda_nvrtc
    cuda.libcublas
    cuda.cuda_cudart
  ];
  # cuDNN dlopens its split graph/engine libraries by basename.
  postPhases = [ "addCudnnRunpath" ];
  addCudnnRunpath = ''
    find "$out/lib" -type f -name '*.so*' -exec patchelf --add-rpath '$ORIGIN' {} \;
  '';
  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r lib include LICENSE $out/
    runHook postInstall
  '';
  meta = {
    description = "Pascal-compatible cuDNN redistribution pinned by Immich 3.1.0";
    homepage = "https://developer.nvidia.com/cudnn";
    license = lib.licenses.unfree;
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
