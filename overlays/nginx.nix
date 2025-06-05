final: prev: {
  nginxQuic = prev.nginxQuic.override {
    modules = prev.lib.unique (
      prev.nginxQuic.modules
      ++ [
        prev.nginxModules.brotli
        prev.nginxModules.zstd
      ]
    );
  };
}
