final: prev: {
  #openai-whisper = prev.openai-whisper.override { torch = prev.pkgs.python3Packages.torch-bin; };
}
