final: prev: {
  #openai-whisper = prev.openai-whisper.override { torch = prev.pkgs.python311Packages.torch-bin; };
}
