final: prev: { openai-whisper = prev.openai-whisper.override { torch = prev.pkgs.torch-bin; }; }
