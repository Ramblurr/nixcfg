inputs: _final: prev: {
  lib = prev.lib // {
    # A sane standard library of pure functions
    # ref: https://github.com/chessai/nix-std
    std = inputs.nix-std.lib;
  };

}
