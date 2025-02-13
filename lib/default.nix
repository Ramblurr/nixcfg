inputs: final: prev:
prev.lib.composeManyExtensions (
  # Order is important to allow using prev instead of final in more places to
  # speed up evaluation.
  map (x: import x inputs) [
    ./nix-std.nix
    ./nix-clj.nix
    ./my.nix
    ./attrs.nix
    ./uint.nix
    ./network.nix
  ]
) final prev
