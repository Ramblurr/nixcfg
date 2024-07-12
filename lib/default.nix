inputs: final: prev:
prev.lib.composeManyExtensions (
  # Order is important to allow using prev instead of final in more places to
  # speed up evaluation.
  map (x: import x inputs) [
    ./attrs.nix
    ./network.nix
    ./toml.nix
  ]
) final prev
