# Lists the attribute paths of the buildable outputs, for the current system,
# of the flake in the working directory. Needs --impure.
let
  outputs = builtins.getFlake (builtins.getEnv "PWD");
  system = builtins.currentSystem;
  attrsOf =
    category:
    if outputs ? ${category} && outputs.${category} ? ${system} then
      map (name: ''${category}.${system}."${name}"'') (builtins.attrNames outputs.${category}.${system})
    else
      [ ];
  formatter =
    if outputs ? formatter && outputs.formatter ? ${system} then [ "formatter.${system}" ] else [ ];
in
builtins.concatMap attrsOf [
  "packages"
  "devShells"
  "checks"
]
++ formatter
