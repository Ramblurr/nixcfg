{
  inputs,
  config,
  lib,
  nodes,
  ...
}:
let
  inherit (config) globals;

  # Try to access the extra builtin we loaded via nix-plugins.
  # Throw an error if that doesn't exist.
  rageImportEncrypted =
    assert lib.assertMsg (builtins ? extraBuiltins.rageImportEncrypted)
      "The extra builtin 'rageImportEncrypted' is not available, so repo.secrets cannot be decrypted. Did you forget to add nix-plugins and point it to `./nix/extra-builtins.nix` ?";
    builtins.extraBuiltins.rageImportEncrypted;
in
{
  imports = [
    (rageImportEncrypted inputs.self.secretsConfig.masterIdentities ./secrets/global.nix.age)
  ];

  globals = {
    homeAssistantUrlExternal = "https://home.${globals.domain.home}";
  };
}
