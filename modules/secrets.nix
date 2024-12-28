# This file is based on the work by oddlama at
# https://github.com/oddlama/nix-config/blob/main/modules/secrets.nix
{
  config,
  lib,
  pkgs,
  ...
}:

let
  # If the given expression is a bare set, it will be wrapped in a function,
  # so that the imported file can always be applied to the inputs, similar to
  # how modules can be functions or sets.
  constSet = x: if builtins.isAttrs x then (_: x) else x;

  assertMsg = pred: msg: pred || builtins.throw msg;
  hasSuffix =
    suffix: content:
    let
      lenContent = builtins.stringLength content;
      lenSuffix = builtins.stringLength suffix;
    in
    lenContent >= lenSuffix && builtins.substring (lenContent - lenSuffix) lenContent content == suffix;

  safeImport =
    nixFile:
    assert assertMsg (builtins.isPath nixFile)
      "The file to decrypt must be given as a path to prevent impurity.";
    assert assertMsg (hasSuffix ".nix" nixFile)
      "The content of the decrypted file must be a nix expression";
    import nixFile;

  importEncrypted = path: (constSet (if builtins.pathExists path then safeImport path else { })) { };

  cfg = config.repo;
in
{
  options.repo = {
    secretFiles_old = lib.mkOption {
      default = { };
      type = lib.types.attrsOf lib.types.path;
      example = lib.literalExpression "{ local = ./secrets/local.nix }";
      description = ''
        This file manages the origin for this machine's repository-secrets. Anything that is
        technically not a secret in the classical sense (i.e. that it has to be protected
        after it has been deployed), but something you want to keep secret from the public;
        Anything that you wouldn't want people to see on GitHub, but that can live unencrypted
        on your own devices. We are assuming you are using a tool like git-crypt to encrypt
        these secrets in the repo. Setup of that is NOT handled by this module.

        The module will use the file extension of the path to call the appropriate builtin reader
        (for example fromJSON). If it has a .nix extension, it will be imported as a nix expression.

        All of these secrets may (and probably will be) put into the world-readable nix-store
        on the build and target hosts. You'll most likely want to store personally identifiable
        information here, such as:
          - MAC Addreses
          - Static IP addresses
          - Your full name (when configuring your users)
          - Your postal address (when configuring e.g. home-assistant)
          - ...

        Each path given here must be an age-encrypted .nix file. For each attribute `<name>`,
        the corresponding file will be decrypted, imported and exposed as {option}`repo.secrets.<name>`.
      '';
    };
    secrets_old = lib.mkOption {
      readOnly = true;
      default = lib.mapAttrs (_: x: importEncrypted x) cfg.secretFiles_old;
      #default = { global = (importEncrypted cfg.secretFiles.global) { }; };
      type = lib.types.unspecified;
      description = "Exposes the loaded repo secrets. This option is read-only.";
    };
  };
}
