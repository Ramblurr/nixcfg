{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];
  perSystem =
    { pkgs, lib, ... }:
    {
      treefmt = {
        projectRootFile = "flake.nix";
        programs = {
          nixfmt.enable = pkgs.lib.meta.availableOn pkgs.stdenv.buildPlatform pkgs.nixfmt-rfc-style.compiler;
          nixfmt.package = pkgs.nixfmt-rfc-style;
          #shellcheck.enable = true;
        };
        settings.formatter = {
          #shellcheck.options = [
          #  "-s"
          #  "bash"
          #];
        };
      };
    };
}
