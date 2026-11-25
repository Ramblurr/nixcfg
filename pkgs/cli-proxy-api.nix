# Our patched CLIProxyAPI fork. The forge is reachable only from the home network
# (no public DNS record) but needs no authentication; builtins.fetchGit fetches it
# anonymously over HTTPS at evaluation time.
# To update: bump rev and version; if go.mod/go.sum changed, bump vendorHash
# (the fork's flake.nix carries the current value).
{ cli-proxy-api }:
let
  version = "8.0.66-unstable-2026-11-25-caller-usage";
  src = builtins.fetchGit {
    url = "https://git.outskirtslabs.com/forks/cliproxyapi.git";
    ref = "main";
    rev = "0e16d8983bc64dfcd554c488dc0ad60f355aa85b";
  };
in
cli-proxy-api.overrideAttrs {
  inherit version src;
  vendorHash = "sha256-02Qqrs9lQ0kE+Tnk/SWvlkLXGSh8RQPxRawFZogRYNc=";
  ldflags = [
    "-s"
    "-w"
    "-X main.Version=${version}"
    "-X main.Commit=${src.shortRev}"
    "-buildid="
  ];
}
