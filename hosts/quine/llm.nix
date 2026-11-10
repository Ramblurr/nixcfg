_: {
  imports = [ ./bb.nix ];
  # TODO disable smolvm due to failed upstream pkg build
  #environment.systemPackages = [ inputs.smolvm.packages.${pkgs.stdenv.hostPlatform.system}.default ];

  modules.dev.llms.t3 = {
    enable = true;
    desktop.enable = true;
    tailscaleServePort = 8443;
  };
}
