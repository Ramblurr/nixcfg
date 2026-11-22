# Host-owned administrative SSH over VSOCK. Public keys only.
# Onboarding and rotation: nixcfg-private/docs/services/microvm-ssh.md.
{
  quine = {
    publicKeyFile = ./ssh/quine-microvm.pub;
    guests = {
      egress = {
        cid = 4246;
        hostKeyFile = ./ssh/egress-host.pub;
      };
    };
  };
  dewey = {
    publicKeyFile = ./ssh/dewey-microvm.pub;
    guests = {
      immich-home = {
        cid = 4243;
        hostKeyFile = ./ssh/immich-home-host.pub;
      };
      opencloud-home = {
        cid = 4244;
        hostKeyFile = ./ssh/opencloud-home-host.pub;
      };
    };
  };
}
