# Host-owned administrative SSH over VSOCK. Public keys only.
# Onboarding and rotation: docs/microvm-ssh.md.
{
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
