# Host-owned administrative SSH over VSOCK. Public keys only.
# Onboarding and rotation: nixcfg-private/docs/services/microvm-ssh.md.
# A guest without hostKeyFile is not pinned yet, so the host refuses to connect.
{
  quine = {
    publicKeyFile = ./ssh/quine-microvm.pub;
    guests = {
      t3-users = {
        cid = 4245;
        hostKeyFile = ./ssh/t3-users-host.pub;
      };
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
  thingstead = {
    publicKeyFile = ./ssh/thingstead-microvm.pub;
    guests = {
      workstead = {
        cid = 4247;
        hostKeyFile = ./ssh/workstead-host.pub;
      };
      homestead = {
        cid = 4248;
        hostKeyFile = ./ssh/homestead-host.pub;
      };
      outstead = {
        cid = 4249;
        hostKeyFile = ./ssh/outstead-host.pub;
      };
    };
  };
  mootforge = {
    publicKeyFile = ./ssh/mootforge-microvm.pub;
    guests = {
      anvil = {
        cid = 4250;
        hostKeyFile = ./ssh/anvil-host.pub;
      };
      cinder = {
        cid = 4251;
        hostKeyFile = ./ssh/cinder-host.pub;
      };
    };
  };
}
