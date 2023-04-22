{pkgs, ...}: let
  fixedSshAgentSocket = "/run/user/1000/sshagent";
  effectiveGpgDir = "/run/user/1000/gnupg/d.kbocp7uc7zjy47nnek3436ij/"; #TODO: get this from gpg-agent module
  gpgSshSock = "${effectiveGpgDir}/S.gpg-agent.ssh";
in {
  config = {
    # symlink root's ssh config to ours
    # to fix nix-daemon's ability to remote build since it sshs from the root account
    system.activationScripts.root_ssh_config = {
      text = ''
        (
          # symlink root ssh config to ours so daemon can use our agent/keys/etc...
          mkdir -p /root/.ssh
          ln -sf /home/ramblurr/.ssh/config /root/.ssh/config
          ln -sf /home/ramblurr/.ssh/known_hosts /root/.ssh/known_hosts
          ln -sf /home/ramblurr/.ssh/known_hosts /root/.ssh/known_hosts

          # and we control our own SSH_AUTH_SOCK fate
          # mkdir -p "$(dirname "${fixedSshAgentSocket}")" || true
          # ln -sf "${gpgSshSock}" "${fixedSshAgentSocket}" || true
        )
      '';
      deps = [];
    };

    home-manager.users.ramblurr = {
      pkgs,
      lib,
      ...
    }: {
      home.file.".ssh/control/.keep".text = "";
      programs.ssh = {
        enable = true;
        controlPath = "/home/ramblurr/.ssh/control/%C"; # TODO: lolwut
        # just no... this shit does not work well
        #controlMaster = "auto";
        #controlPersist = "1m";
        #serverAliveInterval = 5;
        #serverAliveCountMax = 2;
      };
    };
  };
}