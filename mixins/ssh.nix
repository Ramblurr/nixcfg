{pkgs, ...}: let
  fixedSshAgentSocket = "/run/user/1000/sshagent";
  effectiveGpgDir = "/run/user/1000/gnupg/d.kbocp7uc7zjy47nnek3436ij/"; #TODO: get this from gpg-agent module
  gpgSshSock = "${effectiveGpgDir}/S.gpg-agent.ssh";
in {
  config = {
    home-manager.users.ramblurr = {
      pkgs,
      lib,
      ...
    }: {
      home.file.".ssh/control/.keep".text = "";
      programs.ssh = {
        enable = true;
        controlPath = "/home/ramblurr/.ssh/control/%C";
      };

      home.persistence = {
        "/persist/home/ramblurr".directories = [".ssh"];
      };
    };
  };
}
