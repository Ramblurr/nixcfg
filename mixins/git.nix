{pkgs, ...}: {
  home-manager.users.ramblurr = {pkgs, ...}: {
    programs.git = {
      enable = true;
      signing.key = "978C4D08058BA26EB97CB51820782DBCACFAACDA";
      signing.signByDefault = false;
      userEmail = "unnamedrambler@gmail.com";
      userName = "Casey Link";
      extraConfig = {
        init.defaultBranch = "main";
        user.editor = "vim";
        pull.rebase = true;
        safe = {
          directory = "/home/ramblurr/src/nixcfg";
        };
      };
      aliases = {
        ll = "log --pretty=format:\"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]\" --decorate --numstat";
      };

      difftastic = {
        enable = true;
      };
    };
  };
}
