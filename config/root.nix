_: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    histSize = 10000;
    setOptions = [
      "autocd"
      "cdablevars"
      "histallowclobber"
      "histexpiredupsfirst"
      "histfcntllock"
      "histignorealldups"
      "histignoredups"
      "histignorespace"
      "histreduceblanks"
      "incappendhistory"
      "nomultios"
      "sharehistory"
    ];
    interactiveShellInit = ''
      autoload -U colors && colors
      typeset -A key
      key[Home]=$${terminfo [ khome ]}
      key[End]=$${terminfo [ kend ]}
      key[Insert]=$${terminfo [ kich1 ]}
      key[Delete]=$${terminfo [ kdch1 ]}
      key[Up]=$${terminfo [ kcuu1 ]}
      key[Down]=$${terminfo [ kcud1 ]}
      key[Left]=$${terminfo [ kcub1 ]}
      key[Right]=$${terminfo [ kcuf1 ]}
      key[PageUp]=$${terminfo [ kpp ]}
      key[PageDown]=$${terminfo [ knp ]}

      [[ -n "$${key [ Home ]}"      ]]  && bindkey  "$${key [ Home ]}"      beginning-of-line
      [[ -n "$${key [ End ]}"       ]]  && bindkey  "$${key [ End ]}"       end-of-line
      [[ -n "$${key [ Insert ]}"    ]]  && bindkey  "$${key [ Insert ]}"    overwrite-mode
      [[ -n "$${key [ Delete ]}"    ]]  && bindkey  "$${key [ Delete ]}"    delete-char
      [[ -n "$${key [ Up ]}"        ]]  && bindkey  "$${key [ Up ]}"        up-line-or-history
      [[ -n "$${key [ Down ]}"      ]]  && bindkey  "$${key [ Down ]}"      down-line-or-history
      [[ -n "$${key [ Left ]}"      ]]  && bindkey  "$${key [ Left ]}"      backward-char
      [[ -n "$${key [ Right ]}"     ]]  && bindkey  "$${key [ Right ]}"     forward-char
      [[ -n "$${key [ PageUp ]}"    ]]  && bindkey  "$${key [ PageUp ]}"    beginning-of-buffer-or-history
      [[ -n "$${key [ PageDown ]}"  ]]  && bindkey  "$${key [ PageDown ]}"  end-of-buffer-or-history
      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word

      if [[ $EUID -eq 0 ]]; then
      else
        export XDG_RUNTIME_DIR="/run/user/$UID";
        export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$UID/bus";
      fi
    '';
    promptInit = ''
      # Root prompt - red
      if [[ $EUID -eq 0 ]]; then
          PS1='%F{red}[%m:%~]# %f'
      else
          # Regular user prompt - green
          PS1='%F{green}[%n@%m:%~]$ %f'
      fi
    '';

    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../../";
      utcnow = ''date -u +"%Y-%m-%d %H:%M:%S"'';
      log = "journalctl --output cat -e -u"; # log sshd -f
      log-previous-boot = "journalctl --boot=-1";
      st = "systemctl status";
      sS = "systemctl stop";
      sr = "systemctl reload";
      sR = "systemctl restart";
      list-units = "systemctl list-units --type=service | awk '/.service/ {print $1}'";
    };
  };
}
