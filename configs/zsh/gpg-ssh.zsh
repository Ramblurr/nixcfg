#!/usr/bin/env zsh

if [[ ! -f /usr/lib/systemd/user/gpg-agent.socket ]]; then
  # well we're on an older system without the systemd services

  # use a tty for gpg
  # solves error: "gpg: signing failed: Inappropriate ioctl for device"
  GPG_TTY=$(tty)
  export GPG_TTY

  # add alias for ssh to update the tty
  #alias ssh="gpg-connect-agent updatestartuptty /bye >/dev/null; ssh"
  gpgconf --launch gpg-agent
  export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
fi
if [[ -z "$SSH_AUTH_SOCK" ]]; then
  export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
fi
