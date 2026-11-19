# Keeps casey/skills checked out at /var/lib/skills/current for all wrights.
# A push webhook from git.outskirtslabs.com (peirce) triggers a sync; a timer
# catches missed hooks. The hook only pokes the sync, which always fetches the
# fixed public URL, so it needs no secret.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  user = "skills";
  root = "/var/lib/skills";
  url = "https://git.outskirtslabs.com/casey/skills.git";
  branch = "main";
  webhookPort = 9000;
  peirce = lib.head config.site.net.prim.hosts4.peirce;
  # Each revision is extracted into revs/<sha> and `current` is swapped
  # atomically, so readers never see a half-updated tree.
  sync = pkgs.writeShellApplication {
    name = "skills-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
      pkgs.gnutar
      pkgs.util-linux
    ];
    text = ''
      root=${root}
      exec 9> "$root/.lock"
      flock 9

      repo="$root/repo.git"
      [ -d "$repo" ] || git init --quiet --bare "$repo"
      git -C "$repo" fetch --quiet --prune ${url} +refs/heads/${branch}:refs/heads/${branch}
      rev="$(git -C "$repo" rev-parse refs/heads/${branch})"

      mkdir -p "$root/revs"
      if [ ! -d "$root/revs/$rev" ]; then
        tmp="$(mktemp -d "$root/revs/.tmp-XXXXXX")"
        git -C "$repo" archive "$rev" | tar -x -C "$tmp"
        chmod 0755 "$tmp"
        mv -T "$tmp" "$root/revs/$rev"
      fi

      if [ "$(readlink "$root/current" || true)" != "revs/$rev" ]; then
        ln -sfn "revs/$rev" "$root/.current.new"
        mv -T "$root/.current.new" "$root/current"
        echo "skills now at $rev"
      fi

      # Keep the two newest older revisions for sessions still reading them.
      find "$root/revs" -mindepth 1 -maxdepth 1 -name '.tmp-*' -mmin +60 -exec rm -rf {} +
      find "$root/revs" -mindepth 1 -maxdepth 1 ! -name '.tmp-*' ! -name "$rev" -printf '%T@ %p\n' \
        | sort -rn | tail -n +3 | cut -d' ' -f2- | xargs -r rm -rf
    '';
  };
in
{
  users.groups.${user} = { };
  users.users.${user} = {
    isSystemUser = true;
    group = user;
    home = root;
  };

  systemd.tmpfiles.rules = [ "d ${root} 0755 ${user} ${user} -" ];

  systemd.services.skills-sync = {
    description = "Sync casey/skills into ${root}";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = user;
      Group = user;
      UMask = "0022";
      ExecStart = lib.getExe sync;
    };
  };

  systemd.timers.skills-sync = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "15min";
    };
  };

  services.webhook = {
    enable = true;
    inherit user;
    group = user;
    port = webhookPort;
    hooks.skills-sync = {
      execute-command = lib.getExe sync;
      response-message = "skills sync started";
      trigger-rule.match = {
        type = "value";
        value = "refs/heads/${branch}";
        parameter = {
          source = "payload";
          name = "ref";
        };
      };
    };
  };

  # Only peirce (git.outskirtslabs.com) may reach the webhook.
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p tcp -s ${peirce} --dport ${toString webhookPort} -j nixos-fw-accept
  '';
  networking.firewall.extraStopCommands = ''
    iptables -D nixos-fw -p tcp -s ${peirce} --dport ${toString webhookPort} -j nixos-fw-accept || true
  '';
}
