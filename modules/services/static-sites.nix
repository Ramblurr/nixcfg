{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.staticSites;
  sites = builtins.attrValues cfg.sites;
  enabled = lib.filter (site: site.enable) sites;
  quote = builtins.toJSON;
  safePath =
    value:
    lib.hasPrefix "/" value
    && builtins.match "[A-Za-z0-9/._~-]+" value != null
    && lib.all (part: part != "." && part != "..") (lib.splitString "/" value)
    && !lib.hasInfix "//" value;
  overlaps =
    a: b:
    a == b
    || lib.hasPrefix "${lib.removeSuffix "/" a}/" b
    || lib.hasPrefix "${lib.removeSuffix "/" b}/" a;
  pairs =
    predicate: values: lib.all (a: lib.all (b: a.name == b.name || predicate a b) values) values;
  headerValue = lib.types.addCheck lib.types.str (
    value: value != "" && !lib.hasInfix "\n" value && !lib.hasInfix "\r" value
  );
  cacheHeader = matcher: responseMatcher: value: ''
    header ${matcher} {
      Cache-Control ${quote value}
      match {
        status 2xx
        ${responseMatcher}
      }
    }
  '';
  siteHandler =
    site:
    let
      # Deferred response headers unwind in reverse order: strongest policy first.
      pathKeys = lib.sort (
        a: b:
        if lib.hasSuffix "/*" a != lib.hasSuffix "/*" b then
          !lib.hasSuffix "/*" a
        else
          builtins.stringLength a > builtins.stringLength b
      ) (builtins.attrNames site.cacheControlByPath);
      mimeKeys = lib.sort (
        a: b: if lib.hasSuffix "/*" a != lib.hasSuffix "/*" b then !lib.hasSuffix "/*" a else a < b
      ) (builtins.attrNames site.cacheControlByMimeType);
      pathHeaders = lib.concatMapStringsSep "\n" (
        path: cacheHeader (quote path) "" site.cacheControlByPath.${path}
      ) pathKeys;
      mimeHeaders = lib.concatMapStringsSep "\n" (
        mime:
        cacheHeader "" "header Content-Type ${quote mime}\nheader Content-Type ${quote "${mime};*"}"
          site.cacheControlByMimeType.${mime}
      ) mimeKeys;
    in
    ''
      ${lib.optionalString (site.path != "/") ''
        handle ${quote (lib.removeSuffix "/" site.path)} {
          redir * ${quote site.path} 301
        }
      ''}
      handle_path ${quote "${site.path}*"} {
        route {
          root * ${quote site.directory}
          ${lib.optionalString site.spaFallback "try_files {path} /index.html"}
          ${pathHeaders}
          ${mimeHeaders}
          ${lib.optionalString (site.cacheControl != null) (cacheHeader "" "" site.cacheControl)}
          file_server
        }
      }
    '';
in
{
  options.modules.services.staticSites = {
    sites = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }: {
            options = {
              enable = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Serve this site and allow deployment. Disabling retains its user and files but denies SSH access.";
              };
              name = lib.mkOption {
                type = lib.types.str;
                readOnly = true;
                default = name;
                internal = true;
              };
              domain = lib.mkOption {
                type = lib.types.str;
                description = "Exact public DNS hostname; DNS must already point at the ingress host.";
              };
              path = lib.mkOption {
                type = lib.types.str;
                default = "/";
                description = "URL mount, starting and ending with a slash.";
              };
              deployUser = lib.mkOption {
                type = lib.types.str;
                default = "${name}-deploy";
                description = "Unique rsync-only account for this site.";
              };
              directory = lib.mkOption {
                type = lib.types.str;
                default = "/var/lib/static-sites/${name}";
                description = "Deployment directory. Overrides must reside on an existing persistent mount with safe, non-deployer-owned parents.";
              };
              authorizedKeys = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                description = "SSH public keys permitted to deploy this site; never private keys.";
              };
              spaFallback = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "Serve index.html when a requested file does not exist.";
              };
              cacheControl = lib.mkOption {
                type = lib.types.nullOr headerValue;
                default = "public, max-age=0, must-revalidate";
                description = "Cache-Control for successful file responses; null leaves it unset. Validators remain enabled.";
              };
              cacheControlByMimeType = lib.mkOption {
                type = lib.types.attrsOf headerValue;
                default = { };
                description = "Response MIME overrides, ignoring charset parameters. Exact types precede type/* wildcards.";
              };
              cacheControlByPath = lib.mkOption {
                type = lib.types.attrsOf headerValue;
                default = { };
                description = "Site-relative served paths, after SPA fallback. Exact paths precede longest /* prefixes, then MIME rules and the default.";
              };
            };
          }
        )
      );
    };
    handlersByDomain = lib.mkOption {
      type = lib.types.attrsOf lib.types.lines;
      readOnly = true;
      internal = true;
      description = "Static route handlers for the host's Caddy ingress configuration.";
    };
  };

  config = lib.mkMerge [
    {
      modules.services.staticSites.handlersByDomain = lib.mapAttrs (
        _: entries: lib.concatMapStringsSep "\n" siteHandler entries
      ) (lib.groupBy (site: site.domain) enabled);
    }
    (lib.mkIf (sites != [ ]) {
      assertions = [
        {
          assertion = lib.all (
            site:
            builtins.match "[a-z][a-z0-9-]*" site.name != null
            && builtins.match "[a-z_][a-z0-9_-]{0,30}" site.deployUser != null
            && !builtins.elem site.deployUser [
              "root"
              "caddy"
              "haproxy"
              "nobody"
            ]
          ) sites;
          message = "Static site names and deployment usernames must be safe identifiers, not reserved service accounts.";
        }
        {
          assertion =
            builtins.length (lib.unique (map (site: site.deployUser) sites)) == builtins.length sites;
          message = "Each static site must have a different deployment user, including disabled sites.";
        }
        {
          assertion = lib.all (
            site:
            builtins.match "[a-z0-9]([a-z0-9-]*[a-z0-9])?(\\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+" site.domain
            != null
            && safePath site.path
            && lib.hasSuffix "/" site.path
            && safePath site.directory
            && lib.hasPrefix "/var/lib/" site.directory
            && !lib.hasSuffix "/" site.directory
            && site.directory != "/var/lib/static-sites"
          ) sites;
          message = "Static sites require an exact lowercase DNS hostname, a normalized URL mount ending in /, and a directory beneath /var/lib/.";
        }
        {
          assertion = pairs (a: b: a.domain != b.domain || !overlaps a.path b.path) enabled;
          message = "Static site URL mounts on the same hostname must not overlap.";
        }
        {
          assertion = pairs (a: b: !overlaps a.directory b.directory) sites;
          message = "Static site deployment directories must not overlap.";
        }
        {
          assertion = lib.all (
            site:
            lib.all (mime: builtins.match "[a-z0-9.+-]+/([a-z0-9.+-]+|\\*)" mime != null) (
              builtins.attrNames site.cacheControlByMimeType
            )
            && lib.all (
              path: safePath (lib.removeSuffix "*" path) && (!lib.hasInfix "*" path || lib.hasSuffix "/*" path)
            ) (builtins.attrNames site.cacheControlByPath)
          ) sites;
          message = "Static cache overrides support exact MIME types or type/*, and normalized exact paths or /prefix/*.";
        }
        {
          assertion = lib.all (site: !site.enable || site.authorizedKeys != [ ]) sites;
          message = "Enabled static sites require at least one deployment public key.";
        }
      ];
      users.groups = lib.listToAttrs (map (site: lib.nameValuePair site.deployUser { }) sites);
      users.users = lib.listToAttrs (
        map (
          site:
          lib.nameValuePair site.deployUser {
            isSystemUser = true;
            group = site.deployUser;
            home = "/var/empty";
            createHome = false;
            shell = pkgs.bash;
            openssh.authorizedKeys.keys = lib.optionals site.enable site.authorizedKeys;
          }
        ) sites
      );
      environment.systemPackages = [ pkgs.rsync ];
      environment.persistence."/persist".directories = [ "/var/lib/static-sites" ];
      systemd.tmpfiles.rules = [
        "d /var/lib/static-sites 0755 root root - -"
      ]
      ++ map (site: "d ${site.directory} 0755 ${site.deployUser} ${site.deployUser} - -") sites;
      services.openssh = {
        enable = true;
        extraConfig = lib.concatMapStringsSep "\n" (site: ''
          Match User ${site.deployUser}
            ForceCommand ${
              if site.enable then
                "${lib.getExe pkgs.rrsync} -wo -munge -absolute ${site.directory}"
              else
                "${pkgs.coreutils}/bin/false"
            }
            DisableForwarding yes
            PermitTTY no
            PermitUserRC no
            PasswordAuthentication no
            KbdInteractiveAuthentication no
          Match all
        '') sites;
      };
      systemd.services.caddy.unitConfig.RequiresMountsFor = map (site: site.directory) enabled;
    })
  ];
}
