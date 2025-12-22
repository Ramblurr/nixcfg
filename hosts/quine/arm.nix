{
  pkgs,
  ...
}:

let
  baseFolder = "/srv/data/media/rips";
  configFolder = "/etc/arm";
  handbrakeArgs = preset: "--preset-import-file '${configFolder}/handbrake/${preset}.json'";
  HB_PRESET_DVD = "MKV 720p30 x265";
  uid = 950;
  HB_PRESET_BD = "MKV 1080p30 x265";
  extraPkgs = [
    pkgs.abcde
    pkgs.ffmpeg-headless
    pkgs.handbrake
    pkgs.cdparanoia
    pkgs.git
    pkgs.hostname
    pkgs.gawk
    pkgs.diffutils
  ];
in
{

  sops.secrets.omdb_api_key = { };

  users.groups.media = {
    gid = 2000;
    members = [ "ramblurr" ];
  };
  users.users.arm = {
    inherit uid;
  };

  systemd.services."arm@".serviceConfig.ReadWritePaths = [
    "/srv/data/media/movies"
    "/srv/data/media/shows"
    "/srv/data/media/audio"
  ];
  systemd.tmpfiles.settings."50-arm-handbrake-presets" = {
    "/srv/data/media/shows".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "/srv/data/media/audio".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "/srv/data/media/movies".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "/srv/data/media".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "${baseFolder}".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "${baseFolder}/transcoded".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "${baseFolder}/raw".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "${baseFolder}/completed".d = {
      mode = "2770";
      user = "ramblurr";
      group = "media";
    };
    "${configFolder}/handbrake"."L+".argument = toString ./handbrake;
    "${baseFolder}/completed/movies"."L+".argument = "/srv/data/media/movies/";
    "${baseFolder}/completed/shows"."L+".argument = "/srv/data/media/shows/";
    "${baseFolder}/completed/audio"."L+".argument = "/srv/data/media/audio/";
  };

  services.automatic-ripping-machine = {
    enable = true;
    enableTranscoding = true;
    settings = {
      DISABLE_LOGIN = true;
      WEBSERVER_IP = "127.0.0.1";
      WEBSERVER_PORT = 27570;
      OMDB_API_KEY = "@OMDB_API_KEY@";
      DATE_FORMAT = "%Y-%m-%d %H:%M:%S";
      RAW_PATH = "${baseFolder}/raw/";
      TRANSCODE_PATH = "${baseFolder}/transcoded/";
      # Media will be put into movies/ and shows/ subdirectories
      COMPLETED_PATH = "${baseFolder}/completed/";
      # TODO: remove when config is final
      LOGLEVEL = "DEBUG";
      DELRAWFILES = false;

      # HandBrake
      inherit HB_PRESET_BD HB_PRESET_DVD;
      HB_ARGS_DVD = handbrakeArgs HB_PRESET_DVD;
      HB_ARGS_BD = handbrakeArgs HB_PRESET_BD;
    };
    abcdeSettings = {
      OUTPUTTYPE = "flac";
      OUTPUTDIR = "${baseFolder}/completed/audio";
      WAVOUTPUTDIR = "${baseFolder}/raw";
      EJECTCD = "y";
      EXTRAVERBOSE = "2";
      MAXPROCS = 12;
      CDDBPROTO = 6;
      NOSUBMIT = "y";
      PADTRACKS = "y";
    };
  };
  systemd.services.armui.path = extraPkgs;
  systemd.services."arm@".path = extraPkgs;
}
