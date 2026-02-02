{
  lib,
  python3Packages,
  fetchFromGitHub,
}:

python3Packages.buildPythonApplication {
  pname = "youtube-to-rss";
  version = "unstable-2026-01-30";

  src = fetchFromGitHub {
    owner = "johnnysteen";
    repo = "youtube-to-rss";
    rev = "e6aa82ab1ea68d22b70b09e1ba14431f389b9c80";
    hash = "sha256-m/au653SyQgS3PLRk+742bvOwi5zFI2yoIZPDK3XhXk=";
  };

  pyproject = true;

  build-system = [ python3Packages.setuptools ];

  dependencies = [ python3Packages.pyyaml ];

  meta = {
    description = "Download video feeds with yt-dlp and publish them as podcast RSS feeds";
    homepage = "https://github.com/johnnysteen/youtube-to-rss";
    license = lib.licenses.mit;
    mainProgram = "y2r";
  };
}
