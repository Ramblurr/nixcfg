{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "bunny-api-proxy";
  version = "2026.02.2";

  src = fetchFromGitHub {
    owner = "sipico";
    repo = "bunny-api-proxy";
    rev = version;
    hash = "sha256-Rok7W+vHK20fAJs+KGwVXAp1B7uFFtgUz/kpA41VfnU=";
  };

  vendorHash = "sha256-WL1FAcWx43ZRHS5t5bTZvsjtOQVCYgzbmF4yeJ8ZzQs=";

  subPackages = [ "cmd/bunny-api-proxy" ];

  checkFlags = [ "-parallel=1" ];

  ldflags = [
    "-s"
    "-w"
  ];

  meta = {
    description = "API proxy for bunny.net with scoped API keys";
    homepage = "https://github.com/sipico/bunny-api-proxy";
    license = lib.licenses.agpl3Only;
    mainProgram = "bunny-api-proxy";
  };
}
