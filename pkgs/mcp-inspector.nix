{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs,
  jq,
  ...
}:
# Source: https://github.com/andoriyu/flakes/blob/501a8d446c40e914a9e1b27f3cd40484807a6848/packages/mcp-inspector/default.nix
# Modified to set PATH to include node (See https://github.com/andoriyu/flakes/pull/258)
buildNpmPackage rec {
  pname = "mcp-inspector";
  version = "0.15.0";

  src = fetchFromGitHub {
    owner = "modelcontextprotocol";
    repo = "inspector";
    rev = version;
    hash = "sha256-ktV8KzqNVkpASJuHNrgD9lJgfk5gxH+Ij43NxD6j6qo=";
  };

  postPatch = ''
    ${jq}/bin/jq '.packages["node_modules/router/node_modules/path-to-regexp"] += {
      resolved: "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-8.2.0.tgz",
      integrity: "sha512-TdrF7fW9Rphjq4RjrW0Kp2AW0Ahwu9sRGTkS6bvDi0SCwZlEZYmcfDbEsTz8RVk0EHIS/Vd1bv3JhG+1xZuAyQ=="
    }' package-lock.json > package-lock.json.new
    mv package-lock.json.new package-lock.json
    sed -i 's/\.allowExcessArguments()/\.allowExcessArguments?.()/g' cli/src/cli.ts
  '';

  makeWrapperArgs = [
    "--prefix PATH : ${
      lib.makeBinPath [
        nodejs
      ]
    }"
  ];

  npmDepsHash = "sha256-r6ZlnBODaLliu2oFOL/2/c8RFlI7T1yUKButNQvwxyI=";

  #doCheck = false;

  meta = with lib; {
    description = "Visual testing tool for MCP servers";
    homepage = "https://github.com/modelcontextprotocol/inspector";
    license = licenses.mit;
    maintainers = [ "andoriyu@gmail.com" ];
    platforms = platforms.all;
  };
}
