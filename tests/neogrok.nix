{ pkgs }:
let
  neogrok = pkgs.callPackage ../pkgs/neogrok/package.nix { basePath = "/search"; };
in
pkgs.runCommand "neogrok-mirror-integration"
  {
    nativeBuildInputs = [
      pkgs.git
      pkgs.zoekt
      pkgs.universal-ctags
      pkgs.curl
      pkgs.python3
    ];
    CTAGS_COMMAND = "${pkgs.universal-ctags}/bin/ctags";
    GOMAXPROCS = "2";
  }
  ''
    export HOME=$TMPDIR/home
    mkdir -p "$HOME" mirrors/github.com/one mirrors/github.com/two
    git init -q -b main work
    git -C work config user.name Test
    git -C work config user.email test@example.invalid
    printf 'package main\nfunc DemoSymbol() {}\n' > work/main.go
    git -C work add main.go
    git -C work commit -qm initial
    git clone -q --bare work mirrors/github.com/one/demo.git
    git clone -q --bare work mirrors/github.com/two/demo.git
    git init -q --bare mirrors/empty.git
    sha256sum mirrors/github.com/one/demo.git/config > before
    bash ${../pkgs/neogrok/index-mirrors.sh} "$PWD/mirrors" "$PWD/cache" https://src.example.test
    sha256sum -c before
    test ! -e cache/repos/github.com/one/demo.git/objects/info/alternates
    test -L cache/repos/github.com/one/demo.git/objects

    zoekt-webserver -listen 127.0.0.1:6070 -index "$PWD/cache/index" -rpc -html=false > zoekt.log 2>&1 &
    backend=$!
    HOST=127.0.0.1 PORT=3081 ORIGIN=http://127.0.0.1:3081 ZOEKT_URL=http://127.0.0.1:6070 \
      ${neogrok}/bin/neogrok > neogrok.log 2>&1 &
    frontend=$!
    trap 'result=$?; if [ "$result" -ne 0 ]; then tail -n 60 zoekt.log neogrok.log; fi; kill $frontend $backend 2>/dev/null || true' EXIT
    for attempt in $(seq 1 60); do
      if curl -fsS -H 'Content-Type: application/json' -d '{}' http://127.0.0.1:3081/search/api/list > repos.json; then break; fi
      sleep 1
    done
    curl -fsSL http://127.0.0.1:3081/search/ > page.html
    curl -fsS -H 'Content-Type: application/json' \
      -d '{"query":"sym:DemoSymbol","contextLines":1,"files":10,"matches":10}' \
      http://127.0.0.1:3081/search/api/search > search.json
    python3 - <<'PY'
    import json, re, subprocess
    page = open('page.html').read()
    assert 'href="/search/repositories' in page, page
    assets = re.findall(r'(?:href|src)="(/search/_app/[^"]+)"', page)
    assert assets, page
    for asset in assets:
        subprocess.run(['curl', '-fsS', 'http://127.0.0.1:3081' + asset], check=True, stdout=subprocess.DEVNULL)
    query_page = subprocess.check_output(['curl', '-fsSL', 'http://127.0.0.1:3081/search/?q=sym%3ADemoSymbol']).decode()
    assert 'DemoSymbol' in query_page and 'main.go' in query_page, query_page
    repos = json.load(open('repos.json'))
    assert repos['kind'] == 'success', repos
    assert {r['name'] for r in repos['results']['repositories']} == {'github.com/one/demo', 'github.com/two/demo'}, repos
    search = json.load(open('search.json'))
    assert search['kind'] == 'success', search
    files = search['results']['files']
    assert len(files) == 2, search
    assert all(f['fileName']['text'] == 'main.go' for f in files), files
    assert all(f['fileUrl'].startswith('https://src.example.test/github.com/') and '/demo.git/tree/main.go/?id=' in f['fileUrl'] for f in files), files
    PY

    # A new mirror commit is picked up through shared refs and objects.
    printf '\nfunc UpdatedSymbol() {}\n' >> work/main.go
    git -C work commit -qam update
    git -C work push -q "$PWD/mirrors/github.com/one/demo.git" main
    bash ${../pkgs/neogrok/index-mirrors.sh} "$PWD/mirrors" "$PWD/cache" https://src.example.test
    sha256sum -c before
    for attempt in $(seq 1 30); do
      curl -fsS -H 'Content-Type: application/json' \
        -d '{"query":"sym:UpdatedSymbol","contextLines":0,"files":10,"matches":10}' \
        http://127.0.0.1:3081/search/api/search > updated.json
      if python3 -c 'import json; x=json.load(open("updated.json")); assert len(x["results"]["files"]) == 1'; then break; fi
      sleep 1
    done
    python3 -c 'import json; x=json.load(open("updated.json")); assert x["results"]["files"][0]["repository"] == "github.com/one/demo"'
    touch $out
  ''
