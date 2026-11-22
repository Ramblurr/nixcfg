{ pkgs }:
# ts-broker-contract_test.go runs inside ts-egress's egress package, so it
# drives the real decrypter, broker client and Connect store against the
# packaged ts-broker.
pkgs.ts-egress.overrideAttrs (old: {
  pname = "ts-broker-contract";
  postPatch = (old.postPatch or "") + ''
    cp ${./ts-broker-contract_test.go} internal/egress/broker_contract_test.go
  '';
  checkPhase = ''
    runHook preCheck
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    export TS_BROKER=${pkgs.lib.getExe pkgs.ts-broker}
    status=0
    go test -count=1 -v -run '^TestBrokerContract$' ./internal/egress/ > contract.log 2>&1 || status=$?
    cat contract.log
    test "$status" = 0
    grep -q '^--- PASS: TestBrokerContract ' contract.log
    runHook postCheck
  '';
})
