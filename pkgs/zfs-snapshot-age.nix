{
  writeShellApplication,
  zfs,
  coreutils,
}:
writeShellApplication {
  name = "zfs-snapshot-age";
  runtimeInputs = [
    zfs
    coreutils
  ];
  text = ''
    dataset="$1"
    threshold=$(date --date "$2" +%s)
    latest=$(zfs list -Hp -t snapshot -r -o creation -s creation "$dataset" | tail -n 1)
    if [[ ! "$latest" =~ ^[0-9]+$ ]]; then
      echo "FAIL: $dataset has no valid snapshot timestamp" >&2
      exit 1
    fi
    if (( latest <= threshold )); then
      echo "FAIL: $dataset has no snapshot newer than $2" >&2
      exit 1
    fi
    echo "OK: $dataset"
  '';
}
