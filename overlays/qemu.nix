final: prev: {
  # https://nixpk.gs/pr-tracker.html?pr=325059
  qemu = prev.qemu.override { cephSupport = false; };
  qemu_full = prev.qemu_full.override { cephSupport = false; };
}
