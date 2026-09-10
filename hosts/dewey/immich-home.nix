{ ... }:
{
  # The private wrapper supplies the evaluated, secret-aware guest configuration.
  microvm.vms.immich-home = {
    autostart = true;
    restartIfChanged = true;
  };
}
