_inputs: final: prev:

{
  lib = prev.lib // {
    my = prev.lib.my // {
      cidrToIp = ip: builtins.head (builtins.split "/" ip);
    };
  };
}
