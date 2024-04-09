{ lib, ... }:

{
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
}
