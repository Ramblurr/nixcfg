{
  config,
  lib,
  pkgs,
  ...
}:

{

  services.tabby = {
    enable = true;
    model = "TabbyML/DeepseekCoder-6.7B";
  };
}
