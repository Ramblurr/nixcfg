# vpn-gateway

## Flake Install

1. Create Hetzner Cloud VM with Ubuntu
2. Run [nixos infect](https://github.com/elitak/nixos-infect/tree/master#hetzner-cloud)

   ``` yaml
   #cloud-config

   runcmd:
     - curl https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect | PROVIDER=hetznercloud NIX_CHANNEL=nixos-22.11 bash 2>&1 | tee /tmp/infect.log
   ```
