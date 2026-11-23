# IPv4 ranges a confined MicroVM must never reach directly: private LANs,
# CGNAT (which includes the tailnet), link-local, loopback, multicast and the
# unspecified and reserved blocks.
[
  "0.0.0.0/8"
  "10.0.0.0/8"
  "100.64.0.0/10"
  "127.0.0.0/8"
  "169.254.0.0/16"
  "172.16.0.0/12"
  "192.168.0.0/16"
  "224.0.0.0/4"
  "240.0.0.0/4"
]
