package main

import (
	_ "github.com/caddy-dns/desec"
	caddycmd "github.com/caddyserver/caddy/v2/cmd"
	_ "github.com/caddyserver/caddy/v2/modules/standard"
	_ "github.com/greenpau/caddy-security"
)

func main() {
	caddycmd.Main()
}
