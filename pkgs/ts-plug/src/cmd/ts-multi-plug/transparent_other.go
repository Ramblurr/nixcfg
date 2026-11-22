//go:build !linux

package main

import "errors"

var errTransparentUnsupported = errors.New("transparent capture needs Linux")

func startTransparentTCP(string, netstackDial, func(error)) error {
	return errTransparentUnsupported
}

func startTransparentUDP(string, netstackDial, func(error)) error {
	return errTransparentUnsupported
}
