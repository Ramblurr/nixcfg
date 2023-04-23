#!/usr/bin/env bash


swapoff /dev/mapper/cryptswap
cryptsetup close /dev/mapper/cryptswap
cryptsetup close /dev/mapper/cryptkey
zpool destroy rpool
