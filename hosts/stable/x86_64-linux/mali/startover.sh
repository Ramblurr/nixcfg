#!/usr/bin/env bash


if [ -d "/home/ramblurr" ];then
  echo "WHAT ARE YOU DOING?"
  exit 1
fi


umount /mnt/boot
swapoff /dev/mapper/cryptswap
cryptsetup close /dev/mapper/cryptswap
cryptsetup close /dev/mapper/cryptkey
zpool destroy rpool
