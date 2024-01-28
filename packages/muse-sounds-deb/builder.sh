#!/usr/bin/env bash
source $stdenv/setup

PATH=$dpkg/bin:$PATH

set -ex

echo "START BUILDER"

echo "SRC: $src"
echo "OUT: $out"

pwd

ls -al
find . -maxdepth 2
dpkg -x "$src" unpacked
ls -al
find unpacked -maxdepth 3
mkdir -p "$out/"
cp -r unpacked/* "$out/"
