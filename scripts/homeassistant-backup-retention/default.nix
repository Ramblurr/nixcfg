{ pkgs }:
pkgs.writers.writePython3Bin "homeassistant-backup-retention" { } (builtins.readFile ./prune.py)
