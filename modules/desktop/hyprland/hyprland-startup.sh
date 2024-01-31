#!/usr/bin/env sh
systemctl --user restart dynamic-wallpaper@skip.service
killall -q .kwalletd5-wrap
kwalletd5 &
clipman clear --all
wl-paste -t text --watch clipman store &
killall -q mullvad-gui
/run/current-system/sw/bin/mullvad-vpn &
sudo systemctl restart microsocks &
kitty --class startup-kitty --session ~/.config/kitty/kitty.session &
killall -q .easyeffects-wr
easyeffects --gapplication-service &
systemctl --user restart emacs.service &
systemctl --user restart swayidle.service &
