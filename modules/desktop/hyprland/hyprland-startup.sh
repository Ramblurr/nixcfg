#!/usr/bin/env sh

systemctl --user start hyprland-session.target &
systemctl --user start dynamic-wallpaper@skip.service &
kwalletd5 &
clipman clear --all
wl-paste -t text --watch clipman store &
/run/current-system/sw/bin/mullvad-vpn &
sudo systemctl restart microsocks &
kitty --class startup-kitty --session ~/.config/kitty/kitty.session &
