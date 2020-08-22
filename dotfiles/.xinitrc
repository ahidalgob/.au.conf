#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &

xset -b

# we wait 5 seconds to wait for stalonetray to be launched
sleep 5 && dropbox start &
sleep 5 && megasync &
sleep 5 && nm-applet &
sleep 5 && pasystray &
sleep 5 && redshift-gtk &
# sleep 5 && ogstrayindicator &
# sleep 5 && clockify &
sleep 5 && cbatticon &
sleep 5 && dunst &

#sleep 5 && greenclip daemon &

#xscreensaver -no-splash &

#compton -bf -D 5 --shadow-opacity=0 --shadow-radius=0
#picom -b --shadow-opacity=0 --shadow-radius=0 --blur-background
# keep exploring performance tuning aaaandddd make a config file please

bash ~/.au.conf/scripts/setscreens &

setxkbmap us -variant intl
bash ~/.au.conf/scripts/toggleKeyboardLayout

#sleep 1 && compton -b && wpg -s "$(wpg -c)" &
feh --bg-fill 'Downloads/nord/Nord Wallpaper Pack/daniel-leone-v7daTKlZzaw-unsplash [modded].jpg'

exec xmonad
