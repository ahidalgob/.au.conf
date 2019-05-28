#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &

xset -b

# we wait 5 seconds to wait for stalonetray to be launched
sleep 8 && nm-applet &
sleep 8 && dropbox start &
sleep 8 && megasync &

#xscreensaver -no-splash &

compton -bf -D 5

bash ~/.au.conf/scripts/setscreens &

setxkbmap us -variant intl
bash ~/.au.conf/scripts/toogleKeyboardLayout

feh --bg-fill ~/wp.jpg

exec xmonad
