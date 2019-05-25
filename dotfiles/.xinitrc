#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &

xset -b

# we wait 5 seconds to wait for stalonetray to be launched
sleep 5 && nm-applet &
sleep 5 &&dropbox start &
sleep 5 &&megasync &

#xscreensaver -no-splash &

compton -bf -D 5

bash ~/.au.conf/scripts/setscreens.sh
feh --bg-fill ~/wp.jpg

exec xmonad
