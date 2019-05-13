#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &
xset -b
xset r rate 300 60

# we wait 5 seconds to wait for stalonetray to be launched
sleep 5 && nm-applet &
sleep 5 &&dropbox start &
sleep 5 &&megasync &

#xscreensaver -no-splash &

compton -bf -D 5

xrandr --output HDMI1 --auto --output eDP1 --auto --left-of HDMI1
feh --bg-fill ~/.au.conf/wp.jpg

exec xmonad
