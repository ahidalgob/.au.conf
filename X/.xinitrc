#!/bin/bash

# Load resources


xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &

# Set up an icon tray
#stalonetray &

# Fire up apps


xscreensaver -no-splash &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

xset r rate 300 60

compton -b
#xcompmgr &


feh --bg-fill ~/Dropbox/wallhaven-369261.jpg
#feh --bg-fill ~/Pictures/Wolfmother-logo-HD-wallpaper-1600x900.jpg
#feh --bg-scale ~/Pictures/futuristic-city-wallpaper-10.jpg

exec xmonad
