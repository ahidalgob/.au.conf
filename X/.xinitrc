#!/bin/bash

# Load resources


xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &
#xset -b

# Set up an icon tray
#stalonetray &

# Fire up apps


#xscreensaver -no-splash &

#if [ -x /usr/bin/nm-applet ] ; then
   #nm-applet --sm-disable &
#fi

xset r rate 300 60

compton -b
#xcompmgr &


feh --bg-fill ~/.au.conf/wp.jpg

exec xmonad
