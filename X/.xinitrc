#!/bin/bash

# Load resources

xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &
xset -b
xset r rate 300 60

nm-applet &
dropbox start &
megasync &


#xscreensaver -no-splash &

#if [ -x /usr/bin/nm-applet ] ; then
   #nm-applet --sm-disable &
#fi

compton -bf -D 5

feh --bg-fill ~/.au.conf/wp.jpg

exec xmonad
