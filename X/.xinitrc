#!/bin/bash

# Load resources


xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &
xset -b
xset r rate 300 60

nm-applet &
dropbox &
megasync &


#xscreensaver -no-splash &

#if [ -x /usr/bin/nm-applet ] ; then
   #nm-applet --sm-disable &
#fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

compton -b
#xcompmgr &


feh --bg-fill ~/.au.conf/wp.jpg

exec xmonad
