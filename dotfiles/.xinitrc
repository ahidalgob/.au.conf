#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Nice cursor
xsetroot -cursor_name left_ptr &

xset -b

# we wait 5 seconds to wait for stalonetray to be launched
sleep 5 && dropbox start &
sleep 5 && megasync &
#sleep 5 && TogglDesktop &

#xscreensaver -no-splash &

#compton -bf -D 5 --shadow-opacity=0 --shadow-radius=0
#picom -b --shadow-opacity=0 --shadow-radius=0 --blur-background
# keep exploring performance tuning aaaandddd make a config file please

bash ~/.au.conf/scripts/setscreens &

setxkbmap us -variant intl
bash ~/.au.conf/scripts/toggleKeyboardLayout

#compton -bf -D 5 --shadow-opacity=0 --shadow-radius=0
#compton -b --shadow-opacity=0 --shadow-radius=0 --blur-background --blur-method kawase --backend glx --blur-strength 12 --glx-no-stencil --glx-no-rebind-pixmap --use-ewmh-active-win --refresh-rate=0
compton -b

feh --bg-fill ~/wp.jpg

exec xmonad
