#!/bin/bash

if [ "$(setxkbmap -query | grep variant)" ]; then
    setxkbmap
    setxkbmap us
else
    setxkbmap
    setxkbmap us -variant intl
fi

setxkbmap -option caps:swapescape # swaps escape and caps

#TODO detect

THINKPAD="$(lsusb | grep --count Lenovo)"
if [ "$THINKPAD" == "0" ]; then
    xmodmap -e "clear control"

    xmodmap -e "keycode 133 = Escape"
    xmodmap -e "keycode 105 = Left"
    xmodmap -e "keycode 113 = Control_R"
    xmodmap -e "add control = Control_R Control_L"

    #xmodmap -e "keycode 108 = Overlay1_Enable"
    #xmodmap -e "keycode 108 = Mode_switch"
    #xmodmap -e "keysym h = h H Left"
    #xmodmap -e "keysym j = j J Down"
    #xmodmap -e "keysym k = k K Up"
    #xmodmap -e "keysym l = l L Right"
fi

xset r rate 300 60
