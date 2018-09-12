#!/bin/sh
fg="#d0d0d0"
bg="#212121"
conky -c ~/.xmonad/scripts/dzenconky_1 | dzen2 -dock -p -ta r -e 'button3=' -fn 'Exo 2-9' -fg "$fg" -bg "$bg" -h 25 -w 650 -x 720 -y 0
