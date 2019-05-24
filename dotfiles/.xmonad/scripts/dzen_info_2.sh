#!/bin/sh
fg="#d0d0d0"
bg="#050505"
conky -c ~/.xmonad/scripts/dzenconky_2 | dzen2 -dock -p -ta r -e 'button3=' -fn 'Exo 2-8' -fg "$fg" -bg "$bg" -h 20 -w 650 -x 720 -y 748
