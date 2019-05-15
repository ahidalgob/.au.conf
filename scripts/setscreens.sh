NSCREENS="$(xrandr | grep --count [^dis]connected)"
if [ "$NSCREENS" == "1" ]; then
    xrandr --output eDP1 --auto
else
    xrandr --output HDMI1 --auto --output eDP1 --auto --left-of HDMI1
fi
