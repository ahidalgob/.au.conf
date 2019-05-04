
if [ "$(setxkbmap -query | grep variant)" ]; then
    setxkbmap us
else
    setxkbmap us -variant intl
fi
