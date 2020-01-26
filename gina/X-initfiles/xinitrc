#xhost zeus >/dev/null
xset bc
if xdpyinfo | grep '8 planes' >/dev/null
then
/vol/motif/bin/mwm -name mwm_co 2>/dev/null &
xsetroot -solid gray70
else
/vol/motif/bin/mwm -name mwm_bw 2>/dev/null &
xsetroot -gray
fi
xterm -g 80x24+0+174 -name `hostname`&
xterm -C -g 80x10+0+0 -font fixed -name console
