#!/bin/bash
#NEEDED_WINDOW_CLASS="Navigator.firefox"
#NEEDED_WINDOW_CLASS="konsole.konsole"
#NEEDED_WINDOW_CLASS="konsole.konsole"
NEEDED_WINDOW_CLASS="Alacritty.Alacritty"
LAUNCH_PROGRAM="alacritty"
        
######################################################################################################
NEEDED_WINDOW_WINDOW_ID_HEX=`wmctrl -x -l | grep "${NEEDED_WINDOW_CLASS}" | awk '{print $1}' | head -n 1`
NEEDED_WINDOW_WINDOW_ID_DEC=$((${NEEDED_WINDOW_WINDOW_ID_HEX}))
if [ -z "${NEEDED_WINDOW_WINDOW_ID_HEX}" ]; then
    
    wmctrl -s 0
    ${LAUNCH_PROGRAM}&
    ~/scripts/desknum.sh
    
else

    echo "Found window ID:${NEEDED_WINDOW_WINDOW_ID_DEC}(0x${NEEDED_WINDOW_WINDOW_ID_HEX})"
    ACIVE_WINDOW_DEC=`xdotool getactivewindow`
    if [ "${ACIVE_WINDOW_DEC}" == "${NEEDED_WINDOW_WINDOW_ID_DEC}" ]; then
        xdotool windowminimize ${NEEDED_WINDOW_WINDOW_ID_DEC}
	~/scripts/desknum.sh
     else
         xdotool windowactivate ${NEEDED_WINDOW_WINDOW_ID_DEC}
	 ~/scripts/desknum.sh
    fi
fi

