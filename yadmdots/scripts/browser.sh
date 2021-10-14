#!/bin/bash
#NEEDED_WINDOW_CLASS="Navigator.firefox"


#brow="snapshot"
brow="stable"

if [[ "$brow" == "snapshot" ]]; then
    NEEDED_WINDOW_CLASS="Vivaldi-snapshot"
    LAUNCH_PROGRAM="/usr/bin/vivaldi-snapshot"
fi

if [[ "$brow" == "stable" ]]; then
    LAUNCH_PROGRAM="/usr/bin/vivaldi-stable"
    NEEDED_WINDOW_CLASS="Vivaldi.stable"
fi
	
#if [[ $(date +%u) -eq 7 ]] ; then
    #profile-cleaner f
#fi
        
######################################################################################################
NEEDED_WINDOW_WINDOW_ID_HEX=`wmctrl -x -l | grep "${NEEDED_WINDOW_CLASS}" | awk '{print $1}' | head -n 1`
NEEDED_WINDOW_WINDOW_ID_DEC=$((${NEEDED_WINDOW_WINDOW_ID_HEX}))
if [ -z "${NEEDED_WINDOW_WINDOW_ID_HEX}" ]; then
    
    wmctrl -s 2

    if ((`pgrep -c vivaldi` == 0 )); then
    	${LAUNCH_PROGRAM}&
	~/scripts/desknum.sh
	
	 if [[ "$DESKTOP_SESSION" == "/usr/share/xsessions/bspwm" ]]; then
             echo "1"
	     #~/.config/bspwm/desknum.sh&
	 fi
	 
    fi

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

