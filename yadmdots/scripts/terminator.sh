#!/bin/bash
#pkill compton
######################################################################################################
# This script will toggle minimize/activate first window with specified class
# If window not found program will be launched
#
# window class can be found with next programs:
#   wmctrl -x -l
#   xprop
# No credit taken.......... Cannot read the original.....
# Found on http://blog.sokolov.me/2014/06/20/linuxx11-toggle-window-minimizemaximize/
# in Russian :) but works when adjusting the wrapping.
# Assigned to meta-f in KDE plasma 5
######################################################################################################
#crx_mpognobbkildjkofajifpdfhcoklimli.Vivaldi-snapshot
#NEEDED_WINDOW_CLASS="terminator.Terminator"
#NEEDED_WINDOW_CLASS="termite.Termite"
#NEEDED_WINDOW_CLASS="kitty.kitty"
#NEEDED_WINDOW_CLASS="sakura.Sakura"
#NEEDED_WINDOW_CLASS="st.St"
#NEEDED_WINDOW_CLASS="xst-256color.xst-256color"
NEEDED_WINDOW_CLASS="konsole.konsole"
#NEEDED_WINDOW_CLASS="Alacritty.Alacritty"


#LAUNCH_PROGRAM="/usr/bin/terminator"
#LAUNCH_PROGRAM="st -g 130x30 -e tmux"
#LAUNCH_PROGRAM="kitty -e tmux"
## Tmux debug
#LAUNCH_PROGRAM="konsole -e tmux"
## Tmux normal:

#tmux new-session -s foo "stty -ixon; vim"
wmctrl -s 0
#LAUNCH_PROGRAM="alacritty -e tmux"
LAUNCH_PROGRAM="konsole -e tmux"

######################################################################################################
NEEDED_WINDOW_WINDOW_ID_HEX=`wmctrl -x -l | grep ${NEEDED_WINDOW_CLASS} | awk '{print $1}' | head -n 1`
NEEDED_WINDOW_WINDOW_ID_DEC=$((${NEEDED_WINDOW_WINDOW_ID_HEX}))
echo $NEEDED_WINDOW_WINDOW_ID_HEX $NEEDED_WINDOW_WINDOW_ID_DEC
#ntfy send "echo pgrep - `pgrep -c alacritty`"
if [ -z "${NEEDED_WINDOW_WINDOW_ID_HEX}" ]; then

	if [ $(pgrep -c konsole) == "0" ]; then
    	    ${LAUNCH_PROGRAM}
	fi

	if [[ "$XDG_CURRENT_DESKTOP" != "GNOME" ]]; then
		 ~/scripts/desknum.sh&
	fi	
else
    echo "Found window ID:${NEEDED_WINDOW_WINDOW_ID_DEC}(${NEEDED_WINDOW_WINDOW_ID_HEX})"
    ACIVE_WINDOW_DEC=`xdotool getactivewindow`
    if [ "${ACIVE_WINDOW_DEC}" == "${NEEDED_WINDOW_WINDOW_ID_DEC}" ]; then
        xdotool windowminimize ${NEEDED_WINDOW_WINDOW_ID_DEC}
	~/scripts/desknum.sh
    else
        xdotool windowactivate ${NEEDED_WINDOW_WINDOW_ID_DEC}
	if [[ "$XDG_CURRENT_DESKTOP" != "GNOME" ]]; then
	   ~/scripts/desknum.sh&
	fi	
    fi
fi
