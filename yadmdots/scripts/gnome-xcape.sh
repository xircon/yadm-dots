#!/bin/zsh

#emacs terminal daemon:
#/usr/bin/emacs -nw --with-profile terml --daemon=term&!

# if [[ "`pgrep -c cairo-dock`" != "1" ]]; then
#     killall cairo-dock &> /dev/null
# fi

if [[ "$DESKTOP_SESSION" == "plasma" ]]; then
    notify-send "Plasma - aborting gnome script"
    exit
fi

#killall nixieclock &> /dev/null

if [[ "`pgrep -c ydotoold`" == "0" ]]; then
    echo ydotoold not running starting...
    ydotoold&
fi

if [[ "`pgrep -c gpaste-daemon`" == "1" ]]; then
   gpaste-client daemon-reexec&
else
    gpaste-client start&
fi

killall xcape

if [[ "$XDG_SESSION_TYPE" != "wayland" ]]; then
    killall fusumsa
    fusuma&!
fi

#sleep 4 

killall sxhkd&

xmodmap ~/.Xmodmap

# Remember .dual-function-keys.yaml !!!!!
xcape -e "Hyper_L=space"&!
xcape -e "Shift_L=Control_L|space"&!

sxhkd -c ~/.config/sxhkd/gnome-sxhkdrc&!

# if [[ "$DESKTOP_SESSION" != "leftwm" ]]; then
#    nixieclock -a -geometry -15-20
#    sleep 2
#    wmctrl -r "nixieclock" -b add,above,sticky
# fi

if [[ "$XDG_SESSION_TYPE" != "wayland" ]]; then
      if [[ "`pgrep -c cairo-dock`" != "1" ]]; then
	  cairo-dock&!
      fi
fi

if [[ "`pgrep -c albert`" == "0" ]]; then
    albert&!
fi

if [[ "`pgrep -c hydroxide`" == "0" ]]; then
    hydroxide imap&!
fi


echo $UDD xxxxxx
if [[ "$UDD" != "UDD" ]]; then
    /usr/bin/emacs -nw --with-profile terml --daemon=term&!
    sudo mount -a
fi
