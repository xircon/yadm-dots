#!/bin/zsh

feh --bg-fill ~/wallpaper/bamboo.jpg&

picom --experimental-backends&!

# flashfocus - systemd unit

# Start-up programs as an array:
# sp=("flashfocus"  "numlockx on" "lead" "albert"  "xscreensaver" "d-n-c" "indicator-kdeconnect" "syncclip" "variety" \
    # "autokey-gtk" "pnmixer" "lxpolkit" "fusuma" "qk start" "espanso start" "nm-applet" "cairo-clock" \
    # "picom" "xfce4-power-manager" "cairo-dock" "mate-panel" "skippy-xd-runner --start-daemon")

sp=("flashfocus"  "numlockx on" "lead" "albert"  "xscreensaver" "d-n-c" "indicator-kdeconnect" "syncclip" "variety" \
    "pnmixer" "lxpolkit" "fusuma" "nm-applet" "cairo-clock" "xfce4-power-manager" "mate-panel" "flameshot" "gnome-calendar")


f_kill () {

   for i in "${sp[@]}"
   do
    firstword=${i%% *}
    killall $firstword&
        echo "Killed $i"
    done
}

f_startups () {
    
    for i in "${sp[@]}"
    do
       firstword=${i%% *}
       running=$(pgrep -c $firstword)
       echo $i "|" $firstword >> ~/tttt
       if [ "$running" == "0" ]; then
           echo "Launching $i"
           $i&!
       fi   
    done

    echo "End of Loop"
}

f_fxprofile () {
    #Firefox profile cleaner:
    running=$(pgrep -c vivaldi-stable)
    echo "profile cleaner"
    if [ "$running" == "0" ]; then
        profile-cleaner v &
    fi
}

f_setxkb () {
    setxkbmap -option "terminate:ctrl_alt_bksp"
    setxkbmap -option "caps:none"
    setxkbmap -option "shift:both_capslock"
    setxkbmap -layout gb -option "ctrl:nocaps"
}

f_myxcape () {
    killall xcape&
   
    sleep 1

    xmodmap ~/.Xmodmap&

    pkill -USR1 -x sxhkd &
	killall sxhkd
	sleep 2
	sxhkd &! 

    xcape -e "Hyper_L=space" &
    #xcape -e 'Alt_L=Alt_L|f' &
    #xcape -e 'Control_L=Super_L|1' &
    #xcape -e 'Shift_L=Control_L|space' &
    #xcape -e 'Shift_R=Super_L|m' &
    #xcape -e 'Super_L=Super_L|d' &
}

f_emacs () {
    running=$(pgrep -c emacs)
    if [ "$running" == "0" ]; then
        emacs --daemon&
    fi
    echo "Emacs Running"
}


# case expression in
#     pattern1 )
#         statements ;;
#     pattern2 )
#         statements ;;
#     ...
# esac

f_kill

f_startups

f_setxkb

f_myxcape

f_fxprofile

f_emacs

skippy-xd-runner --start-daemon&
sleep 3

xdo raise -N Mate-panel

notify-send "MyBspwm end."
