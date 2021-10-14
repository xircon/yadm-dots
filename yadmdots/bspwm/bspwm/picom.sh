#!/bin/bash
sleep 10
if [[ "$DESKTOP_SESSION" == "/usr/share/xsessions/bspwm" ]]; then
   picom --experimental-backends --backend glx&!
  else
   echo "problem"
fi