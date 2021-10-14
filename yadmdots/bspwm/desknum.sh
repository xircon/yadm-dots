#!/bin/bash

#################################################################
# Script to display desktop name/number - requires dzen2  #
#################################################################

[ ! -f "/usr/bin/dzen2" ] && exit || echo "Dzen2 exists"

# Display Variables:
hgt=40
wid=$((hgt*500/100)) # e.g. 3.5 times the height, will display the words 'Three' &  'Seven' correctly.
time=1
fgcolour="#ffff00"
bgcolour="#000000"
font="-*-monospaced-bold-r-*-*-"$hgt"-*-*-*-*-*-*-*"

# Screen Resolution calculations:
full_res=$(xdotool getdisplaygeometry)
res_wid=${full_res% *}
res_hgt=${full_res#* }

# Do some calculations for x & y co-ordinates:
x=$((a=res_wid-wid, xx=a/2))
y=$((b=res_hgt-hgt-2, yy=b/2))

Deskname=$(bspc query -D -d focused --names)

# Display desktop number centre screen:
echo "$Deskname" | dzen2 -fg $fgcolour -bg $bgcolour -w $wid -x $x -y $y -p $time -fn $font
