#!/usr/bin/bash

# Autogenerate some icon BMPs.
# Edit by hand: close.bmp icon.bmp max.bmp stick.bmp stuck.bmp

srcdir="editbyhand"

# BMP file names (WITHOUT '.bmp')
close="close"
icon="icon"
max="max"
stick="stick"
stuck="stuck"

cp $srcdir/$close.bmp $close.bmp
cp $srcdir/$icon.bmp $icon.bmp
cp $srcdir/$max.bmp $max.bmp
cp $srcdir/$stick.bmp $stick.bmp
cp $srcdir/$stuck.bmp $stuck.bmp

grayscale(){
    convert ${1}.bmp -type GrayScale ${2}.bmp
}
negate(){
    convert ${1}.bmp -negate ${2}.bmp
}

# close
grayscale $close ${close}-unfocus
perl negate_except_round_corner.pl ${close}.bmp ${close}-pressed.bmp "topRight"

# icon
grayscale $icon ${icon}-unfocus
negate $icon ${icon}-pressed

# max
grayscale $max ${max}-unfocus
negate $max ${max}-pressed

# stick
grayscale $stick ${stick}-unfocus
perl negate_except_round_corner.pl ${stick}.bmp ${stick}-pressed.bmp "topLeft"

# stuck
grayscale $stuck ${stuck}-unfocus
perl negate_except_round_corner.pl ${stuck}.bmp ${stuck}-pressed.bmp "topLeft"


# generate XPMs

for i in *.bmp; do
  xpm=$(basename $i .bmp).xpm
  convert "$i" "$xpm"
done
