#!/bin/sh

echo converting $(date)
for i in *.png; do
    convert $i                      ${i%.*}.xpm
    convert $i -channel RGB -negate ${i%.*}-pressed.xpm
    convert $i -colorspace Gray     ${i%.*}-unfocus.xpm
done
