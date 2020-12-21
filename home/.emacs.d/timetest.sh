#!/bin/sh

for i in $(seq 1 20); do
    en --eval '(kill-emacs)'
done
