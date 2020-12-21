#!/bin/sh

from="$1"
to="$2"
file="$3"

# get current entry in dict (not necessarily exists)
line=$(sed -n "/^$from /p" "$file")

# remove entry if the entry already exists
# note: -n <- true for non-empty strings
#       -z <- true for empty strings
if [[ -n "$line" ]]; then
    # when an entry already exists
    right=$(echo "$line" | sed "s/^$from //")

    sed -i "/^$from /d" "$file"

    if [[ ! "$line" =~ "/$to/" ]]; then
        # when $to is not in the list
        sed -i "1i$from \/$to$right" "$file"
    else
        # when $to is alread in the list
        # remove '/to' from the item
        # /a1/a2/to/a3/  ->  /a1/a2/a3/
        # /a1/a2/a3/     ->  /to/a1/a2/a3/
        # and add '/to' as the first item
        newright="/$to"$(echo "$right" | sed "s/\/$to//")
        sed -i "1i$from $newright" "$file"
    fi
else
    # create completely new entry
    sed -i "1i$from \/$to\/" "$file"
fi
