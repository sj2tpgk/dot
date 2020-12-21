#!/bin/sh

newentry_file="/home/tpat3/.uim.d/newentry"
dict_file="/home/tpat3/.uim.d/uimskk_jisyo/skk-uim-jisyo"

from=$(sed '1q;d' "$newentry_file")
to=$(sed '2q;d' "$newentry_file")
sh /home/tpat3/.uim.d/addentry_helper.sh "$from" "$to" "$dict_file"
# newentry="$1"
# sed -i "1i$newentry" "$dict_file"
