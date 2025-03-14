#!/bin/sh

FILE_PATH="${1}"         # Full path of the highlighted file

FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"

case "$FILE_EXTENSION_LOWER" in
    (a|ace|alz|arc|arj|bz|bz2|cab|cpio|deb|gz|jar|lha|lz|lzh|lzma|lzo|rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z|zip|rar|7z)
        if [ "$( stat --printf='%s' -- "${FILE_PATH}" )" -ge 100000000 ]; then
            # 100 Mega Bytes archive
            echo "Large Archive"
            exit 0 # Display no preview at all
        fi
        ;;
esac

timeout 1 "${RANGER_SCOPE:-/usr/share/doc/ranger/config/scope.sh}" "$@"
