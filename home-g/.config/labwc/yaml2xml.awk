# Usage: awk -f yaml2xml.awk rc.yaml > rc.xml

BEGIN {
    stk[0] = 0
    sw = 2
}

/^[ \t]*#/ { next }

/:/ {
    # Close end tags
    lev = (match($0, "[^ \t]") - 1) / sw
    # print "** " indnew " " indold "["$0"]"
    while (lev < stk[0] && 0 < stk[0]) {
        print indent((stk[0]-1)*sw) "</" stk[stk[0]] ">"
        stk[0]--
    }

    # Open new tag (and immediately close)
    tagattrs = trim(substr($0, 1, index($0, ":") - 1))
    tag      = trim(substr(tagattrs, 1, match(tagattrs, "[ \t]|$") - 1))
    val      = trim(substr($0, index($0, ":") + 1))

    # print "{" $0 "}{" tagattrs "}{" tag "}{" val "}"
    if (val) {
        print indent(lev*sw) "<" tagattrs ">" val "</" tag ">"
    } else {
        print indent(lev*sw) "<" tagattrs ">"
        stk[++stk[0]] = tag
    }

}

END {
    while (0 < stk[0]) {
        print indent((stk[0]-1)*sw) "</" stk[stk[0]] ">"
        stk[0]--
    }
}

function trim(s) {
    sub("^[ \t]*", "", s)
    sub("[ \t]*$", "", s)
    return s
}

function indent(n,   s) {
    while (n-- > 0) { s = s " " }
    return s
}


