#!/usr/bin/awk -f

# Dotfiles installer in POSIX-awk.

# in home/
# recursively search
# if .dot.whole is in directory $d, link $d to $HOME/$d, and do not recurse
# else:
#   for each file link $f to $HOME/$f except:
#     if destination already exists, and not symlink or link target is different, ask remove
#   for each directory recurse
# also mkdir -P
#
# ranger config in init.py and python command to load that file?
# also drag drop via yad

BEGIN {
    system("date")
    fromPath = abso("home/") "/"
    toPath   = abso("awk/")  "/"
    # print ENVIRON["HOME"]
    # rec(fromPath, "")
    # print replacePrefix("abc", "xxx", "1bcdef")
    # print replacePrefix("abc", "xxx", "abcdef")
    print
}

function rec(path, depth,    x, path2) {
    if (isFile(path "/.dot.whole'")) {
        print "  " depth "dot whole"
        link(path)
    } else {
        while (("ls -1A " path | getline x) > 0) {
            path2 = path "/" x
            if (isDirectory(path2)) {
                print "D " depth path2
                rec(path2, depth " ")
            } else {
                print "f " depth path2
                link(path2)
            }
        }
    }
}

function link(absTarget,    absLinkName, resolved, dir) {
    absTarget   = abso(absTarget)
    absLinkName = replacePrefix(fromPath, toPath, absTarget)
    print "FROM " absTarget "   TO " absLinkName

    if (exists(absLinkName)) {

        if (isSymlink(absLinkName)) {
            "readlink -f '" absLinkName "'" | getline resolved
            resolved = abso(resolved)
            print "link exists: resolved='" resolved "' absTarget='" absTarget "' absLinkName='" absLinkName "'"
            if (resolved == absTarget) {
                print "correct link already exists. skipping."
                return
            } else if ("y" == ask("Symlink '" absLinkName "' already exists but points to different path. Remove it? [y/n] ", "y,n")) {
                print "rm '" absLinkName "'"
            } else {
                print "skipping this link."
                return
            }
        } else if (isDirectory(absLinkName)) {
            print "d"
        } else if (isFile(absLinkName)) {
            print "f"
        } else {
            error("link: file of unexpected type exists: '" absLinkName "'")
        }

    }

    if (! isDirectory(dir)) {
        print "mkdir -P '" dir "'"
    }



    if (isSymlink(absLinkName)) {
        "readlink -f '" absLinkName "'" | getline resolved
        print "link exists: resolved='" resolved "' absTarget='" absTarget "' absLinkName='" absLinkName "'"
        if (resolved == absTarget) {
            print "correct link already exists. skipping."
            return
        } else if ("y" == ask("Symlink '" absLinkName "' already exists but points to different path. Remove it? [y/n] ", "y,n")) {
            print "rm '" absLinkName "'"
        } else {
            print "skipping this link."
            return
        }
    } else if (isDirectory(absLinkName)) {
        error("link: trying to create symlink in the name '" absLinkName "', but it is an existing directory")
    }

    if (isFile(absLinkName)) {
        if (ask("File (not symlink) '" absLinkName " exists. Remove it? [y/n] ", "y,n") == "y") {
            print "rm '" absLinkName "'"
        } else {
            print "skipping this link."
            return
        }
    }

    "dirname '" absLinkNameabsLinkName "'" | getline dir
    if (!isDirectory(dir)) {
        print "mkdir -P '" dir "'"
    }
    print "ln -s '" absTarget "' '" absLinkName "'"
    print "linking!"

}

function replacePrefix(from, to, str) {
    # E.g. replacePrefix("ab", "xy", "abcd") ==> "xycd"
    # Error when `from` is not a prefix of `str`.
    # Not using sub() to escape regex in `str` or `from`
    # print "from=[" from "]  to=[" to "]  str=[" str "]  index=" index(str, from)
    if (index(str, from) == 1) {
        return to substr(str, length(from) + 1)
    } else {
        error("replacePrefix: '" from "' is not a prefix of '" str "'")
    }
}

function abso(path,    absPath) {
    # Return absolute, canonicalized path
    # E.g. abso("./a/b/..//c.txt") ==> "/home/user/a/c.txt" (assuming pwd is /home/user)
    "readlink -mf " path | getline absPath
    return absPath
}

function isDirectory(path) { return test("-d", path) }
function exists(path)      { return test("-e", path) }
function isFile(path)      { return test("-f", path) }
function isSymlink(path)   { return test("-L", path) }
function test(flag, arg) {
    # Usage: if (test("-L", "/home/u/link1")) { print "it is a symlink" }
    return 0 == system("test " flag " '" arg "'")
}

function ask(msg, answers,    arr, input, i) {
    # Usage: if (ask("Do you proceed?", "y,n") == "y") { print "ok" }
    split(answers, arr, ",")
    while (1) {
        printf msg
        getline input < "-"
        for (i in arr) {
            if (arr[i] == input) {
                return input
            }
        }
        print "Invalid input"
    }
}

# function join(arr, sep,    joined) {
#     for (i in arr) {
#         if (i > 1) {
#             joined = joined sep
#         }
#         joined = joined arr[i]
#     }
#     return joined
# }

function error(msg) { print "Awk Error: " msg; exit 1 }
