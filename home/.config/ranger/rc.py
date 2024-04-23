# TODO reload all config command
# TODO better priority sorting for WSL -> cmd open

# To get fm in commands.py (unreliable??)
# fm = [x for x in gc.get_objects() if isinstance(x, FM)][0]


## Main function

def rc_py_main():

    # Save your $SHELL, and tell ranger to use /bin/sh for most tasks
    # (fish zsh etc. are slow to start)
    orig_shell = os.environ["SHELL"]
    os.environ["SHELL"] = "/bin/sh"

    # Force 16 colors for preview scripts etc.
    # os.environ["TERM"] = "xterm" # not work; currently doing in fish config

    # Fast executable listing
    import ranger.ext.get_executables
    advice_add(ranger.ext.get_executables, "get_executables_uncached", "override", my_get_executables)

    # Rifle rules
    import ranger.ext.rifle
    def my_rifle_config(*_):
        add_rifle(r'''has cmd.exe = echo "$@" | sed -e 's#/mnt/\(.\)#\1:#' -e 's#/home/[^/]*#c:#' -e 's#.*#"&"#' -e 's#/#\\#g' | xargs cmd.exe /C start ""''')
        add_rifle(r'''has cmd.exe = printf '%s' "$@" | sed 's#/mnt/c/#c:\\#;s#/#\\#g' | xcopy''')
    advice_add(ranger.ext.rifle.Rifle, "reload_config", "after", my_rifle_config)

    # Custom commands
    add_commands(toggle_flat, smart_rename, reload_config)

    # Config
    execute(r'''
    set colorscheme       myscheme
    set show_hidden       on
    set tilde_in_titlebar on
    set shorten_title     3
    set update_title      on
    set preview_max_size  104857600
    ''')

    # Keys
    execute(r'''
    map r      chain draw_possible_programs; console open_with%space
    map f      console scout -lftse%space
    map <f5>   reload_config
    map cw     smart_rename

    map <f8>   chain shell -s mkdir -p /home/${USER}/.trash ; shell -s mv %s /home/${USER}/.trash/
    map d<f8>  console delete
    map zd     console delete
    map zt     console touch%space
    map zm     console mkdir%space

    map zr     chain shell $EDITOR %confdir/rc.py ; reload_config

    # Open bookmark in new tab
    map "<any> chain tab_new ; enter_bookmark %any
    map '<any> enter_bookmark %any
    map '<bg>  draw_bookmarks

    map R      eval fm.open_console("rename " + fm.thisfile.relative_path.replace("%", "%%"), position=7)
    ''')

    # Keys (depends on keyboard layout)
    k = lambda x, y: y if "MYKBD" in os.environ and os.environ["MYKBD"] == "colemakdh" else x
    execute(f"""
    map {k("h", "k")}  move left=1
    map {k("j", "n")}  move down=1
    map {k("k", "e")}  move up=1
    map {k("l", "i")}  move right=1

    map {k("H", "K")}  history_go -1
    map {k("L", "I")}  history_go +1

    map {k("J", "N")}  move down=0.5 pages=True
    map {k("K", "E")}  move up=0.5   pages=True

    map {k("n", "j")}  search_next
    map {k("N", "J")}  search_next forward=False

    map {k("I", "L")}  eval fm.open_console('rename ' + fm.thisfile.relative_path.replace("%", "%%"), position=7)

    map {k("e", "h")}n open_with nano -- "$@"
    """)

    from ranger.ext.get_executables import get_executables
    executables = get_executables()

    # Use /bin/sh for :shell for performance, and original $SHELL for S command.
    execute(f"map S shell {orig_shell}")

    # Copy path etc. using my custom command
    if "xcopy" in executables:
        execute(r'''
        # Better yank
        map yd  shell -sf xcopy %d &
        map yf  shell -sf xcopy %f &
        map yF  shell -sf xcopy %F &
        map yp  shell -sf xcopy %p &
        ''')

    return



## Helpers

import os
from ranger.api.commands import Command

def advice_add(obj, member, where, fun):
    # Emacs advice-add
    if not where in ["after", "before", "override"]:
        raise Exception('`where` must be either "after", "before" or "override"')
    oldfun = getattr(obj, member)
    def newfun(*args):
        if where == "override":
            return fun(*args)
        else:
            if where == "before": fun(*args)
            retval = oldfun(*args)
            if where == "after": fun(*args)
            return retval
    setattr(obj, member, newfun)

def add_commands(*cmds):
    # Register each Command instance in cmds as a new command (with the its name)
    obj = (lambda: None)
    for c in cmds:
        setattr(obj, c.__name__, c)
    fm.commands.load_commands_from_module(obj)

def my_get_executables(*paths):
    # Similar to ranger.ext.get_executables.get_executables_uncached()
    # Returns all executable files in each of the given directories, but:
    # if path is in /mnt/c/ or /mnt/d/, list all .exe files and nothing else, without checking permission (for performance).
    # Looks in $PATH by default.
    from stat import S_IXOTH, S_IFREG
    from os import listdir, environ, stat
    import shlex
    import re
    if not paths:
        try:
            pathstring = environ['PATH']
        except KeyError:
            return ()
        paths = list(set(pathstring.split(':')))

    executables = set()
    for path in paths:
        is_wsl_approx = "/mnt/c/" in path or "/mnt/d/" in path
        try:
            content = listdir(path)
        except OSError:
            continue
        for item in content:
            abspath = path + '/' + item
            # ext = re.findall(r"[^.]*$", item)[0].lower() if "." in item else ""
            # if ext in ["dll", "mof", "nls", "a", "def", "la", "lib", "png", "zip"]:
            #     continue
            if is_wsl_approx: # avoid stat() in wsl
                if item.lower().endswith(".exe"):
                    executables.add(item)
                continue
            try:
                filestat = stat(abspath)
            except OSError:
                continue
            if filestat.st_mode & (S_IXOTH | S_IFREG):
                executables.add(item)
    return executables

def add_rifle(line):
    self2 = fm.rifle
    tests, command = line.split(self2.delimiter1, 1)
    tests = tests.split(self2.delimiter2)
    tests = tuple(tuple(f.strip().split(None, 1)) for f in tests)
    command = command.strip()
    self2.rules.append((command, tests))

def execute(ss):
    for s in ss.split("\n"):
        s = s.strip()
        if s and not s.startswith("#"):
            fm.execute_console(s)


## Commands

class toggle_flat(Command):
    """
    :toggle_flat

    (Custom command)
    Flattens or unflattens the directory view.
    """

    def execute(self):
        if self.fm.thisdir.flat == 0:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = -1
            self.fm.thisdir.load_content()
        else:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = 0
            self.fm.thisdir.load_content()

class reload_config(Command):
    """
    :reload_config

    (Custom command)
    Reload rc.conf
    """

    def execute(self):
        self.fm.execute_console('source %confdir/rc.conf')
        self.fm.execute_console('echo Reloaded %confdir/rc.conf')

class smart_rename(Command):
    """
    :smart_rename

    (Custom command)
    Rename or bulkrename if there are selected files.
    """

    def execute(self):
        if self.fm.thistab.thisdir.marked_items:
            self.fm.execute_console("bulkrename")
        else:
            self.fm.execute_console("console rename ")



## Main program

import traceback
try:
    rc_py_main()
except Exception as e:
    # print(e)
    traceback.print_exc()
    print()
    print("(error in rc.py)")




