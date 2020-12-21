from ranger.api.commands import Command
import os

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
    Reload rc.conf and rifle.conf
    """

    def execute(self):
        for cmd in ["source %confdir/rc.conf",
                    "source %confdir/rifle.conf",
                    "echo Reloaded rc.conf and rifle.conf"]:
            self.fm.execute_console(cmd)


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
