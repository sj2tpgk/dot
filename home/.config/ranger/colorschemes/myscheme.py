# This file is part of ranger, the console file manager.
# License: GNU GPL version 3, see the file "AUTHORS" for details.

from __future__ import (absolute_import, division, print_function)

from ranger.colorschemes.default import Default
from ranger.gui.color import *

from logging import getLogger
LOG = getLogger(__name__)

class Scheme(Default):
    progress_bar_color = green

    def use(self, context):
        if context.in_titlebar or context.in_statusbar:
            return Default.use(self, context)

        else:
            # Don't use sym|hardlink hilighting
            link_sav = context.link
            context.link = False

            fg, bg, attr = Default.use(self, context)

            if context.directory and not context.marked and not context.inactive_pane and not context.selected:
                attr &= (~bold)

            if context.tab and context.good:
                fg = black

            if link_sav:
                attr |= (bold | underline) # make links distinguishable (see Note.1)
                if context.bad:
                    fg = magenta

            # Revert
            context.link = link_sav

            return fg, bg, attr

## Note.1
# b.txt = link to a.txt
# I copied b.txt as c.txt without noticing i'm just copying a link.
# Then I removed part of c.txt ==> part of a.txt is removed, not recoverable!

