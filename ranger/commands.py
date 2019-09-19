from ranger.api.commands import Command
import os

# Absolute path of histfile
HISTFILE = os.path.expanduser("~/.ranger_history")
HISTSIZE = 200

class add_history(Command):
    def execute(self):
        f=open(HISTFILE, 'a+')
        f.write(self.fm.thisfile.path + '\n')

from subprocess import PIPE
class fzf_cd(Command):
    def execute(self):
        command="find -L \( -path '*.wine-pipelight' -o -path '*.ivy2*' -o -path '*.texlive*' -o -path '*.git' -o -path '*.metadata' -o -path '*_notes' \) -prune -o -type d -print 2>/dev/null | fzf"
        do_fzf(self.fm, command)

class fzf_history(Command):
    def execute(self):
        import os
        rh=HISTFILE
        rht=rh+".tmp"
        do_fzf(self.fm, f"cat {rh} | tail -n {HISTSIZE} | sort | uniq -u > {rht}; mv {rht} {rh}; cat {rh} | fzf --tac")

class fzf_here(Command):
    def execute(self):
        import os
        do_fzf(self.fm, "fzf --tac") # --tac : reverse input lines

def do_fzf(fm, cmd):
    fzf = fm.execute_command(cmd, stdout=PIPE)
    stdout, stderr = fzf.communicate()
    path = stdout.decode('utf-8').rstrip('\n')
    fm.select_file(path) # This automatically does cd
