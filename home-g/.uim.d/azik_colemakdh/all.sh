#!/bin/sh

# In gnumeric, select Data - Export - CSV
# Then run this script

# Need to manually import rules-mozc.txt in mozc config dialog.

# gnumeric -> csv -> csv-sexp -> rules.scm   and   rules-uim-utf8.scm
#                                 v                 v
#                                rules-mozc.txt    rules-uim-eucjp.scm 

scheme csv-sexp.scm table.csv table.scm
scheme make-rulelist.scm table.scm rules.scm rules-uim-utf8.scm
iconv -f utf8 -t eucjp rules-uim-utf8.scm > rules-uim-eucjp.scm
scheme make-mozc-rulefile.scm rules.scm rules-mozc.txt
../restartuim.sh
