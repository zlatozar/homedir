#!/usr/bin/python

import re, os

# ~/.authinfo is encrypted using emacs: M-x epa-encrypt-file
# Do not froget to remove ~/.authinfo after that
def get_password_emacs(machine, login, port):
    s = "machine %s login %s port %s password ([^ ]*)\n" % (machine, login, port)
    p = re.compile(s)
    # Linux
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    # No encryption
    # authinfo = os.popen("cat ~/.authinfo").read()
    return p.search(authinfo).group(1)
