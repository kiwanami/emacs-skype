#!/usr/bin/python

import sys
import Skype4Py

skype = Skype4Py.Skype()
try:
    skype.Attach()
    sys.stdout.write("OK\n")
    sys.stdout.flush()
except:
    sys.stdout.write("NG\n")
    sys.stdout.flush()
    sys.exit(1)

while True:
    line = sys.stdin.readline()
    if not line: break
    cmd = skype.Command(line.rstrip(), Block=True)
    skype.SendCommand(cmd)
    sys.stdout.write(cmd.Reply.encode("utf-8") + "\n")
    sys.stdout.flush()
