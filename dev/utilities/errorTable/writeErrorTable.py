#!/usr/bin/env python
import re, os, sys, string
from libsbml import *

def WriteSev(error, line):
	sev = error.getSeverity()
	if (sev == 3):
		line = line + "<td class=\"s-fatal\">F</td>"
	elif (sev == 2):
		line = line + "<td class=\"s-error\">E</td>"
	elif (sev == 1):
		line = line + "<td class=\"s-warning\">W</td>"
	else:
		line = line + "<td class=\"s-na\">N</td>"
	return line

if len(sys.argv) != 1:
  print 'Usage: writeErrorTable.py'
else:
  errfile = open("list.txt")
  output = open("out.txt", "w")
  for decr in errfile.readlines():
    num = int (decr[40:45])
    print num
    blank = decr.find(" ")
    name = decr[0:blank]
    error = SBMLError(num,1,1)
    line = " * <tr><td><code>"
    line = line + name
    line = line + "</code></td><td>"
    line = line + error.getShortMessage() + "</td>"
    line = WriteSev(error, line)
    error = SBMLError(num,1,2)
    line = WriteSev(error, line)
    error = SBMLError(num,2,1)
    line = WriteSev(error, line)
    error = SBMLError(num,2,2)
    line = WriteSev(error, line)
    error = SBMLError(num,2,3)
    line = WriteSev(error, line)
    error = SBMLError(num,2,4)
    line = WriteSev(error, line)
    error = SBMLError(num,3,1)
    line = WriteSev(error, line)
    line = line +"</tr>\n"
    output.write(line)
