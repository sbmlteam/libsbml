#!/usr/bin/env python

import re
import sys

def main (args):
    """Usage:
        pythondoctrimmer.py sourcelibsbml.py outputlibsbml.py CLASS CLASS CLASS ...
    """

    if len(sys.argv) < 4:
        print(main.__doc__)
        sys.exit(1)

    input     = open(args[1], 'r')
    output    = open(args[2], 'w')
    filenames = args[3:]

    classes = []
    for f in filenames:
        classes.append(f[f.rfind('/') + 1 : f.rfind('.')])

    print "Processing " + args[1] + " to remove classes " + ", ".join(classes)

    skipping = False
    for line in input.readlines():
        if skipping and re.search(r"\A\S", line):
            skipping = False
            output.write(line)
            continue
        if skipping == False:
            for c in classes:
                if re.search(r"\Aclass " + c + "\(_object\):", line):
                    skipping = True
                    continue
        if skipping:
            continue
        else:
            output.write(line)

    print "Appending the following files: " + " ".join(filenames)
    for f in filenames:
        contents = open(f, 'r').read()
        output.write(contents)

    print "Output left in " + args[2]


if __name__ == '__main__':
  main(sys.argv)
