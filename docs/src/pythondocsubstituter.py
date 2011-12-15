#!/usr/bin/env python

import re
import sys

def getFunctionNames(filename):
    file = open(filename, 'r')
    functions = []

    for line in file.readlines():
        if re.search(r'\Adef ', line):
            functions.append(line[4:line.rfind('(')])

    file.close()
    return functions


def main (args):
    """Usage:
        pythondocsubstituter.py sourcelibsbml.py outputlibsbml.py FILE FILE FILE ...
    """

    if len(sys.argv) < 4:
        print(main.__doc__)
        sys.exit(1)

    input     = open(args[1], 'r')
    output    = open(args[2], 'w')
    filenames = args[3:]

    classes   = []
    functions = []

    for f in filenames:
        if re.search('libsbml.py', f) != None:
            functions = getFunctionNames(f)
        else:
            classes.append(f[f.rfind('/') + 1 : f.rfind('.')])

    print "Processing " + args[1]
    print "Will remove the following classes: " + ", ".join(classes)
    print "Will remove the following functions: " + ", ".join(functions)

    skipping = False
    for line in input:
        if skipping and re.search(r'\A\S', line):
            skipping = False
        if skipping == False:
            for c in classes:
                if re.search(r'\Aclass ' + c + '\([^)]+\):', line):
                    skipping = True
                    continue
        if skipping == False:
            for f in functions:
                if re.search(r'\Adef ' + f, line):
                    skipping = True
                    continue
        if skipping:
            continue
        else:
            output.write(line)

    print "Will substitute the contents from the following files: " + " ".join(filenames)
    for f in filenames:
        contents = open(f, 'r').read()
        output.write(contents)

    print "Output left in " + args[2]


if __name__ == '__main__':
  main(sys.argv)
