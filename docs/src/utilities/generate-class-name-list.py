#!/usr/bin/env python
#
# @file   generate-class-name-list.py
# @brief  Generate a list of class names defined in libSBML
# @author Michael Hucka
# @date   Created 2013-12-19
#
# This program takes one argument, the root of the libSBML src/sbml
# directory.  It walks down the directory tree recursively, looking in .h
# files, and in every file it finds, it looks for the string "@class"
# followed by a word.  It extracts the word.  In the end, it prints to
# standard output all the words it found.

import os, sys, re
from os.path import join


#
# Helper functions.
#

def find_classes_in_file(filename):
    classes = []
    stream = open(filename)
    for line in stream.readlines():
        start = line.find('@class')
        if start < 0:
            continue
        # Sometimes we have "@class Name." instead of "@class Name"
        name = re.sub(r'\.', '', line[start + 6:]).strip()
        # Ignore a few cases.
        if name and (not (name.startswith("doc_") or name.startswith("is "))):
            classes.append(name)

    stream.close()
    return classes


def find_classes(files):
    classes = []
    for f in files:
        classes += find_classes_in_file(f)
    return classes


#
# Main driver.
#

def main(args):
    if len(args) != 2:
	print "Must be given one argument: the path to the libSBML src/sbml dir"
        sys.exit(1)

    src_sbml_dir = args[1]

    files = []
    for root, dir, found_files in os.walk(src_sbml_dir):
        for tail in found_files:
            files.append(os.path.join(root, tail))
            files = [f for f in files if f.endswith('.h')]
            files = filter(lambda x: not x.endswith('fwd.h'), files)
            files = filter(lambda x: not x.endswith('ExtensionTypes.h'), files)

    classes = find_classes(files)
    classes = [re.sub('_t', '', c) for c in classes]
    for c in sorted(set(classes)):
        print c



if __name__ == '__main__':
  main(sys.argv)

