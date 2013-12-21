#!/usr/bin/env python
#
# @file   c-doc-filter.py
# @brief  Post-process libSBML's source files for creating C docs with Doxygen.
# @author Michael Hucka
# @date   Created 2013-12-19
#
# This filter is hooked into our Doxygen configuration using the INPUT_FILTER
# configuration variable in doxygen-config-c.txt.in.  This means it's called
# on every input file before Doxygen sees it.
#
# The purpose of program is to look inside every comment, and translate class
# names from C to C_t.  It does this by looking at a list of all known
# classes (which is assumed to exist in a file named "class-list.txt" in the
# directory where it is called) and then doing text replacements on the input
# file.  The search tries to be careful to pick up only standalone references
# to the class names, and avoids those preceded by the percent character (%)
# which is Doxygen's symbol-quoting character.
#
# This first checks the environment variable LIBSBML_CLASSES_LIST; if it is
# set, the value is taken to be the path to a file containing the list of
# classes instead of the default file name "class-list.txt".

import sys, string, os, re

#
# Global variables.
#

libsbml_classes = []

#
# Helper functions.
#

def rewrite_references(match):
    body = match.group(1)

    # Replace class name C with C_t, except if the name is preceded by '%'
    # or suffixed with an underscore or ".h", ".cpp", and similar.

    p = re.compile(r'\b(?<!%)(' + '|'.join(libsbml_classes)
                   + r')(?!(\.h|\.cpp|\.c|_))\b')
    contents = p.sub(r'\1_t', body)
    return '/**' + contents + '*/'


def filter_contents (contents):
    global libsbml_classes

    p = re.compile(r'/\*\*(.+?)\*/', re.DOTALL | re.MULTILINE)
    contents = p.sub(rewrite_references, contents)

    return contents


#
# Main driver.
#

def main (args):
    """Usage: c-doc-filter.py  FILE
    """

    global libsbml_classes

    if len(args) != 2:
        print main.__doc__
        sys.exit(1)

    # Check if the environment variable LIBSBML_CLASSES_LIST is set.
    # If it is, use its value as the path to the classes list file.
    # If not, use a default name.

    if os.environ.get('LIBSBML_CLASSES_LIST'):
        classes_list_file = os.environ.get('LIBSBML_CLASSES_LIST')
    else:
        classes_list_file = 'class-list.txt'

    istream         = open(classes_list_file, 'r')
    libsbml_classes = istream.read().splitlines()
    istream.close()

    istream         = open(args[1], 'r')
    contents        = istream.read()
    istream.close()

    sys.stdout.write(filter_contents(contents))
    sys.exit(0)



if __name__ == '__main__':
  main(sys.argv)
