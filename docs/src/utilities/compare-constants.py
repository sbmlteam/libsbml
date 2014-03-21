#!/usr/bin/env python
#
# @file    compare-constants.py
# @brief   Compare the constants in the substitution files to the current list
# @author  Michael Hucka
#
# Usage:
#   compare-constants.py ORIGINAL SUBSITUTION
#
# This program compares the lists of constants in the ORIGINAL file with the
# list in the SUBSITUTION file and reports the differences.  This is used as
# a quick check of the documentation substitution files in java-substitutions
# python-substitutions, and possibly others in the future.
#
# <!--------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2014 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#     3. University of Heidelberg, Heidelberg, Germany
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#  
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
#  
# Copyright (C) 2002-2005 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
# ---------------------------------------------------------------------- -->*/

import sys, re

#
# Globally-scoped variables.
#

# List of symbols that show up in SWIG-generated output but that are internal
# and should not be documented in the user API docs.
#
exclusion_list = ['MathCheckOFF',
                  'MathCheckON',
                  'OverdeterCheckOFF',
                  'OverdeterCheckON',
                  'PracticeCheckOFF',
                  'PracticeCheckON',
                  'SBMLCheckOFF',
                  'SBMLCheckON',
                  'SBOCheckOFF',
                  'SBOCheckON',
                  'UnitsCheckOFF',
                  'UnitsCheckON',
                  'IdCheckOFF',
                  'IdCheckON',
                  'AllChecksON']

#
# Utility functions.
#

def search_file(file, pattern, exclude, results):
    exclude_regexp = r'\A' + r'\Z|\A'.join(exclude) + r'\Z'
    for line in file:
        if pattern.match(line):
            found = pattern.match(line).group(1)
            if not exclude_regexp or not re.search(exclude_regexp, found):
                results.append(found)


#
# Main code.
#

def main (args):
    """usage: compare-constants.py original-file substitution-file
    Figures out the language (Java, Python) from the file name extension.
    """

    global exclusion_list

    original = open(args[1], 'r')
    substitution = open(args[2], 'r')

    if args[1].endswith('.java'):
        pattern = re.compile(r'^\s*public final static \w+ (\w+)')
    elif args[1].endswith('.py'):
        pattern = re.compile(r'\A(\w+) =')
        exclusion_list += ['_swigregister', 'cerr', 'clog', 'cout', 'cvar']

    original_names = []
    substitution_names = []

    search_file(original, pattern, exclusion_list, original_names)
    search_file(substitution, pattern, exclusion_list, substitution_names)

    diff_original = set(original_names) - set(substitution_names)
    diff_substitution = set(substitution_names) - set(original_names)

    if len(diff_original) > 0 or len(diff_substitution) > 0:
        print("")
        print("*"*79)
        print("Warning: differences found in documentation substitution files.")

    if len(diff_original) > 0:
        print("The following exist in " + args[1])
        print("but are missing from " + args[2])
        print("Missing: " + "\nMissing: ".join(sorted(diff_original)))

    if len(diff_substitution) > 0:
        print("")
        print("The following extras exist in " + args[2])
        print("but are not in " + args[1])
        print("Extra: " + "\nExtra: ".join(sorted(diff_substitution)))

    if len(diff_original) > 0 or len(diff_substitution) > 0:
        print("*"*79)


if __name__ == '__main__':
    main(sys.argv)
