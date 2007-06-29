#!/usr/bin/python
#
# @file    swigdocpostprocess.py
# @brief   Post-process libSBML's Python doc strings for use by Doxygen.
# @author  Michael Hucka
#
# $Id$
# $Source$
#
# Usage: swigdoc.py < libsbml.py > libsbml-for-doxygen.py
#
# This filter reads the standard input (on which it expects the libsbml.py
# file produced by <libsbml>/src/bindings/swig/swigdoc.py), cooks the
# contents for use with Doxygen, and writes the results to standard output.
# This is designed to be used as the value of the INPUT_FILTER
# configuration variable in Doxygen.  The need for this is to do additional
# transformations that can't be done in swigdoc.py because they rely on
# having in hand the final output from SWIG.
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2007 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#----------------------------------------------------------------------- -->*/

import sys, string, os.path, re


def reformatDocString (match):
  text = match.group(1)

  # The following are the bits we construct, assigned to variables to
  # make the following code slightly more readable

  intro  = '<span class="signatureTitle">Python method signature(s)</span>:\n'
  sStart = '<pre class="signature">'
  sEnd   = '</pre>'

  # Regexp for one or more signatures of the form
  #    foo(arg, arg, ...) -> result(arg, arg, ...)
  #    bar(arg, arg ...) -> result(arg, arg, ...)
  # This relies on the fact that the first line(s) (up to a blank line) in
  # every method documentation string is the signature of the method.
  # The \A at the beginning of the regexp forces it that way.

  sigRE = '\A(\s*)((\w+\([\w()=:"<>?, ]+\)( -> [\w()=:"<>?|, \t]+)?\s*)+)'

  # This matches when the signatures are the only thing in a docstring.
  p = re.compile(sigRE + '\Z', re.MULTILINE)
  text = p.sub(r'\1' + intro + r'\1' + sStart + r'\2' + sEnd, text)

  # This matches when the signatures are followed with more text.
  p = re.compile(sigRE + '^\s*$', re.MULTILINE)
  text = p.sub(r'\1' + intro + r'\1' + sStart + r'\2' + sEnd + r'\1<p>', text)
  
  # Prettify the arrow:
  newArrow = '@htmlonly ' + \
             '<img class="signatureArrow" src="right-arrow.gif"> ' + \
             '@endhtmlonly'
  p = re.compile(' -> ')
  text = p.sub(r' ' + newArrow + ' ', text)

  # Crucial detail: need a '!' character after the opening triple quotes or
  # else Doxygen puts the entire docstring inside a verbatim environment.

  return "\"\"\"!" + text + "\"\"\""


def filterDocStrings (contents):
  p = re.compile('\"\"\"(.+?)\"\"\"', re.DOTALL | re.MULTILINE)
  contents = p.sub(reformatDocString, contents)

  return contents


def filterContents(contents):
  """
  filterContents(contents) -> contents

  Makes overall adjustments to Python definitions in the input, to allow
  Doxygen to recognize the content correctly.  This method handles things
  outside the docstrings; see filterDocStrings() for the method that
  handles the contents of docstrings.  These adjustments are really only
  needed because Doxygen is quirky about how it looks for some things.
  """

  # The following removes the space after the ':' in an __init__ definition
  # because otherwise, Doxygen 1.4.5's parser fails to match __init__
  # properly.  Clearly it's a bug in Doxygen.
  contents = re.sub('def __init__\(([^)]+)\): \n',
                    r'def __init__(\1):\n', contents)
  return contents


def filterForDoxygen (contents):
  """
  filterForDoxygen(contents) -> contents

  Massage the content of a python file to better suit Doxygen's expectations.
  """
  contents = filterContents(contents)
  contents = filterDocStrings(contents)
  return contents


def main (args):
  """Usage: swigdoc.py libsbml.py > output.py

  libsbml.py    pathname to the libsbml.py file produced by SWIG.

  This cooks the final output of our swigdoc.py + SWIG sequence for use
  with doxygen, to do additional transformations that can't be done in
  swigdoc.py because they rely on having in hand the final output from
  SWIG.
  """

  if len(args) != 2:
    print main.__doc__
    sys.exit(1)

  # If the file name doesn't end in .py, skip it:
  if not re.search('.py$', args[1]):
    sys.exit(0)

  istream    = open(args[1], 'r')
  contents   = istream.read()
  istream.close()

  sys.stdout.write(filterForDoxygen(contents))


if __name__ == '__main__':
  main(sys.argv)


# Misc. code from previous iterations that I don't want to throw
# away just yet:
# 
#  text = p.sub(r'\1<table class="signatureTable"><tr><td class="signatureIntro"><b>Python method signature(s)</b>:</td><td class="signatureBlock">\2</td></tr></table>\1<p>', text)
