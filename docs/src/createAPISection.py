#!/usr/bin/env python

#
# Filename    : createAPISection.py
# Description : Reads libsbml header files and produces LaTeX
# Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
# Organization: JST ERATO Kitano Symbiotic Systems Project
# Created     : 2003-07-16
# Revision    : $Id$
# Source      : $Source$
#
# Copyright 2002 California Institute of Technology and
# Japan Science and Technology Corporation.
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or
# any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
# documentation provided hereunder is on an "as is" basis, and the
# California Institute of Technology and Japan Science and Technology
# Corporation have no obligations to provide maintenance, support,
# updates, enhancements or modifications.  In no event shall the
# California Institute of Technology or the Japan Science and Technology
# Corporation be liable to any party for direct, indirect, special,
# incidental or consequential damages, including lost profits, arising
# out of the use of this software and its documentation, even if the
# California Institute of Technology and/or Japan Science and Technology
# Corporation have been advised of the possibility of such damage.  See
# the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
#
# The original code contained here was initially developed by:
#
#     Ben Bornstein
#     The Systems Biology Markup Language Development Group
#     ERATO Kitano Symbiotic Systems Project
#     Control and Dynamical Systems, MC 107-81
#     California Institute of Technology
#     Pasadena, CA, 91125, USA
#
#     http://www.cds.caltech.edu/erato
#     mailto:sysbio-team@caltech.edu
#
# Contributor(s):
#

import sys
from string import *


def getDocString (stream):
  """getDocString(stream) -> list

  Returns the list of lines between the start and end of the next
  comment block in the stream.  The comment block must be at the start
  of each line and in the following sytle:

    /**
     * This line would be returned.
     */
  """

  line   = stream.readline()
  inDocs = 0
  result = []

  while (line):
    line = rstrip(line)

    if line == ' */': break
    if inDocs       : result.append(line)
    if line == '/**': inDocs = 1

    line = stream.readline()

  return result


def isPublicFunction (stream):
  """isPublicFunction -> 0 | 1

  If the next line in the stream is the word LIBSBML_EXTERN, returns
  true (1), false (0) otherwise.
  """

  line = stream.readline()
  line = strip(line)
  return (line == "LIBSBML_EXTERN")


def getReturnType (stream):
  """getReturnType(stream) -> string

  Returns the next line in the stream with leading and trailing
  whitespace removed.  If called after isPublicFunction(), this will
  be the function's return type.
  """
  line = stream.readline()
  line = strip(line)
  return line


def getPrototype (stream):
  """getPrototype(stream) -> list

  Returns a list of lines that together constitute the function
  prototype (minus the return type).  The returned list will usually
  contain only a single line, but on occassion, prototypes span
  multiple lines.

  This functions should be called after getReturnType().
  """

  line   = stream.readline()
  result = []

  while (line):
    line = rstrip(line)

    if len(line) > 0 and line[-1] == ';':
      result.append(line[0:-1])
      break
    else:
      result.append(line)

    line = stream.readline()

  return result


def escapeUnderscores(string):
  """escapeUnderscore(string) -> string

  Escapes underscores for LaTeX.  Returns the string with any
  undercores replaced by '\_'.
  """

  return replace(string, '_', '\_')


def filterDocStringLine(string):
  """filterDocStringLine(string) -> string

  Filters a line of a docstring by performing the following substitutions:

    *        ->  (empty)
    %        ->  \%
    @return  ->  Returns
    <b>      ->  \textbf{
    </b>     ->  }
  """

  string = replace( string, '*'      , ''          )
  string = replace( string, '%'      , '\%'        )
  string = replace( string, '@return', 'Returns'   )
  string = replace( string, '<b>'    , '\\textbf{' )
  string = replace( string, '</b>'   , '}'         )

  index = find(string, ';')
  if index > 0 and string[0:3] == '   ':
    string = '\\texttt{' + string + '}'

  return string


def getBasename(filename):
  """getBasename(filename) -> basename

  Returns filename with any prefix and extension removed.
  """

  slash = rfind(filename, '/')
  dot   = rfind(filename, '.')

  if slash > 0: filename = filename[slash + 1:]
  if dot   > 0: filename = filename[0:dot]

  return filename


def TeXifyHeaderFile(filename):
  """TexifyHeaderFile(filename)

  Reads the given header file and prints out a corresponding LaTeX
  subsection for the public APIs.  The LaTeX codes emitted assume the
  style file cekmanual.
  """

  basename  = getBasename(filename)
  stream    = open(filename)

  separator = '%' + ('-' * 77)
  print separator
  print '\\subsection{%s}' % (basename,)
  print separator
  
  line = stream.readline()

  while(line):
    docstring = getDocString(stream)

    if isPublicFunction(stream):
      type      = escapeUnderscores( getReturnType(stream) )
      prototype = map( escapeUnderscores, getPrototype(stream) )
      docstring = map( escapeUnderscores, docstring            )
      docstring = map( filterDocStringLine, docstring          )

      print '\\begin{methoddef}{%s %s}' % (type, join(prototype))
      print join(docstring, '\n')
      print '\\end{methoddef}'
      print '\n'

    line = stream.readline()

  stream.close()


#
# Main
#
if __name__ == '__main__':

  if len(sys.argv) < 2:
    print '\n  Usage: %s c-header-filename\n' % (sys.argv[0], )
  else:
    TeXifyHeaderFile(sys.argv[1])
