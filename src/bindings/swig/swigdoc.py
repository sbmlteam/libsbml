#!/usr/bin/env python

#
# \file   swigdoc.py
# \brief  Creates SWIG docstrings for the Java and Python langauge modules.
# \author Ben Bornstein
#
# $Id$
# $Source$
#

#
# Copyright 2005 California Institute of Technology and
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
#     
#     The SBML Team
#     Control and Dynamical Systems, MC 107-81
#     California Institute of Technology
#     Pasadena, CA, 91125, USA
#
#     http://sbml.org
#     mailto:sbml-team@caltech.edu
#
# Contributor(s):
#   Christoph Flamm - Code to generate Perl docs
#


import sys, string, os.path, re


class CHeader:
  """CHeader encapsulates the C++ class and C function definitions
  found within a C header file.  It has the following public
  attributes:

    - classes
    - functions
  """

  def __init__ (self, stream):
    """CHeader (stream=None) -> CHeader

    Creates a new CHeader reading from the given stream.
    """
    self.classes   = [ ]
    self.functions = [ ]

    if stream is not None:
      self.read(stream)


  def read (self, stream):
    """read (PushBackStream)

    Reads a C/C++ header file from the given stream.
    """
    inClass = False
    inDocs  = False
    inFunc  = False
    inSkip  = False

    docstring = ''
    lines     = ''

    for line in stream.xreadlines():
      stripped = line.strip()

      if stripped      == '#ifndef SWIG': inSkip = True
      if stripped[0:6] == '#endif'      : inSkip = False
      if inSkip: continue

      if stripped == '':
        docstring = ''
        lines     = ''
        inDocs    = False
        inFunc    = False
        continue

      if stripped.startswith('class ') and not stripped.endswith(';'):
        inClass   = True
        classname = line[6:].split(':')[0].strip()
        self.classes.append( CClass(classname) )
        continue

      if stripped == '};':
        inClass = False
        continue

      if inDocs or stripped == '/**':
        docstring += line
        inDocs     = (stripped != '*/')
        continue

      if stripped == 'LIBSBML_EXTERN':
        inFunc = True
        continue

      if inFunc:
        lines += line

      if inFunc and stripped.endswith(';'):

        stop = lines.rfind('(')
        name = lines[:stop].split()[-1]
        func = CFunction(docstring, name)

        if inClass:
          self.classes[-1].methods.append(func)
        else:
          self.functions.append(func)



class CClass:
  """A CClass encapsulates a C++ class.  It has the following public
  attributes:

    - name
    - methods
  """

  def __init__ (self, name):
    """CClass(name) -> CClass

    Creates a new CClass with the given name.
    """
    self.name    = name
    self.methods = [ ]



class CFunction:
  """A CFunction encapsulates a C/C++ function.  Currently, it has the
  following public attributes:

    - docstring
    - name
  """

  def __init__ (self, docstring, name):
    """CFunction(docstring, name) -> CFunction

    Creates a new CFunction with the given docstring and name.
    """
    self.docstring = docstring
    self.name      = name



def getHeadersFromSWIG (filename):
  """getHeadersFromSWIG (filename) -> (filename1, filename2, .., filenameN)

  Reads the list of %include directives from the given SWIG (.i).  The
  list of C/C++ headers (.h) included is returned.
  """
  stream = open(filename)
  lines  = stream.readlines()

  lines  = filter(lambda line: line.strip().startswith('%include'), lines)
  lines  = filter(lambda line: line.strip().endswith('.h')        , lines)
  lines  = map(lambda line: line.replace('%include', '').strip(), lines)

  stream.close()

  return lines;



def processHeader (filename, ostream, language):
  """processHeader (filename, ostream, language='java'|'python')

  Reads the the given header file and writes to ostream the necessary
  SWIG incantation to annotate each method (or function) with a
  docstring appropriate for the given language.
  """
  istream = open(filename)
  header  = CHeader(istream)
  istream.close()

  for c in header.classes:
    for m in c.methods:
      if not m.name.startswith('~'):
        writeDocstring(ostream, language, m.docstring, m.name, c.name)

  for f in header.functions:
    writeDocstring(ostream, language, f.docstring, f.name)



def sanitizeForPython (docstring):
  """sanitizeForPython (docstring) -> docstring

  Performs some mimimal Python specific sanitizations on the
  C++/Doxygen docstring.
  """
  docstring = docstring.replace('/**', '').replace('*/', '').replace('*', '')
  docstring = docstring.replace('@return', 'Returns')
  return docstring



def sanitizeForPerl (docstring):
  """sanitizeForPerl (docstring) -> docstring

  Performs some mimimal Perl specific sanitizations on the
  C++/Doxygen docstring.
  """
  docstring = docstring.replace('/**', '').replace('*/', '').replace('*', '')
  docstring = docstring.replace('@return', 'Returns')
  docstring = docstring.replace(' < ', ' E<lt> ').replace(' > ', ' E<gt> ')
  docstring = ' '.join(docstring.split())
  docstring = re.sub('<code>([^<]*)</code>', r'C<\1>', docstring)
  docstring = re.sub('<b>([^<]*)</b>', r'B<\1>', docstring)  
  return docstring



def writeDocstring (ostream, language, docstring, methodname, classname=None):
  """writeDocstring (ostream, language='java'|'python'|'perl', docstring,
  methodname, classname='')

  Writes to ostream the necessary SWIG incantation to annotate the
  given class method (or function) with a docstring appropriate for
  the given language.
  """
  if language == 'java':
    pre  = '%javamethodmodifiers'
    post = 'public'
  elif language == 'perl':
    pre  = '=item'
    post = ''
  else:
    pre  = '%feature("docstring")'
    post = ''

  docstring = docstring.replace('"', "'")

  if language == 'python':
    docstring = sanitizeForPython(docstring)
  elif language == 'perl':
    docstring = sanitizeForPerl(docstring)  

  output = pre + ' '

  if classname:
    output += classname + '::'

  if language == 'perl':
   output += '%s\n\n%s%s\n\n'    % (methodname, docstring, post)
  else:
    output += '%s "\n%s%s";\n\n' % (methodname, docstring, post)

  ostream.write(output)
  ostream.flush()



def main (args):
  """usage: swigdoc.py [java | python | perl] -Ipath libsbml.i docstrings.i

  java | python | perl  generate docstrings for this language module.
  path                  is the path to the libsbml src/ directory.
  libsbml.i             is the master libsbml SWIG interface file.
  docstrings.i          is the file to output the SWIG docstrings.
  """
  if len(args) != 5:
    print main.__doc__
    sys.exit(1)

  language    = args[1]
  includepath = string.replace(args[2], '-I', '')
  headers     = getHeadersFromSWIG(args[3])
  stream      = open(args[4], 'w')

  if language == 'perl':
    infile = open(os.path.abspath('LibSBML.txt'), 'r')
    stream.write(infile.read())
    stream.write('=head1 FUNCTION INDEX\n\n=over 8\n\n')

  for file in headers:
    filename = os.path.normpath(os.path.join(includepath, file))
    processHeader(filename, stream, language)

  if language == 'perl':
   stream.write('=cut\n')

  stream.close()


if __name__ == '__main__':
  main(sys.argv)
 
