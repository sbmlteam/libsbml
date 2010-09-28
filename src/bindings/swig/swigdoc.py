#!/usr/bin/env python
#
# @file   swigdoc.py
# @brief  Creates documentation for Java, Python, and Perl.
# @author Ben Bornstein
# @author Christoph Flamm
# @author Akiya Jouraku
# @author Michael Hucka
#
# $Id$
# $HeadURL$
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
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

#
# Globally-scoped variables
#

docincpath     = ''
libsbmlclasses = ["AlgebraicRule",
                  "ASTNode",
                  "AssignmentRule",
                  "CVTerm",
                  "Compartment",
                  "CompartmentType",
                  "Constraint",
                  "Date",
                  "Delay",
                  "Event",
                  "EventAssignment",
                  "FunctionDefinition",
                  "InitialAssignment",
                  "KineticLaw",
                  "libsbml",
                  "List",
                  "ListOf",
                  "ListOfCompartmentTypes",
                  "ListOfCompartments",
                  "ListOfConstraints",
                  "ListOfEventAssignments",
                  "ListOfEvents",
                  "ListOfFunctionDefinitions",
                  "ListOfInitialAssignments",
                  "ListOfLocalParameters",
                  "ListOfParameters",
                  "ListOfReactions",
                  "ListOfRules",
                  "ListOfSpecies",
                  "ListOfSpeciesReferences",
                  "ListOfSpeciesTypes",
                  "ListOfUnitDefinitions",
                  "ListOfUnits",
                  "LocalParameter",
                  "Model",
                  "ModelCreator",
                  "ModelHistory",
                  "ModifierSpeciesReference",
                  "OFStream",
                  "OStream",
                  "OStringStream",
                  "Parameter",
                  "RateRule",
                  "RDFAnnotationParser",
                  "Reaction",
                  "Rule",
                  "SBMLDocument",
                  "SBMLError",
                  "SBMLErrorLog",
                  "SBMLNamespaces",
                  "SBMLReader",
                  "SBMLVisitor",
                  "SBMLWriter",
                  "SBO",
                  "SBase",
                  "SimpleSpeciesReference",
                  "Species",
                  "SpeciesReference",
                  "SpeciesType",
                  "StoichiometryMath",
                  "SyntaxChecker",
                  "Trigger",
                  "Unit", 
                  "UnitDefinition",
                  "XMLAttributes",
                  "XMLError",
                  "XMLErrorLog",
                  "XMLNamespaces",
                  "XMLNode",
                  "XMLToken",
                  "XMLTriple" ]

#
# Classes and methods.
#

class CHeader:
  """CHeader encapsulates the C++ class and C function definitions
  found within a C header file.  It has the following public
  attributes:

    - classes
    - functions
  """

  def __init__ (self, forLanguage, stream):
    """CHeader (forLanguage, stream=None) -> CHeader

    Creates a new CHeader reading from the given stream.
    """
    self.classes   = [ ]
    self.functions = [ ]
    self.classDocs = [ ]

    if stream is not None:
      self.read(forLanguage, stream)


  def read (self, forLanguage, stream):
    """read (PushBackStream)

    Reads a C/C++ header file from the given stream.
    """
    inClass     = False
    inClassDocs = False
    inDocs      = False
    inFunc      = False
    inSkip      = False
    isInternal  = False
    ignoreThis  = False

    docstring   = ''
    lines       = ''

    for line in stream.readlines():
      stripped = line.strip()

      if stripped == '#ifndef SWIG':
        inSkip = True
      if stripped.startswith('#endif') and (stripped.find('SWIG') >= 0):
        inSkip = False
      if inSkip: continue

      # Track things that we flag as internal, so that we can
      # remove them from the documentation.

      if (stripped.find('@cond doxygen-libsbml-internal') >= 0):    isInternal = True
      if (stripped.find('@endcond') >= 0): isInternal = False

      # Watch for class description, usually at top of file.

      if (not inClassDocs) and stripped.startswith('* @class'):
        inClassDocs = True
        classname = stripped[8:].strip()
        if classname.endswith('.'):
          classname = classname[:-1]
        docstring = ''
        continue

      if inClassDocs:
        if stripped.startswith('* @brief'):
          docstring += ' * ' + stripped[9:].strip() + '\n'
          continue
        elif not stripped.endswith('*/') and not stripped.startswith('* @class'):
          docstring += line
          continue
        else:
          docstring = '/**\n' + docstring + ' */'
          doc = CClassDoc(docstring, classname)
          self.classDocs.append(doc)

        # There may be more class docs in the same comment.
        if stripped.startswith('* @class'):
          classname = stripped[8:].strip()
          if classname.endswith('.'):
            classname = classname[:-1]          
        else:
          inClassDocs = False

        docstring = ''
        continue

      # Watch for class definition, methods and out-of-class functions.

      if stripped.startswith('class ') and not stripped.endswith(';'):
        ignoreThis = False
        inClass   = True
        classname = line[6:].split(':')[0].strip()
        if classname[:6] == 'LIBSBM' or classname[:6] == 'LIBLAX':
          classname = classname.split(' ')[1].strip()
        self.classes.append( CClass(classname) )
        continue

      if stripped == '};':
        inClass = False
        continue

      if stripped == '/**':
        docstring  = ''
        lines      = ''
        ignoreThis = False
        inDocs     = True

      if inDocs:
        docstring += line
        inDocs     = (stripped != '*/')
        continue

      # If we get here, we're no longer inside a comment block.
      # Start saving lines, but skip embedded comments.

      if stripped.startswith('#') or (stripped.find('typedef') >= 0):
        ignoreThis = True
        continue

      if not ignoreThis:
        cppcomment = stripped.find('//')
        if cppcomment != -1:
          stripped = stripped[:cppcomment]
        lines += stripped + ' '         # Space avoids jamming code together.

        # Keep an eye out for the end of the declaration.

        if not stripped.startswith('*') and \
               (stripped.endswith(';') or stripped.endswith(')') or stripped.endswith('}')):
          # It might be a forward declaration.  Skip it.
          if lines.startswith('class'):
            continue

          # It might be a C++ operator redefinition.  Skip it.
          if lines.find('operator') >= 0:
            continue

          # It might be an enum.  Skip it.
          if stripped.endswith('}'):
            lines   = lines[:lines.rfind('{')]
          if not stripped.startswith('enum'):
            stop    = lines.rfind('(')
            name    = lines[:stop].split()[-1]
            args    = lines[stop:lines.rfind(')')+1]
            isConst = lines[lines.rfind(')'):].rfind('const')
  
            # Swig doesn't seem to mind C++ argument lists, even though they
            # have "const", "&", etc.  So I'm leaving the arg list unmodified.

            func = Method(forLanguage, isInternal, docstring, name, args, (isConst > 0))

            if inClass:
              c = self.classes[-1]
              c.methods.append(func)
              if c.methodVariants.get(name) == None:
                c.methodVariants[name] = {}
              c.methodVariants[name][name + args] = c.methodVariants[name].get(name + args, 0) + 1
            else:
              self.functions.append(func)
              # FIXME need do nc variants


class CClass:
  """A CClass encapsulates a C++ class.  It has the following public
  attributes:

    - name
    - methods
    - methodVariants
  """

  def __init__ (self, name):
    """CClass(name) -> CClass

    Creates a new CClass with the given name.
    """
    self.name           = name
    self.methods        = [ ]
    self.methodVariants = {}



class Method:
  """A Method encapsulates a C/C++ function.  Currently, it has the
  following public attributes:

    - forLanguage
    - isInternal
    - docstring
    - name
    - args
    - isConst
  """

  def __init__ (self, forLanguage, isInternal, docstring, name, args, isConst):
    """Method(forLanguage, isInternal, docstring name, args, isConst) -> Method

    Creates a new Method description with the given docstring, name and args,
    for the language forLanguage, with special consideration if the method
    was declared constant and/or internal.
    """

    self.name    = name
    self.isConst = isConst

    if isInternal:
      if forLanguage == 'java':
        # This is a hack, but Javadoc offers no way of selectively omitting
        # documentation from the output, so this is the most straightforward
        # and unobtrusive approach I've found.  The following flags our
        # internal methods with the Javadoc @deprecated tag.  We then invoke
        # the javadoc program with the flag -nodeprecated, and presto, the
        # methods are not put into the documentation generated.
        #
        p = re.compile('(.+?)\*/', re.MULTILINE)
        self.docstring = p.sub(r'\1\n * @deprecated libSBML internal\n */', docstring)
      else:
        self.docstring = "  @internal\n" + docstring
    else:
      self.docstring = docstring

    # In Java, if a method is const and swig has to translate the type,
    # then for some reason swig cannot match up the resulting doc strings
    # that we put into %javamethodmodifiers.  The result is that the java
    # documentation for the methods are empty.  I can't figure out why, but
    # have figured out that if we omit the argument list in the doc string
    # that is put on %javamethodmodifiers for such case, swig does generate 
    # the comments for those methods.  This approach is potentially dangerous
    # because swig might attach the doc string to the wrong method if a
    # methods has multiple versions with varying argument types, but the
    # combination doesn't seem to arise in libSBML currently, and anyway,
    # this fixes a real problem in the Java documentation for libSBML.

    if forLanguage == 'java':
      if isConst and (args.find('unsigned int') >= 0):
        self.args = ''
      elif not args.strip() == '()':
        if isConst:
          self.args = args + ' const'
        else:
          self.args = args
      else:
        if isConst:
          self.args = '() const'
        else:
          self.args = ''
    else:
      self.args = args



class CClassDoc:
  """Encapsulates documentation for a class.  Currently, it has the
  following public attributes:

    - docstring
    - name
  """

  def __init__ (self, docstring, name):
    """CClassDoc(docstring, name) -> CClassDoc

    Creates a new CClassDoc with the given docstring and name.
    """

    # Take out excess leading blank lines.
    docstring = re.sub('/\*\*(\s+\*)+', r'/** \n *', docstring)

    self.docstring = docstring
    self.name      = name



def fixUpIncludePath(line):
  # All our paths start with 'sbml/...'.  Look to see if there's any other
  # slash, as an indication the path is 'sbml/xml/foo.h' and not 'sbml/foo.h'.

  if line[5:].find('/') > 0:
    return line[5:]
  else:
    return line



def getHeadersFromSWIG (filename):
  """getHeadersFromSWIG (filename) -> (filename1, filename2, .., filenameN)

  Reads the list of %include directives from the given SWIG (.i).  The
  list of C/C++ headers (.h) included is returned.
  """
  stream = open(filename)
  lines  = stream.readlines()

  lines  = [line for line in lines if line.strip().startswith('%include')]
  lines  = [line for line in lines if line.strip().endswith('.h')]
  lines  = [line.replace('%include', '').strip() for line in lines]

  # We have a weird source setup.  The following accounts for it so that we
  # use the source tree directly rather than the "include" copy as was
  # previously done.  The latter may fall out of date while working on the
  # documentation and then lead to hard-to-track down inconsistencies
  # between the docs and the source tree.  (I've wasted hours trying to
  # figure out why doxygen wasn't picking up a recent change on separate
  # occasions, only to realize I forgot to run 'make' or 'make include' in
  # the source tree after a change.)

  lines  = list(map(fixUpIncludePath, lines))

  stream.close()

  return lines



def rewriteCommonReferences (docstring, language):
  """rewriteCommonReferences (docstring, language) -> docstring

  Rewrites common C++ doxygen references to match language-specific needs.
  """

  if language == 'java':  
    target = 'libsbmlConstants#'
  elif language == 'python':
    target = 'libsbml.'
  else:
    target = ''

  if target != '':
    docstring = re.sub(r'OperationReturnValues_t#', target, docstring)
    docstring = re.sub(r'SBMLTypeCode_t#',          target, docstring)
    docstring = re.sub(r'ASTNodeType_t#',           target, docstring)
    docstring = re.sub(r'RuleType_t#',              target, docstring)
    docstring = re.sub(r'UnitKind_t#',              target, docstring)
    docstring = re.sub(r'ModelQualifierType_t#',    target, docstring)
    docstring = re.sub(r'BiolQualifierType_t#',     target, docstring)
    docstring = re.sub(r'QualifierType_t#',         target, docstring)
    docstring = re.sub(r'SBMLErrorCode_t#',         target, docstring)
    docstring = re.sub(r'SBMLErrorCategory_t#',     target, docstring)
    docstring = re.sub(r'SBMLErrorSeverity_t#',     target, docstring)
    docstring = re.sub(r'XMLErrorCode_t#',          target, docstring)
    docstring = re.sub(r'XMLErrorCategory_t#',      target, docstring)
    docstring = re.sub(r'XMLErrorSeverity_t#',      target, docstring)

  return docstring



def translateVerbatim (match):
  text = match.group()
  if re.search('@verbatim', text) != None:
    tagName = 'verbatim'
  else:
    tagName = 'code'
  text = text.replace('<p>', '')
  text = text.replace('<', '&lt;')
  text = text.replace('>', '&gt;')

  regexp = '@' + tagName + '[ \t]*'
  text = re.sub(regexp, r"<div class='fragment'><pre>", text)

  regexp = '(\s*\*\s*)*@end' + tagName
  p = re.compile(regexp, re.MULTILINE)
  text = p.sub(r'</pre></div>', text)

  return text



def translateInclude (match):
  global docincpath

  text    = match.group()
  file    = match.group(1)
  stream  = open(docincpath + '/common-text/' + file, 'r')
  content = stream.read()
  stream.close()

  return content


def translateIfJava (match):
  text = match.group()
  if match.group(1) == 'java' or match.group(1) == 'notcpp':
    text =  match.group(2)
  else:
    text = ''
  return text


def translateIfPerl (match):
  text = match.group()
  if match.group(1) == 'perl' or match.group(1) == 'notcpp':
    text =  match.group(2)
  else:
    text = ''
  return text



def translateIfPython (match):
  text = match.group()
  if match.group(1) == 'python' or match.group(1) == 'notcpp':
    text =  match.group(2)
  else:
    text = ''
  return text



def translateJavaCrossRef (match):
  prior = match.group(1)
  classname = match.group(2)
  method = match.group(3)
  return prior + '{@link ' + classname + '#' + method + '}'



def translatePythonCrossRef (match):
  prior = match.group(1)
  classname = match.group(2)
  method = match.group(3)
  args = match.group(4)
  return prior + classname + "." + method + "()"



def translatePythonSeeRef (match):
  prior = match.group(1)
  method = match.group(2)
  args = match.group(3)
  return prior + method + "()"



def javafyClassRef (match):
  return match.group(1) + match.group(2) + ' '



def pythonifyClassRef (match):
  return match.group(1) + match.group(2)



def translateClassRef (match):
  leading      = match.group(1)
  classname    = match.group(2)
  trailing     = match.group(3)

  if leading != '%' and leading != '(':
    return leading + '{@link ' + classname + '}' + trailing
  else:
    return leading + classname + trailing



def sanitizeForHTML (docstring, language):
  """sanitizeForHTML (docstring, language) -> docstring

  Performs HTML transformations on the C++/Doxygen docstring.
  """

  # First do conditional section inclusion based on the current language.
  # This ONLY handles @if foo @endif, not @if foo @else bar @endif. 

  p = re.compile('@if\s+(java|python|perl|clike|notcpp)\s+(.+?)@endif', re.DOTALL)
  if language == 'java':
    docstring = p.sub(translateIfJava, docstring)
  elif language == 'perl':
    docstring = p.sub(translateIfPerl, docstring)
  elif language == 'python':
    docstring = p.sub(translateIfPython, docstring)

  # Replace blank lines between paragraphs with <p>.  There are two main
  # cases: comments blocks whose lines always begin with an asterix (e.g.,
  # C/C++), and comment blocks where they don't (e.g., Python).  The third
  # substitution below does the same thing for blank lines, except for the
  # very end of the doc string.

  p = re.compile('^(\s+)\*\s*$', re.MULTILINE)
  docstring = p.sub(r'\1* <p>', docstring)
  p = re.compile('^((?!\s+\Z)\s+)$', re.MULTILINE)
  docstring = p.sub(r'\1<p>', docstring)
  p = re.compile('^(?!\Z)$', re.MULTILINE)
  docstring = p.sub(r'<p>', docstring)

  # Javadoc doesn't have an @htmlinclude command, so we process the file
  # inclusion directly here.

  p = re.compile('@htmlinclude\s+([^\s]+).*$', re.MULTILINE)
  docstring = p.sub(translateInclude, docstring)

  # There's no Javadoc verbatim or @code/@endcode equivalent, so we have to
  # convert it to raw HTML and transform the content too.  This requires
  # helpers.  The following treats both @verbatim and @code the same way.

  p = re.compile('@verbatim.+?@endverbatim', re.DOTALL)
  docstring = p.sub(translateVerbatim, docstring)
  p = re.compile('@code.+?@endcode', re.DOTALL)
  docstring = p.sub(translateVerbatim, docstring)

  # Javadoc doesn't have a @section or @subsection commands, so we translate
  # those ourselves.

  p = re.compile('@section\s+[^\s]+\s+(.*)$', re.MULTILINE)
  docstring = p.sub(r'<h2>\1</h2>', docstring)
  p = re.compile('@subsection\s+[^\s]+\s+(.*)$', re.MULTILINE)
  docstring = p.sub(r'<h3>\1</h3>', docstring)
  p = re.compile('@subsubsection\s+[^\s]+\s+(.*)$', re.MULTILINE)
  docstring = p.sub(r'<h4>\1</h4>', docstring)

  # Javadoc doesn't have an @image command.  We translate @image html
  # but ditch @image latex.

  p = re.compile('@image\s+html+\s+([^\s]+).*$', re.MULTILINE)
  docstring = p.sub(r"<center><img src='\1'></center><br>", docstring)
  p = re.compile('@image\s+latex+\s+([^\s]+).*$', re.MULTILINE)
  docstring = p.sub(r'', docstring)

  # Doxygen doesn't understand HTML character codes like &ge;, so we've
  # been using doxygen's Latex facility to get special mathematical
  # characters into the documentation, but as luck would have it, Javadoc
  # doesn't understand the Latex markup.  All of this is getting old.

  docstring = re.sub(r'\\f\$\\geq\\f\$', '&#8805;', docstring)
  docstring = re.sub(r'\\f\$\\leq\\f\$', '&#8804;', docstring)
  docstring = re.sub(r'\\f\$\\times\\f\$', '&#215;', docstring)

  # The following are done in pairs because I couldn't come up with a
  # better way to catch the case where @c and @em end up alone at the end
  # of a line and the thing to be formatted starts on the next one after
  # the comment '*' character on the beginning of the line.

  docstring = re.sub('@c *([^ ,;()/*\n\t]+)', r'<code>\1</code>', docstring)
  docstring = re.sub('@c(\n[ \t]*\*[ \t]*)([^ ,;()/*\n\t]+)', r'\1<code>\2</code>', docstring)
  docstring = re.sub('@p +([^ ,.:;()/*\n\t]+)', r'<code>\1</code>', docstring)
  docstring = re.sub('@p(\n[ \t]*\*[ \t]+)([^ ,.:;()/*\n\t]+)', r'\1<code>\2</code>', docstring)
  docstring = re.sub('@em *([^ ,.:;()/*\n\t]+)', r'<em>\1</em>', docstring)
  docstring = re.sub('@em(\n[ \t]*\*[ \t]*)([^ ,.:;()/*\n\t]+)', r'\1<em>\2</em>', docstring)

  docstring = re.sub(r'@li\b', '<li>', docstring)

  docstring = re.sub(r'\s*\Z', '\n', docstring)

  # Doxygen automatically cross-references class names in text to the class
  # definition page, but Javadoc does not.  Rather than having to put in a
  # lot conditional @if/@endif's into the documentation to manually create
  # cross-links just for the Java case, let's automate.  This needs to be
  # done better (e.g., by not hard-wiring the class names).

  p = re.compile(r'(\W)(' + '|'.join(libsbmlclasses) + r')\b([^:])', re.DOTALL)
  docstring = p.sub(translateClassRef, docstring)

  # Massage Java method cross-references.

  p = re.compile('(\s+)(\S+?)::(\w+\s*\([^)]*?\))', re.MULTILINE)
  docstring = p.sub(translateJavaCrossRef, docstring)

  # Take out any left-over quotes, because Javadoc doesn't have the %foo
  # quoting mechanism.

  docstring = re.sub('(\s)%(\w)', r'\1\2', docstring)

  return docstring



def removeStar (match):
  text = match.group()
  text = text.replace('*', '')
  return text



def rewriteDocstringForJava (docstring):
  """rewriteDocstringForJava (docstring) -> docstring

  Performs some mimimal javadoc-specific sanitizations on the
  C++/Doxygen docstring.
  """

  docstring = rewriteCommonReferences(docstring, 'java')  

  # Preliminary: rewrite some of the data type references to equivalent
  # Java types.  (Note: this rewriting affects only the documentation
  # comments inside classes & methods, not the method signatures.)

  docstring = docstring.replace(r'const char *', 'String ')
  docstring = docstring.replace(r'const char* ', 'String ')
  docstring = docstring.replace(r'an unsigned int', 'a long integer')
  docstring = docstring.replace(r'unsigned int', 'long')
  docstring = docstring.replace(r'const std::string&', 'String')
  docstring = docstring.replace(r'const std::string', 'String')
  docstring = docstring.replace(r'std::string', 'String')
  docstring = docstring.replace(r'NULL', 'null')

  # Also use Java syntax instead of "const XMLNode*" etc.

  p = re.compile(r'const (%?)(' + '|'.join(libsbmlclasses) + r') ?(\*|&)', re.DOTALL)
  docstring = p.sub(javafyClassRef, docstring)  
  p = re.compile(r'(%?)(' + '|'.join(libsbmlclasses) + r') ?(\*|&)', re.DOTALL)
  docstring = p.sub(javafyClassRef, docstring)  

  # Do the big work.

  docstring = sanitizeForHTML(docstring, 'java')

  # Fix up for a problem introduced by sanitizeForHTML -- should fix
  # properly some day.

  p = re.compile('(@see\s+[\w ,(]*){@link\s+([^}]+?)}')
  docstring = p.sub(r'\1\2', docstring)

  # Inside of @see, change double colons to pound signs.

  docstring = re.sub('(@see\s+\w+)::', r'\1#', docstring)

  # The syntax for @see is slightly different: method names need to have a
  # leading pound sign character.  This particular bit of code only handles
  # a single @see foo(), which means the docs have to be written that way.
  # Maybe someday in the future it should be expanded to handle
  # @see foo(), bar(), etc., but I don't have time right now to do it.

  docstring = re.sub('(@see\s+)([\w:.]+)\(', r'\1#\2(', docstring)

  # Remove the '*' character that Javadoc doesn't want to see in @see's.
  # (This doesn't make a difference; javadoc still can't match up the refs.)

  #  p = re.compile('@see[\s\w.:,()#]+[*][\s\w.:,()*#]')
  #  docstring = p.sub(removeStar, docstring)

  # The syntax for @link is vastly different.
  
  p = re.compile('@link([\s/*]+[\w\s,.:#()*]+[\s/*]+[\w():#]+[\s/*]*)@endlink', re.DOTALL)
  docstring = p.sub(r'{@link \1}', docstring)

  # Outside of @see and other constructs, dot is used to reference members
  # instead of C++'s double colon.

  docstring = docstring.replace(r'::', '.')

  # Need to escape the quotation marks:

  docstring = docstring.replace('"', "'")
  docstring = docstring.replace(r"'", r"\'")

  return docstring



def indentVerbatimForPython (match):
  text = match.group()

  p = re.compile('^(.)', re.MULTILINE)
  text = p.sub(r'  \1', text)

  return text



def rewriteDocstringForPython (docstring):
  """rewriteDocstringForPython (docstring) -> docstring

  Performs some mimimal Python specific sanitizations on the
  C++/Doxygen docstring.

  Note: this is not the only processing performed for the Python
  documentation.  In docs/src, the doxygen-based code has an additional,
  more elaborate filter that processes the output of *this* filter.
  """

  docstring = rewriteCommonReferences(docstring, 'python')  

  docstring = docstring.replace('/**', '').replace('*/', '')
  p = re.compile('^(\s*)\*([ \t]*)', re.MULTILINE)
  docstring = p.sub(r'\2', docstring)

  # Rewrite some of the data type references to equivalent Python types.
  # (Note: this rewriting affects only the documentation comments inside
  # classes & methods, not the method signatures.)

  docstring = docstring.replace(r'const char *', 'string ')
  docstring = docstring.replace(r'const char* ', 'string ')
  docstring = docstring.replace(r'an unsigned int', 'a long integer')
  docstring = docstring.replace(r'unsigned int', 'long')
  docstring = docstring.replace(r'const std::string&', 'string')
  docstring = docstring.replace(r'const std::string', 'string')
  docstring = docstring.replace(r'std::string', 'string')
  docstring = docstring.replace(r'NULL', 'None')

  # Also use Python syntax instead of "const XMLNode*" etc.

  p = re.compile(r'const (%?)(' + '|'.join(libsbmlclasses) + r') ?(\*|&)', re.DOTALL)
  docstring = p.sub(pythonifyClassRef, docstring)  
  p = re.compile(r'(%?)(' + '|'.join(libsbmlclasses) + r') ?(\*|&)', re.DOTALL)
  docstring = p.sub(pythonifyClassRef, docstring)  

  # Need to escape the quotation marks:

  docstring = docstring.replace('"', "'")
  docstring = docstring.replace(r"'", r"\'")

  # Python method cross-references won't be made by doxygen unless
  # the method reference is written without arguments.

  p = re.compile('(\s+)(\S+?)::(\w+\s*)(\([^)]*?\))', re.MULTILINE)
  docstring = p.sub(translatePythonCrossRef, docstring)
  p = re.compile('(@see\s+)(\w+\s*)(\([^)]*?\))')
  docstring = p.sub(translatePythonSeeRef, docstring)

  # Friggin' doxygen escapes HTML character codes, so the hack we have to
  # do for Javadoc turns out doesn't work for the Python documentation.
  # Kluge around it.

  docstring = re.sub(r'\\f\$\\geq\\f\$', '>=', docstring)
  docstring = re.sub(r'\\f\$\\leq\\f\$', '<=', docstring)
  docstring = re.sub(r'\\f\$\\times\\f\$', '*', docstring)

  # SWIG does some bizarre truncation of leading characters that
  # happens to hit us because of how we have to format verbatim's.
  # This tries to kluge around it:  
  p = re.compile('@verbatim.+?@endverbatim', re.DOTALL)
  docstring = p.sub(indentVerbatimForPython, docstring)

  return docstring



def rewriteDocstringForPerl (docstring):
  """rewriteDocstringForPerl (docstring) -> docstring

  Performs some mimimal Perl specific sanitizations on the
  C++/Doxygen docstring.
  """

  docstring = rewriteCommonReferences(docstring, 'perl')  

  # Get rid of the /** ... */ and leading *'s.
  docstring = docstring.replace('/**', '').replace('*/', '').replace('*', ' ')

  # Get rid of indentation
  p = re.compile('^\s+(\S*\s*)', re.MULTILINE)
  docstring = p.sub(r'\1', docstring)

  # Get rid of paragraph indentation not caught by the code above.
  p = re.compile('^[ \t]+(\S)', re.MULTILINE)
  docstring = p.sub(r'\1', docstring)

  # Get rid of blank lines.
  p = re.compile('^[ \t]+$', re.MULTILINE)
  docstring = p.sub(r'', docstring)

  # Get rid of the %foo quoting.
  docstring = re.sub('(\s)%(\w)', r'\1\2', docstring)

  # The following are done in pairs because I couldn't come up with a
  # better way to catch the case where @c and @em end up alone at the end
  # of a line and the thing to be formatted starts on the next one after
  # the comment '*' character on the beginning of the line.

  docstring = re.sub('@c *([^ ,.:;()/*\n\t]+)', r'C<\1>', docstring)
  docstring = re.sub('@c(\n[ \t]*\*[ \t]*)([^ ,.:;()/*\n\t]+)', r'\1C<\2>', docstring)
  docstring = re.sub('@p +([^ ,.:;()/*\n\t]+)', r'C<\1>', docstring)
  docstring = re.sub('@p(\n[ \t]*\*[ \t]+)([^ ,.:;()/*\n\t]+)', r'\1C<\2>', docstring)
  docstring = re.sub('@em *([^ ,.:;()/*\n\t]+)', r'I<\1>', docstring)
  docstring = re.sub('@em(\n[ \t]*\*[ \t]*)([^ ,.:;()/*\n\t]+)', r'\1I<\2>', docstring)

  docstring = docstring.replace('<ul>', '\n=over\n')
  docstring = docstring.replace('<li> ', '\n=item\n\n')
  docstring = docstring.replace('</ul>', '\n=back\n')

  docstring = docstring.replace('@return', 'Returns')
  docstring = docstring.replace(' < ', ' E<lt> ').replace(' > ', ' E<gt> ')
  docstring = re.sub('<code>([^<]*)</code>', r'C<\1>', docstring)
  docstring = re.sub('<b>([^<]*)</b>', r'B<\1>', docstring)  

  return docstring



def processClassMethods(ostream, language, cclass):
  # In the Python docs, we have to combine the docstring for methods with
  # different signatures and write out a single method docstring.  In the
  # other languages, we write out separate docstrings for every method
  # having a different signature.

  if language == 'python':
    written = {}
    for m in cclass.methods:
      if written.has_key(m.name + m.args):
        continue
      if m.name.startswith('~'):
        continue

      if cclass.methodVariants[m.name].__len__() > 1:
        docstring = ' This method has multiple variants that differ in the' + \
                    ' arguments they accept.  Each is described separately' + \
                    ' below.\n'
        for mm in cclass.methods:
          # Ignore methods marked @internal.
          if mm.name == m.name and re.search('@internal', docstring) == None:
            docstring += "\n <hr>\n Method variant with the following"\
                         + " signature:\n <pre class='signature'>" \
                         + mm.name \
                         + rewriteDocstringForPython(mm.args) \
                         + "</pre>\n\n"
            docstring += rewriteDocstringForPython(mm.docstring)
      else:
        docstring = rewriteDocstringForPython(m.docstring)
      ostream.write(formatMethodDocString(language, m.name, cclass.name, docstring, m.args))
      written[m.name + m.args] = 1
  else: # Not python
    for m in cclass.methods:
      if m.name.startswith('~'):
        continue
      if language == 'java':
        docstring = rewriteDocstringForJava(m.docstring)
      elif language == 'perl':
        docstring = rewriteDocstringForPerl(m.docstring)  
      ostream.write(formatMethodDocString(language, m.name, cclass.name, docstring, m.args))

  ostream.flush()



def formatMethodDocString (language, methodname, classname, docstring, args=None):
  if language == 'java':
    pre  = '%javamethodmodifiers'
    post = ' public'
  elif language == 'perl':
    pre  = '=item'
    post = ''
  else:
    pre  = '%feature("docstring")'
    post = ''

  output = pre + ' '

  if classname:
    output += classname + '::'

  if language == 'perl':
    output += '%s\n\n%s%s\n\n'   % (methodname, docstring, post)
  elif language == 'python':
    output += '%s "\n%s%s";\n\n' % (methodname, docstring, post)
  else:
    output += '%s%s "\n%s%s";\n\n' % (methodname, args, docstring, post)

  return output



def generateFunctionDocString (language, methodname, docstring, args):
  if language == 'java':
    doc = rewriteDocstringForJava(docstring)
  elif language == 'python':
    doc = rewriteDocstringForPython(docstring)
  elif language == 'perl':
    doc = rewriteDocstringForPerl(docstring)
  return formatMethodDocString(language, methodname, None, doc, args)



def generateClassDocString (language, docstring, classname):
  if language == 'java':
    pre = '%typemap(javaimports) '
    docstring = rewriteDocstringForJava(docstring)    
    output = pre + classname + ' "\n' + docstring + '"\n\n'

  elif language == 'python':
    pre = '%feature("docstring") '
    docstring = rewriteDocstringForPython(docstring)
    output = pre + classname + ' "\n' + docstring + '"\n\n'

  elif language == 'perl':
    docstring = rewriteDocstringForPerl(docstring)
    output = '=back\n\n=head2 ' + classname + '\n\n' + docstring + '\n\n=over\n\n'

  return output



def processClasses (ostream, classes, language):
  for c in classes:
    processClassMethods(ostream, language, c)



def processFunctions (ostream, functions, language):
  for f in functions:
    ostream.write(generateFunctionDocString(language, f.name, f.docstring, f.args))



def processClassDocs (ostream, classDocs, language):
  for c in classDocs:
    ostream.write(generateClassDocString(language, c.docstring, c.name))



def processFile (filename, ostream, language):
  """processFile (filename, ostream, language='java'|'python')

  Reads the the given header file and writes to ostream the necessary SWIG
  incantation to annotate each method (or function) with a docstring
  appropriate for the given language.
  """

  istream = open(filename)
  header  = CHeader(language, istream)
  istream.close()

  processClassDocs(ostream, header.classDocs, language)
  processClasses(ostream, header.classes, language)
  processFunctions(ostream, header.functions, language)

  ostream.flush()



def main (args):
  """usage: swigdoc.py [java | python | perl] -Ipath -Dpath libsbml.i output.i

  java | python | perl  generate docstrings for this language module.
  path                  is the path to the libsbml src/ directory.
  libsbml.i             is the master libsbml SWIG interface file.
  output.i              is the file to output the SWIG docstrings.
  """

  if len(args) != 6:
    print(main.__doc__)
    sys.exit(1)

  global docincpath

  language    = args[1]
  includepath = args[2].replace('-I', '')
  docincpath  = args[3].replace('-D', '')
  headers     = getHeadersFromSWIG(args[4])
  stream      = open(args[5], 'w')

  headers.append("bindings/swig/OStream.h")

  if language == 'perl':
    infile = open(os.path.abspath('LibSBML.txt'), 'r')
    stream.write(infile.read())
    stream.write('=head1 FUNCTION INDEX\n\n=over 8\n\n')

  for file in headers:
    filename = os.path.normpath(os.path.join(includepath, file))
    processFile(filename, stream, language)

  if language == 'perl':
   stream.write('=cut\n')

  stream.close()


if __name__ == '__main__':
  main(sys.argv)
 
