#!/usr/bin/env python
#
# @file   swigdoc.py
# @brief  Creates documentation for C#, Java, Python, and Perl.
# @author Ben Bornstein
# @author Christoph Flamm
# @author Akiya Jouraku
# @author Michael Hucka
# @author Frank Bergmann
#
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
#----------------------------------------------------------------------- -->*/

import sys, string, os.path, re

#
# Globally-scoped variables
#

language       = ''
docincpath     = ''
libsbmlclasses = ["AlgebraicRule",
                  "ASTNode",
                  "ASTNodeList",
                  "AssignmentRule",
                  "CVTerm",
                  "CVTermList",
                  "Compartment",
                  "CompartmentType",
                  "Constraint",
                  "ConversionOption",
                  "ConversionProperties",
                  "Date",
                  "DateList",
                  "Delay",
                  "Event",
                  "EventAssignment",
                  "FunctionDefinition",
                  "InitialAssignment",
                  "KineticLaw",
                  "libsbml",
                  "L3ParserSettings",
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
                  "ModelCreatorList",
                  "ModelHistory",
                  "ModifierSpeciesReference",
                  "OFStream",
                  "OStream",
                  "OStringStream",
                  "Parameter",
                  "Priority",
                  "RDFAnnotationParser",
                  "RateRule",
                  "Reaction",
                  "Rule",
                  "SBMLConstructorException",
                  "SBMLConverter",
                  "SBMLConverterRegistry",
                  "SBMLDocument",
                  "SBMLDocumentPlugin",
                  "SBMLDocumentPluginNotRequired",
                  "SBMLError",
                  "SBMLErrorLog",
                  "SBMLExtension",
                  "SBMLExtensionException",
                  "SBMLExtensionNamespaces",
                  "SBMLExtensionRegister",
                  "SBMLExtensionRegistry",
                  "SBMLExternalValidator",
                  "SBMLFunctionDefinitionConverter",
                  "SBMLInitialAssignmentConverter",
                  "SBMLLevelVersionConverter",
                  "SBMLNamespaces",
                  "SBMLNamespacesList",
                  "SBMLReader",
                  "SBMLRuleConverter",
                  "SBMLStripPackageConverter",
                  "SBMLUnitsConverter",
                  "SBMLVisitor",
                  "SBMLWriter",
                  "SBO",
                  "SBase",
                  "SBaseExtensionPoint",
                  "SBaseList",
                  "SBasePlugin",
                  "SBasePluginCreator",
                  "SBasePluginCreatorBase",
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
                  "XMLConstructorException",
                  "XMLError",
                  "XMLErrorLog",
                  "XMLNamespaces",
                  "XMLNode",
                  "XMLToken",
                  "XMLTriple" ]

# In some languages like C#, we have to be careful about the method declaration
# that we put on the swig %{java|cs}methodmodifiers.  In particular, in C#, if
# a method overrides a base class' method, we have to add the modifier "new".
#
# FIXME: The following approach of hard-coding the list of cases is
# definitely not ideal.  We need to extract the list somehow, but it's not
# easy to do within this script (swigdoc.py) because the syntax of the
# files we read in is C++, not the target language like C#, and in C++,
# it's not obvious if the method you're looking at overrides another.  We a
# more sophisticated parser like the compiler itself, or we should write a
# small C# program to gather this info prior to running swigdoc.py.

overriders = \
{ 
'AlgebraicRule'             : [ 'clone', 'hasRequiredAttributes' ],
'AssignmentRule'            : [ 'clone', 'hasRequiredAttributes' ],
'Compartment'               : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setId', 'setName', 'unsetId', 'unsetName' ],
'CompartmentType'           : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setId', 'setName', 'unsetId', 'unsetName' ],
'Constraint'                : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredElements' ],
'Delay'                     : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredElements' ],
'Event'                     : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'hasRequiredElements', 'setId', 'setName', 'unsetId', 'unsetName', 'connectToChild', 'enablePackageInternal' ],
'EventAssignment'           : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'hasRequiredElements', 'getId' ],
'FunctionDefinition'        : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'hasRequiredElements', 'setId', 'setName', 'unsetId', 'unsetName' ],
'InitialAssignment'         : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'hasRequiredElements', 'getId' ],
'ISBMLExtensionNamespaces'  : [ 'getURI', 'getPackageName' ],
'KineticLaw'                : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'hasRequiredElements', 'connectToChild', 'enablePackageInternal' ],
'ListOf'                    : [ 'clone', 'getTypeCode', 'getElementName', 'connectToChild', 'enablePackageInternal' ],
'ListOfCompartmentTypes'    : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfCompartments'        : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfConstraints'         : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfEventAssignments'    : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfEvents'              : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfFunctionDefinitions' : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfInitialAssignments'  : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfParameters'          : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfLocalParameters'     : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfReactions'           : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfRules'               : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfSpecies'             : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfSpeciesReferences'   : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfSpeciesTypes'        : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfUnitDefinitions'     : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'ListOfUnits'               : [ 'clone', 'getTypeCode', 'getItemTypeCode', 'getElementName', 'get', 'remove' ],
'Parameter'                 : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setId', 'setName', 'unsetId', 'unsetName' ],
'LocalParameter'            : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'getDerivedUnitDefinition' ],
'Model'                     : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredElements', 'setId', 'setName', 'unsetId', 'unsetName', 'setAnnotation', 'appendAnnotation', 'connectToChild', 'enablePackageInternal' ],
'SimpleSpeciesReference'    : [ 'getId', 'getName', 'isSetId', 'isSetName', 'setId', 'setName', 'unsetId', 'unsetName' ],
'ModifierSpeciesReference'  : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes' ],
'Priority'                  : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredElements' ],
'RateRule'                  : [ 'clone', 'hasRequiredAttributes' ],
'Reaction'                  : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setId', 'setName', 'unsetId', 'unsetName', 'connectToChild', 'enablePackageInternal' ],
'Rule'                      : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredElements', 'hasRequiredAttributes', 'getId' ],
'SBMLDocument'              : [ 'clone', 'getModel', 'getTypeCode', 'getElementName', 'getNamespaces', 'connectToChild', 'enablePackageInternal' ],
'SBMLDocumentPlugin'        : [ 'clone' ],
'SBMLErrorLog'              : [ 'getError' ],
'Species'                   : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setId', 'setName', 'unsetId', 'unsetName' ],
'SpeciesReference'          : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setAnnotation', 'appendAnnotation' ],
'SpeciesType'               : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'setId', 'setName', 'unsetId', 'unsetName' ],
'StoichiometryMath'         : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredElements' ],
'Trigger'                   : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredElements', 'hasRequiredAttributes' ],
'Unit'                      : [ 'clone', 'getTypeCode', 'getElementName', 'hasRequiredAttributes' ],
'UnitDefinition'            : [ 'clone', 'getId', 'getName', 'isSetId', 'isSetName', 'getTypeCode', 'getElementName', 'hasRequiredAttributes', 'hasRequiredElements', 'setId', 'setName', 'unsetId', 'unsetName', 'connectToChild', 'enablePackageInternal' ],
'XMLNode'                   : [ 'clone' ]
}

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

  def __init__ (self, stream):
    """CHeader (stream=None) -> CHeader

    Creates a new CHeader reading from the given stream.
    """
    self.classes   = [ ]
    self.functions = [ ]
    self.classDocs = [ ]

    if stream is not None:
      self.read(stream)


  def read (self, stream):
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

      if (stripped.find('@cond doxygen-libsbml-internal') >= 0): isInternal = True
      if (stripped.find('@endcond') >= 0):                       isInternal = False

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
          doc = CClassDoc(docstring, classname, isInternal)
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
          # If it's not an enum at this point, parse it.
          if stripped.endswith('}'):
            lines   = lines[:lines.rfind('{')]
          if not stripped.startswith('enum'):

            # If this segment begins with a comment, we need to skip over it.
            searchstart = lines.rfind('*/')
            if (searchstart < 0):
              searchstart = 0

            # Find (we hope) the end of the method name.
            stop = lines[searchstart:].find('(')

            # Pull out the method name & signature.
            if (stop > 0):
              name     = lines[searchstart : searchstart + stop].split()[-1]
              endparen = lines.rfind(')')
              args     = lines[searchstart + stop : endparen + 1]
              isConst  = lines[endparen:].rfind('const')

              # Swig doesn't seem to mind C++ argument lists, even though they
              # have "const", "&", etc. So I'm leaving the arg list unmodified.
              func = Method(isInternal, docstring, name, args, (isConst > 0))

              # Reset buffer for the next iteration, to skip the part seen.
              lines = lines[endparen + 2:]

              if inClass:
                c = self.classes[-1]
                c.methods.append(func)

                # Record method variants that take different arguments.
                if c.methodVariants.get(name) == None:
                  c.methodVariants[name] = {}
                c.methodVariants[name][args] = func
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

    - isInternal
    - docstring
    - name
    - args
    - isConst
  """

  def __init__ (self, isInternal, docstring, name, args, isConst):
    """Method(isInternal, docstring name, args, isConst) -> Method

    Creates a new Method description with the given docstring, name and args,
    for the language, with special consideration if the method
    was declared constant and/or internal.
    """

    self.name       = name
    self.isConst    = isConst
    self.isInternal = isInternal

    if isInternal:
      if language == 'java':
        # We have a special Javadoc doclet that understands a non-standard
        # Javadoc tag, @internal.  When present in the documentation string
        # of a method, it causes it to be excluded from the final
        # documentation output.  @internal is something doxygen offers.
        #
        p = re.compile('(\s+?)\*/', re.MULTILINE)
        self.docstring = p.sub(r'\1* @internal\1*/', docstring)
      elif language == 'csharp':
        # We mark internal methods in a different way for C#.
        self.docstring = docstring
      else:
        self.docstring = "  @internal\n" + docstring
    else:
      self.docstring = docstring

    # In Java and C#, if a method is const and swig has to translate the type,
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

    if language == 'java' or language == 'csharp':
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

  def __init__ (self, docstring, name, isInternal):
    """CClassDoc(docstring, name) -> CClassDoc

    Creates a new CClassDoc with the given docstring and name.
    """

    # Take out excess leading blank lines.
    docstring = re.sub('/\*\*(\s+\*)+', r'/** \n *', docstring)

    self.docstring  = docstring
    self.name       = name
    self.isInternal = isInternal



def getHeadersFromSWIG (filename):
  """getHeadersFromSWIG (filename) -> (filename1, filename2, .., filenameN)

  Reads the list of %include directives from the given SWIG (.i).  The
  list of C/C++ headers (.h) included is returned.
  """
  stream = open(filename)
  lines  = stream.readlines()
  stream.close()

  lines  = [line for line in lines if line.strip().startswith('%include')]
  lines  = [line for line in lines if line.strip().endswith('.h')]
  return [line.replace('%include', '').strip() for line in lines]



def rewriteCommonReferences (docstring):
  """rewriteCommonReferences (docstring) -> docstring

  Rewrites common C++ doxygen references to match language-specific needs.
  """

  if language == 'java':  
    target = 'libsbmlConstants#'
  elif language == 'csharp':  
    target = 'libsbmlcs.libsbml.'
  elif language == 'python':
    target = 'libsbml.'
  else:
    target = ''

  if target != '':
    docstring = re.sub(r'ConversionOptionType_t#',  target, docstring)
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
    docstring = re.sub(r'ParseLogType_t#',          target, docstring)

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

  file    = match.group(1)
  ending  = match.group(2)
  stream  = open(docincpath + '/common-text/' + file, 'r')
  content = stream.read()
  stream.close()

  return content + ending



def translateIfElse (match):
  text = match.group()
  if match.group(1) == language or \
     match.group(1) == 'notcpp' or \
     match.group(1) == 'notclike':
    text =  match.group(2)
  elif match.group(4) == '@else':
    text = match.group(5)
  else:
    text = ''
  return text



def translateJavaCrossRef (match):
  prior = match.group(1)
  classname = match.group(2)
  method = match.group(3)
  return prior + '{@link ' + classname + '#' + method + '}'



def translateCSharpCrossRef (match):
  prior = match.group(1)
  classname = match.group(2)
  method = match.group(3)
  return prior + '<see cref="' + classname + '.' + method + '"/>'



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



def rewriteClassRefAddingSpace (match):
  return match.group(1) + match.group(2) + match.group(3)



def rewriteClassRef (match):
  return match.group(1) + match.group(2)



def translateClassRefJava (match):
  leading      = match.group(1)
  classname    = match.group(2)
  trailing     = match.group(3)

  if leading != '%' and leading != '(':
    return leading + '{@link ' + classname + '}' + trailing
  else:
    return leading + classname + trailing



def translateClassRefCSharp (match):
  leading      = match.group(1)
  classname    = match.group(2)
  trailing     = match.group(3)

  if leading != '%' and leading != '(':
    return leading + '<see cref="' + classname + '"/>' + trailing
  else:
    return leading + classname + trailing



def rewriteList (match):
  lead   = match.group(1);
  list   = match.group(2);
  space  = match.group(3);
  ending = match.group(4);

  list = re.sub(r'@li\b', '<li>', list)
  return lead + "<ul>\n" + lead + list + "\n" + lead + "</ul>" + space + ending;



def rewriteDeprecated (match):
  lead   = match.group(1);
  depr   = match.group(2);
  body   = match.group(3);
  ending = match.group(5);
  return lead + depr + '<div class="deprecated">' + body + '</div>\n' + lead + ending



def sanitizeForHTML (docstring):
  """sanitizeForHTML (docstring) -> docstring

  Performs HTML transformations on the C++/Doxygen docstring.
  """

  # Remove @~, which we use as a hack in Doxygen 1.7-1.8

  docstring = docstring.replace(r'@~', '')

  # First do conditional section inclusion based on the current language.
  # Our possible conditional elements and their meanings are:
  #
  #   java:     only Java
  #   python:   only Python
  #   perl:     only Perl
  #   cpp:      only C++
  #   csharp:   only C#
  #   conly:    only C
  #   clike:    C, C++
  #   notcpp:	not C++
  #   notclike: not C or C++
  #
  # The notcpp/notclike variants are because Doxygen 1.6.x doesn't have
  # @ifnot, yet sometimes we want to say "if not C or C++".

  cases = 'java|python|perl|cpp|csharp|conly|clike|notcpp|notclike'
  p = re.compile('@if\s+(' + cases + ')\s+(.+?)((@else)\s+(.+?))?@endif', re.DOTALL)
  docstring = p.sub(translateIfElse, docstring)

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

  p = re.compile('@htmlinclude\s+([^\s:;,(){}+|?"\'/]+)([\s:;,(){}+|?"\'/])', re.MULTILINE)
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

  # Convert @li into <li>, but also add <ul> ... </ul>.  This is a bit
  # simple-minded (I suppose like most of this code), but ought to work
  # for the cases we use in practice.

  p = re.compile('^(\s+\*\s+)(@li\s+.*?)(\s+)(\*/|\*\s+@(?!li\s)|\*\s+<p>)', re.MULTILINE|re.DOTALL)
  docstring = p.sub(rewriteList, docstring)

  # Wrap @deprecated content with a class so that we can style it.

  p = re.compile('^(\s+\*\s+)(@deprecated\s)((\S|\s)+)(<p>|\*/)', re.MULTILINE|re.DOTALL)
  docstring = p.sub(rewriteDeprecated, docstring)

  # Doxygen automatically cross-references class names in text to the class
  # definition page, but Javadoc does not.  Rather than having to put in a
  # lot conditional @if/@endif's into the documentation to manually create
  # cross-links just for the Java case, let's automate.  This needs to be
  # done better (e.g., by not hard-wiring the class names).

  p = re.compile(r'([^a-zA-Z0-9_.">])(' + '|'.join(libsbmlclasses) + r')\b([^:])', re.DOTALL)
  if language == 'csharp':
    docstring = p.sub(translateClassRefCSharp, docstring)
  elif language == 'java':
    docstring = p.sub(translateClassRefJava, docstring)

  # Massage method cross-references.

  p = re.compile('(\s+)(\S+?)::(\w+\s*\([^)]*?\))', re.MULTILINE)
  if language == 'csharp':
    docstring = p.sub(translateCSharpCrossRef, docstring)
  elif language == 'java':
    docstring = p.sub(translateJavaCrossRef, docstring)

  # Clean-up step needed because some of the procedures above are imperfect.
  # This converts " * * @foo" lines into " * @foo":

  p = re.compile('^(\s+)\*\s+\*\s+@', re.MULTILINE)
  docstring = p.sub(r'\1* @', docstring)

  # Take out any left-over Doxygen-style quotes, because Javadoc doesn't have
  # the %foo quoting mechanism.

  docstring = re.sub('(\s)%(\w)', r'\1\2', docstring)

  # Currently, we don't handle @ingroup.

  docstring = re.sub('@ingroup \w+', '', docstring)

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

  docstring = rewriteCommonReferences(docstring)  

  # Preliminary: rewrite some of the data type references to equivalent
  # Java types.  (Note: this rewriting affects only the documentation
  # comments inside classes & methods, not the method signatures.)

  docstring = docstring.replace(r'const char *', 'String ')
  docstring = docstring.replace(r'const char* ', 'String ')
  docstring = docstring.replace(r'an unsigned int', 'a long integer')
  docstring = docstring.replace(r'unsigned int', 'long')
  docstring = docstring.replace(r'const std::string&', 'String')
  docstring = docstring.replace(r'const std::string &', 'String ')
  docstring = docstring.replace(r'const std::string ', 'String ')
  docstring = docstring.replace(r'std::string', 'String')
  docstring = docstring.replace(r'NULL', 'null')

  # Also use Java syntax instead of "const XMLNode*" etc.

  p = re.compile(r'const (%?)(' + '|'.join(libsbmlclasses) + r')( ?)(\*|&)', re.DOTALL)
  docstring = p.sub(rewriteClassRefAddingSpace, docstring)  
  p = re.compile(r'(%?)(' + '|'.join(libsbmlclasses) + r')( ?)(\*|&)', re.DOTALL)
  docstring = p.sub(rewriteClassRefAddingSpace, docstring)  

  # Do the big work.

  docstring = sanitizeForHTML(docstring)

  # Fix up for a problem introduced by sanitizeForHTML: it puts {@link ...}
  # into the arguments of functions mentioned in @see's, if the function
  # has more than one argument.  This gets rid of the @link's.  This should
  # be fixed properly some day.

  p = re.compile(r'((@see|@throws)\s+[\w\\ ,.\'"=<>()#]*?){@link\s+([^}]+?)}')
  while re.search(p, docstring) != None:
    docstring = p.sub(r'\1\3', docstring)

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
  
  p = re.compile('@link([\s/*]+[\w\s,.:#()*]+[\s/*]*[\w():#]+[\s/*]*)@endlink', re.DOTALL)
  docstring = p.sub(r'{@link \1}', docstring)

  # Outside of @see and other constructs, dot is used to reference members
  # instead of C++'s double colon.

  docstring = docstring.replace(r'::', '.')

  # Need to escape quotation marks.  The reason is that the
  # %javamethodmodifiers directives created for use with SWIG will
  # themselves be double-quoted strings, and leaving embedded quotes
  # will completely screw that up.

  docstring = docstring.replace('"', "'")
  docstring = docstring.replace(r"'", r"\'")

  return docstring



def rewriteDocstringForCSharp (docstring):
  """rewriteDocstringForCSharp (docstring) -> docstring

  Performs some mimimal C#-specific sanitizations on the
  C++/Doxygen docstring.
  """

  docstring = rewriteCommonReferences(docstring)  

  # Preliminary: rewrite some of the data type references to equivalent
  # C# types.  (Note: this rewriting affects only the documentation
  # comments inside classes & methods, not the actual method signatures.)

  docstring = docstring.replace(r'const char *', 'string ')
  docstring = docstring.replace(r'const char* ', 'string ')
  docstring = docstring.replace(r'an unsigned int', 'a long integer')
  docstring = docstring.replace(r'unsigned int', 'long')
  docstring = docstring.replace(r'const std::string&', 'string')
  docstring = docstring.replace(r'const std::string &', 'string ')
  docstring = docstring.replace(r'const std::string', 'string')
  docstring = docstring.replace(r'std::string', 'string')
  docstring = docstring.replace(r'const ', '')
  docstring = docstring.replace(r'NULL', 'null')
  docstring = docstring.replace(r'boolean', 'bool')

  # Use C# syntax instead of "const XMLNode*" etc.

  p = re.compile(r'const (%?)(' + '|'.join(libsbmlclasses) + r')( ?)(\*|&)', re.DOTALL)
  docstring = p.sub(rewriteClassRefAddingSpace, docstring)  
  p = re.compile(r'(%?)(' + '|'.join(libsbmlclasses) + r')( ?)(\*|&)', re.DOTALL)
  docstring = p.sub(rewriteClassRefAddingSpace, docstring)  

  # <code> has its own special meaning in C#; we have to turn our input
  # file's uses of <code> into <c>.  Conversely, we have to turn our
  # uses of verbatim to <code>.

  p = re.compile(r'<code>(.+?)</code>', re.DOTALL)
  docstring = p.sub(r'<c>\1</c>', docstring)
  p = re.compile('@verbatim(.+?)@endverbatim', re.DOTALL)
  docstring = p.sub(r'<code>\1</code>', docstring)

  # Do replacements on some documentation text we sometimes use.

  p = re.compile(r'libsbmlConstants([@.])')
  docstring = p.sub(r'libsbmlcs.libsbml\1', docstring)

  # Fix @link for constants that we forgot conditionalize in the source.

  p = re.compile(r'@link +([A-Z_0-9]+?)@endlink', re.DOTALL)
  docstring = p.sub(r'@link libsbml.\1@endlink', docstring)

  # Can't use math symbols.  Kluge around it.

  docstring = re.sub(r'\\f\$\\geq\\f\$', '>=', docstring)
  docstring = re.sub(r'\\f\$\\leq\\f\$', '<=', docstring)
  docstring = re.sub(r'\\f\$\\times\\f\$', '*', docstring)

  # Some additional special cases.

  docstring = docstring.replace(r'SBML_formulaToString()', 'libsbmlcs.libsbml.formulaToString()')
  docstring = docstring.replace(r'SBML_parseFormula()', 'libsbmlcs.libsbml.parseFormula()')

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

  docstring = rewriteCommonReferences(docstring)  

  # Take out the C++ comment start and end.

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
  docstring = docstring.replace(r'@c true', '@c True')
  docstring = docstring.replace(r'@c false', '@c False')

  # Also use Python syntax instead of "const XMLNode*" etc.

  p = re.compile(r'const (%?)(' + '|'.join(libsbmlclasses) + r') ?(\*|&)', re.DOTALL)
  docstring = p.sub(rewriteClassRef, docstring)  
  p = re.compile(r'(%?)(' + '|'.join(libsbmlclasses) + r') ?(\*|&)', re.DOTALL)
  docstring = p.sub(rewriteClassRef, docstring)  

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

  docstring = rewriteCommonReferences(docstring)  

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



def processClassMethods(ostream, cclass):
  # In the Python docs, we have to combine the docstring for methods with
  # different signatures and write out a single method docstring.  In the
  # other languages, we write out separate docstrings for every method
  # having a different signature.

  if language == 'python':
    written = {}
    for m in cclass.methods:
      if m.name + m.args in written:
        continue
      if m.name.startswith('~'):
        continue

      if cclass.methodVariants[m.name].__len__() > 1:

        # This method has more than one variant.  It's possible some or all
        # of them are marked @internal.  Therefore, before we start writing
        # a statement that there are multiple variants, we must check that
        # we're left with more than one non-internal method to document.
        count = 0
        for argVariant in list(cclass.methodVariants[m.name].values()):
          if re.search('@internal', argVariant.docstring) == None:
            count += 1
        if count <= 1:
          continue

        newdoc = ' This method has multiple variants that differ in the' + \
                 ' arguments\n they accept.  Each is described separately' + \
                 ' below.\n'

        for argVariant in list(cclass.methodVariants[m.name].values()):
          # Each entry in the methodVariants dictionary is itself a dictionary.
          # The dictionary entries are keyed by method arguments (as strings).
          # The dictionary values are the 'func' objects we use.
          if re.search('@internal', argVariant.docstring) == None:
            newdoc += "\n <hr>\n Method variant with the following"\
                      + " signature:\n <pre class='signature'>" \
                      + argVariant.name \
                      + rewriteDocstringForPython(argVariant.args) \
                      + "</pre>\n\n"
            newdoc += rewriteDocstringForPython(argVariant.docstring)
          written[argVariant.name + argVariant.args] = 1
      else:
        newdoc = rewriteDocstringForPython(m.docstring)
      ostream.write(formatMethodDocString(m.name, cclass.name, newdoc, m.isInternal, m.args))
      written[m.name + m.args] = 1
  else: # Not python
    for m in cclass.methods:
      if m.name.startswith('~'):
        continue
      if language == 'java':
        newdoc = rewriteDocstringForJava(m.docstring)
      elif language == 'csharp':
        newdoc = rewriteDocstringForCSharp(m.docstring)
      elif language == 'perl':
        newdoc = rewriteDocstringForPerl(m.docstring)  
      ostream.write(formatMethodDocString(m.name, cclass.name, newdoc, m.isInternal, m.args))

  ostream.flush()



def formatMethodDocString (methodname, classname, docstring, isInternal, args=None):
  if language == 'java':
    pre  = '%javamethodmodifiers'
    post = ' public'
  elif language == 'csharp':
    pre  = '%csmethodmodifiers'
    # See the comment for the definition of 'overriders' for more info.
    if classname in overriders and methodname in overriders[classname]:
      post = ' public new'
    else:
      post = ' public'
    if isInternal:
      post = ' /* libsbml-internal */' + post
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
    output += '%s\n\n%s%s\n\n\n'   % (methodname, docstring.strip(), post)
  elif language == 'python':
    output += '%s "\n %s%s\n";\n\n\n' % (methodname, docstring.strip(), post)
  else:
    output += '%s%s "\n%s%s\n";\n\n\n' % (methodname, args, docstring.strip(), post)

  return output



def generateFunctionDocString (methodname, docstring, args, isInternal):
  if language == 'java':
    doc = rewriteDocstringForJava(docstring)
  elif language == 'csharp':
    doc = rewriteDocstringForCSharp(docstring)
  elif language == 'python':
    doc = rewriteDocstringForPython(docstring)
  elif language == 'perl':
    doc = rewriteDocstringForPerl(docstring)
  return formatMethodDocString(methodname, None, doc, isInternal, args)



def generateClassDocString (docstring, classname):
  if language == 'java':
    pre = '%typemap(javaimports) '
    docstring = rewriteDocstringForJava(docstring).strip()
    output = pre + classname + ' "\n' + docstring + '\n"\n\n\n'

  elif language == 'csharp':
    pre = '%typemap(csimports) '
    docstring = rewriteDocstringForCSharp(docstring).strip()
    output = pre + classname + ' "\n using System;\n using System.Runtime.InteropServices;\n\n' + docstring + '\n"\n\n\n'

  elif language == 'python':
    pre = '%feature("docstring") '
    docstring = rewriteDocstringForPython(docstring).strip()
    output = pre + classname + ' "\n ' + docstring + '\n";\n\n\n'

  elif language == 'perl':
    docstring = rewriteDocstringForPerl(docstring).strip()
    output = '=back\n\n=head2 ' + classname + '\n\n' + docstring + '\n\n=over\n\n\n'

  return output



def processClasses (ostream, classes):
  for c in classes:
    processClassMethods(ostream, c)



def processFunctions (ostream, functions):
  for f in functions:
    ostream.write(generateFunctionDocString(f.name, f.docstring, f.args, f.isInternal))



def processClassDocs (ostream, classDocs):
  for c in classDocs:
    if c.isInternal == False:
      ostream.write(generateClassDocString(c.docstring, c.name))



def processFile (filename, ostream):
  """processFile (filename, ostream)

  Reads the the given header file and writes to ostream the necessary SWIG
  incantation to annotate each method (or function) with a docstring
  appropriate for the given language.
  """

  istream = open(filename)
  header  = CHeader(istream)
  istream.close()

  processClassDocs(ostream, header.classDocs)
  processClasses(ostream, header.classes)
  processFunctions(ostream, header.functions)

  ostream.flush()



def main (args):
  """usage: swigdoc.py [java | python | perl | csharp] -Ipath -Dpath libsbml.i output.i

  java | python | perl | csharp  generate docstrings for this language module.
  path                           is the path to the libsbml src/ directory.
  libsbml.i                      is the master libsbml SWIG interface file.
  output.i                       is the file to output the SWIG docstrings.
  """

  if len(args) != 6:
    print((main.__doc__))
    sys.exit(1)

  global docincpath
  global language

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
    processFile(filename, stream)

  if os.path.exists('local-doc-extras.i'):
    stream.write('\n%include "local-doc-extras.i"\n')

  if language == 'perl':
    stream.write('=cut\n')

  stream.close()


if __name__ == '__main__':
  main(sys.argv)
 

