/**
 * @file    local.i
 * @brief   Python-specific SWIG directives for wrapping libSBML API
 * @author  Ben Bornstein
 * @author  Ben Kovitz
 * @author  Akiya Jouraku
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/


/**
 * Turn on (minimal) Python docstrings and then append our own.
 */
%feature("autodoc", "1");
%include "pydoc.i"

/**
 *  Wraps std::cout, std::cerr, std::clog, std::ostream, and std::ostringstream, 
 *
 * (sample code) -----------------------------------------------------
 *
 * 1. wraps std::cout
 *
 *    xos = libsbml.XMLOutputStream(libsbml.cout)
 *
 * 2. wraps std::cerr
 *
 *    d = libsbml.readSBML("foo.xml")
 *    if d.getNumErrors() > 0 :
 *       d.printErrors(libsbml.cerr)
 *    
 *
 * 3. wraps std::ostringstream
 *
 *    oss = libsbml.ostringstream()
 *    xos = libsbml.XMLOutputStream(oss)
 *    ...
 *    libsbml.endl(oss)
 *    s = oss.str()
 * 
 */
%include <std_alloc.i>
%include <std_basic_string.i>
%include <std_string.i>

#pragma SWIG nowarn=509
%warnfilter(401) basic_ios<char>;

namespace std
{
  // Template class basic ios
  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_ios : public ios_base {};

  // Template class basic_ostream
  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  class basic_ostream : virtual public basic_ios<_CharT, _Traits> 
  {
    public:
      explicit
      basic_ostream(std::basic_streambuf<_CharT, _Traits>* __sb);
      virtual 
      ~basic_ostream();
  };

  // Template class basic_ostringstream
  template<typename _CharT, typename _Traits = char_traits<_CharT>,
           typename _Alloc = allocator<_CharT> >
  class basic_ostringstream : public basic_ostream<_CharT, _Traits>
  {
    public:
      explicit
      basic_ostringstream(std::ios_base::openmode __mode = std::ios_base::out);
      ~basic_ostringstream();

      basic_string<_CharT, _Traits, _Alloc> 
      str() const;

      void
      str(const basic_string<_CharT, _Traits, _Alloc>& __s);
  };

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  basic_ostream<_CharT, _Traits>& 
  endl(basic_ostream<_CharT, _Traits>&);

  template<typename _CharT, typename _Traits = char_traits<_CharT> >
  basic_ostream<_CharT, _Traits>& 
  flush(basic_ostream<_CharT, _Traits>&);
}

namespace std
{
  /**
   *  std::ostream and std::ostringstream 
   *  (std::ios is not wrapped)
   */
  typedef basic_ios<char>           ios;
  typedef basic_ostream<char>       ostream ;
  typedef basic_ostringstream<char> ostringstream ;

  %template()              basic_ios<char>;
  %template(ostream)       basic_ostream<char>;
  %template(ostringstream) basic_ostringstream<char>;

  /**
   *  output manipulators
   */
  %template(endl)  endl<char, char_traits<char> >;
  %template(flush) flush<char, char_traits<char> >;

  /**
   *  std::cout, std::cerr, and std::clog.
   */
  %immutable;
  extern std::ostream cout;
  extern std::ostream cerr;
  extern std::ostream clog;
  %mutable;
}


/**
 * Convert an SBase object to a string.
 *
%extend SBase
{
  %pythoncode
  {
    def __str__(self):
      return self.toSBML()
  }
}*/


/**
 * Allows ListOf objects:
 *
 *   - To be indexed and sliced, e.g. lst[0].
 */
%extend ListOf
{
  int __len__()
  {
    return self->size();
  }

  %pythoncode
  {
    def __getitem__(self, key):

      try:
         keyIsSlice = isinstance(key, slice)
      except:
         keyIsSlice = 0

      if keyIsSlice:
        start = key.start
        if start is None:
          start = 0
        stop = key.stop
        if stop is None:
          stop = self.size()
        return [self[i] for i in range(
          self._fixNegativeIndex(start), self._fixNegativeIndex(stop)
        )]

      key = self._fixNegativeIndex(key)
      if key < 0 or key >= self.size():
        raise IndexError(key)
      return self.get(key)


    def _fixNegativeIndex(self, index):
      if index < 0:
        return index + self.size()
      else:
        return index


    def __iter__(self):
      for i in range(self.size()):
        yield self[i]


    def __repr__(self):
      return "[" + ", ".join([repr(self[i]) for i in range(len(self))]) + "]"


    def __str__(self):
      return repr(self)
  }
}


/**
 * Convert SBase objects into the most specific type possible.
 */
%typemap(out) SBase*
{
  $result = SWIG_NewPointerObj($1, GetDowncastSwigType($1), $owner | %newpointer_flags);
}

/**
 * Convert Rule objects into the most specific type possible.
 */
%typemap(out) Rule*
{
  $result = SWIG_NewPointerObj($1, GetDowncastSwigType($1), $owner | %newpointer_flags);
}


/**
 * Add an equality operator to SBase.  All subclasses of SBase
 * will inherit this method.
 *
 * The %extend rewrites __cmp__ such that two objects of
 * disimilar type can be compared without throwing a TypeError.  For
 * example: the following will return false and not throw an exception:
 *
 *   c = libsbml.Compartment()
 *   n = 5
 *   c == n
 *
 * For more information, see testEquality() in test/TestPython.py
 */

%define SWIGPYTHON__CMP__(CLASS)
%extend CLASS
{
  %pythoncode
  {
    def __cmp__(self, rhs):
      if hasattr(self, 'this') and hasattr(rhs, 'this'):
        if self.this == rhs.this: return 0
      return 1
  }
}
%enddef

SWIGPYTHON__CMP__(SBase)
SWIGPYTHON__CMP__(SBMLWriter)
SWIGPYTHON__CMP__(SBMLReader)
SWIGPYTHON__CMP__(ASTNode)
SWIGPYTHON__CMP__(CVTerm)
SWIGPYTHON__CMP__(Date)
SWIGPYTHON__CMP__(ModelHistory)
SWIGPYTHON__CMP__(ModelCreator)
SWIGPYTHON__CMP__(XMLNamespaces)
SWIGPYTHON__CMP__(XMLAttributes)
SWIGPYTHON__CMP__(XMLToken)
SWIGPYTHON__CMP__(XMLTriple)
SWIGPYTHON__CMP__(XMLError)
SWIGPYTHON__CMP__(XMLErrorLog)
SWIGPYTHON__CMP__(XMLHandler)
SWIGPYTHON__CMP__(XMLOutputStream)
SWIGPYTHON__CMP__(XMLInputStream)

/**
 * The features directives below override the default SWIG generated
 * code for certain methods.  The idea is to tell SWIG to disown the
 * passed-in object.  The containing object will takeover ownership
 * and delete the object as appropriate.  This avoids a deadly
 * double-delete which can result in a segmentation fault.  For
 * example, each SBase that is appended to a ListOf is subsequently
 * owned by that ListOf.
 */

%define TAKEOVER_OWNERSHIP(METHOD_NAME,ARG_INDEX)
%feature("pythonprepend")
METHOD_NAME
%{
        if args[ARG_INDEX] is not None: args[ARG_INDEX].thisown = 0
%}
%enddef


// ----------------------------------------------------------------------
// ListOf
// ----------------------------------------------------------------------

TAKEOVER_OWNERSHIP(ListOf::appendAndOwn(SBase*),1)

// ----------------------------------------------------------------------
// ASTNode
// ----------------------------------------------------------------------

TAKEOVER_OWNERSHIP(ASTNode::addChild(ASTNode*),1)
TAKEOVER_OWNERSHIP(ASTNode::prependChild(ASTNode*),1)

// ----------------------------------------------------------------------
// FomulaUnitsData
// ----------------------------------------------------------------------

TAKEOVER_OWNERSHIP(FormulaUnitsData::setUnitDefinition(UnitDefinition*),1)
TAKEOVER_OWNERSHIP(FormulaUnitsData::setPerTimeUnitDefinition(UnitDefinition *),1)
TAKEOVER_OWNERSHIP(FormulaUnitsData::setEventTimeUnitDefinition(UnitDefinition *),1)
TAKEOVER_OWNERSHIP(FormulaUnitsData::setL1SpeciesConcPerTimeUnitDefinition(UnitDefinition *),1)
TAKEOVER_OWNERSHIP(FormulaUnitsData::setL1SpeciesConcUnitDefinition(UnitDefinition *),1)

// ----------------------------------------------------------------------
// SBMLReader
// ----------------------------------------------------------------------

%pythoncode
%{
import sys
import os.path


def conditional_abspath (filename):
  """conditional_abspath (filename) -> filename

  Returns filename with an absolute path prepended, if necessary.
  Some combinations of platforms and underlying XML parsers *require*
  an absolute path to a filename while others do not.  This function
  encapsulates the appropriate logic.  It is used by readSBML() and
  SBMLReader.readSBML().
  """
  if sys.platform.find('cygwin') != -1:
    return filename
  else:
    return os.path.abspath(filename)
%}

%feature("shadow")
SBMLReader::readSBML(const std::string&)
%{
  def readSBML(*args):
    """readSBML(filename) -> SBMLDocument

    Reads an SBML document from the given file.  If filename does not exist
    or is not an SBML file, a fatal error will be logged.  Errors can be
    identified by their unique ids, e.g.:

      reader = libsbml.SBMLReader()
      d      = reader.readSBML(filename)

      if d.getNumErrors() > 0:
        pm = d.getError(0)
        if pm.getErrorId() == libsbml.XMLFileUnreadable
        if pm.getErrorId() == libsbml.XMLTagMismatch: 
    """
    args_copy    = list(args)
    args_copy[1] = conditional_abspath(args[1])
    return _libsbml.SBMLReader_readSBML(*args_copy)
%}


/**
 * Since we cannot seem to "shadow" readSBML() (maybe because it's
 * not a method of some object, but rather a top-level function, we
 * employ the following HACK: Tell SWIG to ignore readSBML and just
 * define it in terms of SBMLReader.readSBML().  This is less than
 * ideal, because the libSBML C/C++ core does essentially the same
 * thing, so now we're repeating ourselves.
 */

%ignore readSBML(const char*);

%pythoncode
%{
def readSBML(*args):
  """readSBML(filename) -> SBMLDocument

  Reads an SBML document from the given file.  If filename does not exist
  or is not an SBML file, a fatal error will be logged.  Errors can be
  identified by their unique ids, e.g.:

    d = readSBML(filename)

    if d.getNumErrors() > 0:
      pm = d.getError(0)
      if pm.getErrorId() == libsbml.XMLFileUnreadable
      if pm.getErrorId() == libsbml.XMLTagMismatch: 

  """
  reader = SBMLReader()
  return reader.readSBML(args[0])
%}

// ----------------------------------------------------------------------
// Layout Extension
// ----------------------------------------------------------------------


#ifdef USE_LAYOUT
%include layout_local.i
#endif
