/**
 * \file    local.i
 * \brief   Python-specific SWIG directives for wrapping libSBML API
 * \author  Ben Bornstein and Ben Kovitz
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


/**
 * Turn on (minimal) Python docstrings and then append our own.
 */
%feature("autodoc", "1");
%include "pydoc.i"

%include "std_iostream.i"
%include "std_sstream.i"


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
  $result = SWIG_NewPointerObj($1, GetDowncastSwigType($1), 0);
}

/**
 * Convert Rule objects into the most specific type possible.
 */
%typemap(out) Rule*
{
  $result = SWIG_NewPointerObj($1, GetDowncastSwigType($1), 0);
}


/**
 * Add an equality operator to SBase.  All subclasses of SBase
 * will inherit this method.
 *
 * The %feature("shadow") rewrites __cmp__ such that two objects of
 * disimilar type can be compared without throwing a TypeError.  For
 * example: the following will return false and not throw an exception:
 *
 *   c = libsbml.Compartment()
 *   n = 5
 *   c == n
 *
 * The %extend forces the generation of a Python shadow class method
 * named __cmp__.  For some strange reason, this is order dependent.
 * The directive %feature must occur before %extend.  If not, the
 * %feature directive seems to be ignored.  If anything, it seems like
 * it should be the reverse.
 *
 * For more information, see testEquality() in accept.py.
 */
%feature("shadow") SBase::__cmp__
{
  def __cmp__(self, rhs):
    if hasattr(self, 'this') and hasattr(rhs, 'this'):
      if self.this == rhs.this: return 0
    return 1
}

%extend SBase
{
  bool __cmp__(const SBase& rhs) { return self != &rhs; }
}


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
// RDFAnnotationParser
// ----------------------------------------------------------------------

TAKEOVER_OWNERSHIP(FormulaUnitsData::setUnitDefinition(UnitDefinition*),1)
TAKEOVER_OWNERSHIP(FormulaUnitsData::setPerTimeUnitDefinition(UnitDefinition *),1)
TAKEOVER_OWNERSHIP(FormulaUnitsData::setEventTimeUnitDefinition(UnitDefinition *),1)

/**
 * The features directives below override the default SWIG generated
 * python code for methods which return new SBase* object.  
 * The idea is to tell SWIG to give the ownership of new SBase* object
 * to caller.
 * Generally, %newobject directive is used for such ownership management.
 * Regarding to SBase*, however, the above "%typemap(out) SBase*" directive 
 * overrides the native SWIG_NewPointerObj() function (used in libsbml_wrap.cpp)
 * and reset ownership flag (SWIG_POINTER_OWN is set by %newobject) to 0. 
 */
// ----------------------------------------------------------------------
// ListOf
// ----------------------------------------------------------------------

%feature("shadow")
ListOf::remove(unsigned int)
%{
  def remove(*args):
    result = _libsbml.ListOf_remove(*args)
    if result is not None: result.thisown = 1
    return result
%}


%feature("shadow")
ListOf::remove(const std::string&)
%{
  def remove(*args):
    result = _libsbml.ListOf_remove(*args)
    if result is not None: result.thisown = 1
    return result
%}

// ----------------------------------------------------------------------
// SBase* *::clone()
// ----------------------------------------------------------------------

%feature("shadow")
SBase::clone() const
%{
      def clone(*args):
        result = _libsbml.SBase_clone(*args)
        if result is not None: result.thisown = 1
        return result
%}

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
        if pm.getId() == libsbml.SBML_READ_ERROR_FILE_NOT_FOUND: ..
        if pm.getId() == libsbml.SBML_READ_ERROR_NOT_SBML: ...
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
      if pm.getId() == libsbml.SBML_READ_ERROR_FILE_NOT_FOUND: ...
      if pm.getId() == libsbml.SBML_READ_ERROR_NOT_SBML: ...
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
