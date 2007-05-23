/**
 * \file    local.i
 * \brief   Perl-specific SWIG directives for wrapping libSBML API
 * \author  TBI {xtof,raim}@tbi.univie.ac.at
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 TBI
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Christoph Flamm and Rainer Machne
 *
 * Contributor(s):
 */

/**
 * Convert SBase and Rule objects into the most specific type possible.
 */
%typemap(out) SBase*, Rule*
{
  ST(argvi) = sv_newmortal();
  SWIG_MakePtr(ST(argvi++), (void*)$1, GetDowncastSwigType($1), SWIG_SHADOW|0);
}

/**
 * typemap to handle functions which take a FILE*
 */
%typemap(in) FILE * {
  if (SvOK($input)) /* check for undef */
        $1 = PerlIO_findFILE(IoIFP(sv_2io($input)));
  else  $1 = NULL;
}

/**
 * By default, returned boolean false (C++) is converted to "" (Perl) in 
 * SWIG 1.3.31.
 * The following typemap converts returned boolean value to 0 (false) or 
 * 1 (true) like C/C++ for compatibility.
 */
%typemap(out) bool
{
   ST(argvi) = sv_newmortal();
   sv_setiv(ST(argvi++), (IV) $1);
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

// ----------------------------------------------------------------------
// ListOf
// ----------------------------------------------------------------------

%feature("shadow") ListOf::appendAndOwn(SBase*)
%{
  sub appendAndOwn {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ListOf_appendAndOwn(@_);
  }
%}

// ----------------------------------------------------------------------
// ASTNode
// ----------------------------------------------------------------------

%feature("shadow") ASTNode::addChild(ASTNode*)
%{
  sub addChild {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ASTNode_addChild(@_);
  }
%}

%feature("shadow") ASTNode::prependChild(ASTNode*)
%{
  sub prependChild {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ASTNode_prependChild(@_);
  }
%}

// ----------------------------------------------------------------------
// RDFAnnotationParser
// ----------------------------------------------------------------------

%feature("shadow") FormulaUnitsData::setUnitDefinition(UnitDefinition*)
%{
  sub setUnitDefinition {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::FormulaUnitsData_setUnitDefinition(@_);
  }
%}

%feature("shadow") FormulaUnitsData::setPerTimeUnitDefinition(UnitDefinition*)
%{
  sub setPerTimeUnitDefinition {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::FormulaUnitsData_setPerTimeUnitDefinition(@_);
  }
%}

%feature("shadow") FormulaUnitsData::setEventTimeUnitDefinition(UnitDefinition*)
%{
  sub setEventTimeUnitDefinition {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::FormulaUnitsData_setEventTimeUnitDefinition(@_);
  }
%}

/**
 * The features directives below override the default SWIG generated
 * perl code for methods which return new SBase* object.
 * The idea is to tell SWIG to give the ownership of new SBase* object
 * to caller.
 * Generally, %newobject directive is used for such ownership management.
 * Regarding to SBase*, however, the above "%typemap(out) SBase*" directive
 * overrides the native SWIG_NewPointerObj() function (used in LibSBML_wrap.cxx)
 * and reset ownership flag (SWIG_OWNER is set by %newobject) to 0.
 */

// ----------------------------------------------------------------------
// ListOf
// ----------------------------------------------------------------------

%feature("shadow") ListOf::remove(unsigned int)
%{
  sub remove {
    my $result = LibSBMLc::ListOf_remove(@_);
    $result->ACQUIRE() if (defined $result);
    return $result;
  }
%}

%feature("shadow") ListOf::remove(const std::string&)
%{
  sub remove {
    my $result = LibSBMLc::ListOf_remove(@_);
    $result->ACQUIRE() if (defined $result);
    return $result;
  }
%}


// ----------------------------------------------------------------------
// SBase* *::clone()
// ----------------------------------------------------------------------

%feature("shadow")
SBase::clone() const
%{
  sub clone {
    my $result = LibSBMLc::SBase_clone(@_);
    $result->ACQUIRE() if (defined $result);
    return $result;
  }
%}


/**
 * Wraps standard output streams
 */

%{
#include <iostream>
%}

%rename(COUT) cout;
%rename(CERR) cerr;
%rename(CLOG) clog;

namespace std
{
%immutable;
extern std::ostream cout;
extern std::ostream cerr;
extern std::ostream clog;
%mutable;
}

%inline
%{
  std::ostream& endl (std::ostream& os) 
  {
    return std::endl(os);
  }
%}

/**
 * Renames functions whose name is Perl keyword
 *
 */

%rename(nextToken) XMLInputStream::next;
%rename(nextToken) XMLTokenizer::next;

