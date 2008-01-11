/**
 * @file    local.i
 * @brief   Ruby-specific SWIG directives for wrapping libSBML API
 * @author  Alex Gutteridge
 * @author  Ben Bornstein
 * @author  Akiya Jouraku
 *
 * $Id$
 * $Source$
 */

/*
 *This file is part of libSBML.  Please visit http://sbml.org for more
 *information about SBML, and the latest version of libSBML.
 *
 *Copyright 2005-2007 California Institute of Technology.
 *Copyright 2002-2005 California Institute of Technology and
 *                    Japan Science and Technology Corporation.
 *
 *This library is free software; you can redistribute it and/or modify it
 *under the terms of the GNU Lesser General Public License as published by
 *the Free Software Foundation.  A copy of the license agreement is provided
 *in the file named "LICENSE.txt" included with this software distribution
 *and also available online as http://sbml.org/software/libsbml/license.html
 */

%trackobjects;

/**
 * Convert an SBase object to a string.
 */

%extend SBase
{
   char* __str__(void){
     return self->toSBML();
   }
}

/**
 * Allows ListOf objects:
 *
 *   - To be indexed and sliced, e.g. lst[0].
 */

%mixin ListOf "Enumerable";

%extend ListOf
{
  int __len__()
  {
    return self->size();
  }

  SBase* __getitem__(int i)
  {
     return self->get(fixNegativeIndex(i,self));
  }

  void each(void)
  {
     uint i;
     for(i=0;i<self->size();i++){    
       rb_yield(SWIG_NewPointerObj(self->get(i),
       GetDowncastSwigType(self->get(i)), 0));
     }
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


// ----------------------------------------------------------------------
// takeover ownership
// ----------------------------------------------------------------------

/**
 * - void ListOf::appendAndOwn(SBase* item)
 */
%apply SWIGTYPE *DISOWN {SBase* item};
%apply SWIGTYPE * {const SBase* item};

/**
 * - void ASTNode::addChild (ASTNode* child)
 * - void ASTNode::prependChild (ASTNode* child)
 */
%apply SWIGTYPE *DISOWN {ASTNode* child};
%apply SWIGTYPE * {const ASTNode* child};

/**
 * - void FormulaUnitsData::setUnitDefinition(UnitDefinition * ud)
 * - void FormulaUnitsData::setPerTimeUnitDefinition(UnitDefinition * ud)
 * - void FormulaUnitsData::setEventTimeUnitDefinition(UnitDefinition * ud)
 * - void FormulaUnitsData::setL1SpeciesConcPerTimeUnitDefinition(UnitDefinition * ud)
 * - void FormulaUnitsData::setL1SpeciesConcUnitDefinition(UnitDefinition * ud)
 */
%apply SWIGTYPE *DISOWN {UnitDefinition* ud};
%apply SWIGTYPE * {const UnitDefinition* ud};

// ----------------------------------------------------------------------
// Layout Extension
// ----------------------------------------------------------------------


#ifdef USE_LAYOUT
%include layout_local.i
#endif
