/**
 * @cond doxygen-libsbml-internal
 *
 * @file    UnitKindList.cpp
 * @brief   Maintains a list of UnitKinds. 
 * @author  Ben Bornstein
 *
 * $UnitKind:  Exp $
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <algorithm>
#include <sbml/units/UnitKindList.h>


using std::string;
using std::vector;


/**
 * @return true if UnitKind is already in this UnitKindList, false otherwise.
 */
bool
UnitKindList::contains (const std::string UnitKind) const
{
  UnitKindList::const_iterator end = mKinds.end();
  
  return std::find(mKinds.begin(), end, UnitKind) != end;
}


/**
 * Removes the first occurence of this UnitKind in this UnitKindList.
 */
void
UnitKindList::removeUnitKind (const std::string UnitKind)
{
  UnitKindList::iterator end = mKinds.end();
  UnitKindList::iterator pos = std::find(mKinds.begin(), end, UnitKind);


  if (pos != end) mKinds.erase(pos, pos+1);
}


/** @endcond doxygen-libsbml-internal */
