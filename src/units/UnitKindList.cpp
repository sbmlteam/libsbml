/**
 * @file    UnitKindList.cpp
 * @brief   Maintains a list of UnitKinds. 
 * @author  Ben Bornstein
 *
 * $UnitKind:  Exp $
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
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
