/**
 * @file    IdList.cpp
 * @brief   Maintains a list of SIds.  Useful for finding cycles.
 * @author  Ben Bornstein
 *
 * $Id$
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
#include "IdList.h"


using std::string;
using std::vector;


/**
 * @return true if id is already in this IdList, false otherwise.
 */
bool
IdList::contains (const std::string id) const
{
  IdList::const_iterator end = mIds.end();
  
  return std::find(mIds.begin(), end, id) != end;
}


/**
 * Removes all ids in this IdList before the given id.
 */
void
IdList::removeIdsBefore (const std::string id)
{
  IdList::iterator end = mIds.end();
  IdList::iterator pos = std::find(mIds.begin(), end, id);


  if (pos != end) mIds.erase(mIds.begin(), pos);
}
