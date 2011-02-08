/**
 * @cond doxygen-libsbml-internal
 *
 * @file    IdList.cpp
 * @brief   Maintains a list of SIds.  Useful for finding cycles.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 */
/* Copyright (C) 2009-2011 jointly by the following organizations: 
/*     1. California Institute of Technology, Pasadena, CA, USA
/*     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
/*  
/* Copyright (C) 2006-2008 by the California Institute of Technology,
/*     Pasadena, CA, USA 
/*  
/* Copyright (C) 2002-2005 jointly by the following organizations: 
/*     1. California Institute of Technology, Pasadena, CA, USA
/*     2. Japan Science and Technology Agency, Japan
/* 
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

LIBSBML_CPP_NAMESPACE_BEGIN

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

LIBSBML_CPP_NAMESPACE_END

/** @endcond */
