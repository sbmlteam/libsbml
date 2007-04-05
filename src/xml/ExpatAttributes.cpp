/**
 * \file    ExpatAttributes.cpp
 * \brief   Creates new XMLAttributes from "raw" Expat attributes.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
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


#include <sbml/xml/ExpatAttributes.h>


using namespace std;


/**
 * Creates a new XMLAttributes set from the given "raw" Expat attributes.
 * The Expat attribute names are assumed to be in namespace triplet form
 * separated by sepchar.
 */
ExpatAttributes::ExpatAttributes (const XML_Char** attrs, const XML_Char sep)
{
  unsigned int size = 0;
  while (attrs[2 * size]) ++size;

  mNames .reserve(size);
  mValues.reserve(size);

  for (unsigned int n = 0; n < size; ++n)
  {
    mNames .push_back( XMLTriple( attrs[2 * n], sep ) );
    mValues.push_back( string   ( attrs[2 * n + 1]  ) );
  }
}


/**
 * Destroys this Attribute set.
 */
ExpatAttributes::~ExpatAttributes ()
{
}
