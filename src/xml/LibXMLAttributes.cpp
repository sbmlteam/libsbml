/**
 * \file    LibXMLAttributes.cpp
 * \brief   Creates new XMLAttributes from "raw" LibXML attributes.
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


#include <sbml/xml/LibXMLTranscode.h>
#include <sbml/xml/LibXMLAttributes.h>


using namespace std;


/**
 * Creates a new XMLAttributes set from the given "raw" LibXML attributes.
 */
LibXMLAttributes::LibXMLAttributes (  const xmlChar**     attributes
                                    , const unsigned int& size )
{
  mNames .reserve(size);
  mValues.reserve(size);

  for (unsigned int n = 0; n < size; ++n)
  {
    const string name    = LibXMLTranscode( attributes[5 * n]     );
    const string prefix  = LibXMLTranscode( attributes[5 * n + 1] );
    const string uri     = LibXMLTranscode( attributes[5 * n + 2] );

    const xmlChar* start = attributes[5 * n + 3];
    const xmlChar* end   = attributes[5 * n + 4];
    int length           = (end - start) / sizeof(xmlChar);

    const string value   =  LibXMLTranscode((length > 0) ? start : 0, length);

    mNames .push_back( XMLTriple(name, uri, prefix) );
    mValues.push_back( value );
  }
}


/**
 * Destroys this Attribute set.
 */
LibXMLAttributes::~LibXMLAttributes ()
{
}
