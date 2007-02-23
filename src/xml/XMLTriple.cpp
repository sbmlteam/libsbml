/**
 * \file    XMLTriple.cpp
 * \brief   Stores an XML namespace triple.
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


#include "XMLTriple.h"


using namespace std;


/**
 * Creates a new empty XMLTriple.
 */
XMLTriple::XMLTriple ()
{
}


/**
 * Creates a new XMLTriple.
 */
XMLTriple::XMLTriple (  const string&  name
                      , const string&  uri
                      , const string&  prefix ) :
   mName  ( name   )
 , mURI   ( uri    )
 , mPrefix( prefix )
{
}


/**
 * Creates a new XMLTriple by splitting triplet on sepchar.  Triplet
 * may be in one of the following formats:
 *
 *   name
 *   uri sepchar name
 *   uri sepchar name sepchar prefix
 */
XMLTriple::XMLTriple (const string& triplet, const char sepchar)
{ 
  string::size_type start = 0;
  string::size_type pos   = triplet.find(sepchar, start);


  if (pos != string::npos)
  {
    mURI = triplet.substr(start, pos);

    start = pos + 1;
    pos   = triplet.find(sepchar, start);

    if (pos != string::npos)
    {
      mName   = triplet.substr(start, pos - start);
      mPrefix = triplet.substr(pos + 1);
    }
    else
    {
      mName = triplet.substr(start);
    }
  }
  else
  {
    mName = triplet;
  }
}


/**
 * 
 **/
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_create (void)
{
  return new(nothrow) XMLTriple;
}


/**
 * 
 **/
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_createWith (const char *name, const char *uri, const char *prefix)
{
  return new(nothrow) XMLTriple(name, uri, prefix);
}


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLTriple_free (XMLTriple_t *triple)
{
  delete static_cast<XMLTriple*>( triple );
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLTriple_getName (const XMLTriple_t *triple)
{
  return triple->getName().empty() ? NULL : triple->getName().c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLTriple_getPrefix (const XMLTriple_t *triple)
{
  return triple->getPrefix().empty() ? NULL : triple->getPrefix().c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLTriple_getURI (const XMLTriple_t *triple)
{
  return triple->getURI().empty() ? NULL : triple->getURI().c_str();
}
