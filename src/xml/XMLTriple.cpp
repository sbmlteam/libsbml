/**
 * @file    XMLTriple.cpp
 * @brief   Stores an XML namespace triple.
 * @author  Ben Bornstein
 *
 * $Id$
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

#include <sbml/xml/XMLTriple.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new empty XMLTriple.
 */
XMLTriple::XMLTriple ()
{
}


/*
 * Creates a new XMLTriple.
 */
XMLTriple::XMLTriple (  const std::string&  name
                      , const std::string&  uri
                      , const std::string&  prefix ) :
   mName  ( name   )
 , mURI   ( uri    )
 , mPrefix( prefix )
{
}


/*
 * Creates a new XMLTriple by splitting triplet on sepchar.  Triplet
 * may be in one of the following formats:
 *
 *   name
 *   uri sepchar name
 *   uri sepchar name sepchar prefix
 */
XMLTriple::XMLTriple (const std::string& triplet, const char sepchar)
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


/*
 * Copy constructor; creates a copy of this XMLTriple set.
 */
XMLTriple::XMLTriple(const XMLTriple& orig)
{
  mName   = orig.mName;
  mURI    = orig.mURI;
  mPrefix = orig.mPrefix;
}

/*
 * Assignment operator for XMLTriple.
 */
XMLTriple& 
XMLTriple::operator=(const XMLTriple& orig)
{
  mName   = orig.mName;
  mURI    = orig.mURI;
  mPrefix = orig.mPrefix;

  return *this;
}

/*
 * Creates and returns a deep copy of this XMLTriple set.
 * 
 * @return a (deep) copy of this XMLTriple set.
 */
XMLTriple* 
XMLTriple::clone () const
{
  return new XMLTriple(*this);
}

/*
 * @return true if this XMLTriple set is empty, false otherwise.
 */
bool
XMLTriple::isEmpty () const
{
  return ( getName().size() == 0
        && getURI().size() == 0
        && getPrefix().size() == 0);
}


/** @cond doxygen-c-only */


/**
 * Creates a new empty XMLTriple_t structure and returns a pointer to it.
 *
 * @return pointer to created XMLTriple_t structure.
 */
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_create (void)
{
  return new(nothrow) XMLTriple;
}


/**
 * Creates a new XMLTriple_t structure with name, prefix and uri.
 *
 * @param name a string, name for the XMLTriple_t structure.
 * @param uri a string, URI of the XMLTriple_t structure.
 * @param prefix a string, prefix for the URI of the XMLTriple_t structure.
 *
 * @return pointer to the created XMLTriple_t structure.
   */
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_createWith (const char *name, const char *uri, const char *prefix)
{
  return new(nothrow) XMLTriple(name, uri, prefix);
}


/**
 * Destroys this XMLTriple_t structure.
 *
 * @param triple XMLTriple_t structure to be freed.
 **/
LIBLAX_EXTERN
void
XMLTriple_free (XMLTriple_t *triple)
{
  delete static_cast<XMLTriple*>( triple );
}


/**
 * Creates a deep copy of the given XMLTriple_t structure
 * 
 * @param t the XMLTriple_t structure to be copied
 * 
 * @return a (deep) copy of the given XMLTriple_t structure.
 */
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_clone (const XMLTriple_t* t)
{
  return static_cast<XMLTriple*>( t->clone() );
}


/**
 * Returns the name from this XMLTriple_t structure.
 *
 * @param triple XMLTriple_t structure to be queried.
 *
 * @return name from this XMLTriple_t structure.
 */
LIBLAX_EXTERN
const char *
XMLTriple_getName (const XMLTriple_t *triple)
{
  return triple->getName().empty() ? NULL : triple->getName().c_str();
}


/**
 * Returns the prefix from this XMLTriple_t structure.
 *
 * @param triple XMLTriple_t structure to be queried.
 *
 * @return prefix from this XMLTriple_t structure.
 */
LIBLAX_EXTERN
const char *
XMLTriple_getPrefix (const XMLTriple_t *triple)
{
  return triple->getPrefix().empty() ? NULL : triple->getPrefix().c_str();
}


/**
 * Returns the URI from this XMLTriple_t structure.
 *
 * @param triple XMLTriple_t structure to be queried.
 *
 * @return URI from this XMLTriple_t structure.
 */
LIBLAX_EXTERN
const char *
XMLTriple_getURI (const XMLTriple_t *triple)
{
  return triple->getURI().empty() ? NULL : triple->getURI().c_str();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLTriple is empty.
 * 
 * @param triple XMLTriple_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLTriple is empty, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLTriple_isEmpty (const XMLTriple_t *triple)
{
  return static_cast<int> (triple->isEmpty());
}


/** @endcond doxygen-c-only */
