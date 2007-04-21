/**
 * @file    XMLTriple.h
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 * @class XMLTriple.
 * @brief Implementation of %XMLTriple construct.
 */


#ifndef XMLTriple_h
#define XMLTriple_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <string>


class LIBLAX_EXTERN XMLTriple
{
public:

  /**
   * Creates a new empty XMLTriple.
   */
  XMLTriple ();

  /**
   * Creates a new XMLTriple with name, prefix and uri.
   *
   * @param name a string, name for the XMLTriple.
   * @param uri a string, URI of the XMLTriple.
   * @param prefix a string, prefix for the URI of the XMLTriple,
   */
  XMLTriple (  const std::string&  name
             , const std::string&  uri
             , const std::string&  prefix );

  /**
   * Creates a new XMLTriple by splitting triplet on sepchar.  
   *
   * Triplet may be in one of the following formats:
   *
   * @li name
   * @li uri sepchar name
   * @li uri sepchar name sepchar prefix
   *
   * @param triplet a string representing the triplet as above
   * @param sepchar a character, the sepchar used in the triplet
   */
  XMLTriple (const std::string& triplet, const char sepchar = ' ');
  
  /**
   * Copy constructor; creates a copy of this XMLTriple set.
   */
  XMLTriple(const XMLTriple& orig);


  /**
   * Assignment operator for XMLTriple.
   */
  XMLTriple& operator=(const XMLTriple& orig);

  /**
   * Creates and returns a deep copy of this XMLTriple set.
   * 
   * @return a (deep) copy of this XMLTriple set.
   */
  XMLTriple* clone () const;


  /**
   * Returns the name from this XMLTriple.
   *
   * @return name from this XMLTriple.
   */
  const std::string& getName   () const { return mName;   }

  /**
   * Returns the prefix from this XMLTriple.
   *
   * @return prefix from this XMLTriple.
   */
  const std::string& getPrefix () const { return mPrefix; }

  /**
   * Returns the URI from this XMLTriple.
   *
   * @return URI from this XMLTriple.
   */
  const std::string& getURI    () const { return mURI;    }

  /**
   * Predicate returning @c true or @c false depending on whether 
   * this XMLTriple is empty.
   * 
   * @return @c true if this XMLTriple is empty, @c false otherwise.
   */
  bool isEmpty () const;

private:

  std::string  mName;
  std::string  mURI;
  std::string  mPrefix;

};

#endif  /* __cplusplus */

#ifndef SWIG

BEGIN_C_DECLS


/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/

/**
 * 
 **/
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_create (void);


/**
 * 
 **/
LIBLAX_EXTERN
XMLTriple_t *
XMLTriple_createWith (const char *name, const char *uri, const char *prefix);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLTriple_free (XMLTriple_t *triple);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLTriple_getName (const XMLTriple_t *triple);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLTriple_getPrefix (const XMLTriple_t *triple);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLTriple_getURI (const XMLTriple_t *triple);



END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLTriple_h */
