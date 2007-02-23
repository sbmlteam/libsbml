/**
 * \file    XMLTriple.h
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
   * Creates a new XMLTriple.
   */
  XMLTriple (  const std::string&  name
             , const std::string&  uri
             , const std::string&  prefix );

  /**
   * Creates a new XMLTriple by splitting triplet on sepchar.  Triplet
   * may be in one of the following formats:
   *
   *   name
   *   uri sepchar name
   *   uri sepchar name sepchar prefix
   */
  XMLTriple (const std::string& triplet, const char sepchar = ' ');

  const std::string& getName   () const { return mName;   }
  const std::string& getPrefix () const { return mPrefix; }
  const std::string& getURI    () const { return mURI;    }


private:

  std::string  mName;
  std::string  mURI;
  std::string  mPrefix;

};

#endif  /* __cplusplus */

#ifndef SWIG

BEGIN_C_DECLS


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
