/**
 * \file    XMLNamespaces.h
 * \brief   A list of XMLNamespace declarations (URI/prefix pairs)
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


#ifndef XMLNamespaces_h
#define XMLNamespaces_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <string>
#include <vector>

class XMLOutputStream;


class LIBLAX_EXTERN XMLNamespaces
{
public:

  /**
   * Creates a new empty list of XML namespace declarations.
   */
  XMLNamespaces ();

  /**
   * Destroys this list of XML namespace declarations.
   */
  virtual ~XMLNamespaces ();


  /**
   * Appends an XML namespace prefix/URI pair to this list of namespace
   * declarations.
   */
  void add (const std::string& uri, const std::string& prefix = "");

  /**
   * Clears (deletes) all XML namespace declarations.
   */
  void clear ();

  /**
   * Lookup the index of an XML namespace declaration by URI.
   *
   * @return the index of the given declaration, or -1 if not present.
   */
  int getIndex (const std::string uri) const;

  /**
   * @return the number of attributes in this list.
   */
  int getLength () const;

  /**
   * @return the prefix of an XML namespace declaration in this list (by
   * position).  If index is out of range, an empty string will be
   * returned.
   */
  std::string getPrefix (int index) const;

  /**
   * @return the prefix of an XML namespace declaration given its URI.  If
   * URI does not exist, an empty string will be returned.
   */
  std::string getPrefix (const std::string& uri) const;

  /**
   * @return the URI of an XML namespace declaration in this list (by
   * position).  If index is out of range, an empty string will be
   * returned.
   */
  std::string getURI (int index) const;

  /**
   * @return the URI of an XML namespace declaration given its prefix.  If
   * no prefix is given and a default namespace exists it will be returned.
   * If prefix does not exist, an empty string will be returned.
   */
  std::string getURI (const std::string& prefix = "") const;

  /**
   * @return true if this XMLNamespaces set is empty, false otherwise.
   */
  bool isEmpty () const;


#ifndef SWIG

  /**
   * Writes the XML namespace declarations to stream.
   */
  void write (XMLOutputStream& stream) const;

  /**
   * Inserts this XML namespace declarations into stream.
   */
  LIBLAX_EXTERN
  friend XMLOutputStream&
  operator<< (XMLOutputStream& stream, const XMLNamespaces& namespaces);

#endif  /* !SWIG */


protected:

  /**
   * Removes the default XML namespace.
   */
  void removeDefault ();


  typedef std::pair<std::string, std::string> PrefixURIPair;
  std::vector<PrefixURIPair> mNamespaces;
};


#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS


/**
 * Creates a new XMLNamespaces structure and returns a pointer to it.
 **/
LIBLAX_EXTERN
XMLNamespaces_t *
XMLNamespaces_create (void);

/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNamespaces_free (XMLNamespaces_t *ns);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNamespaces_add (XMLNamespaces_t *ns, 
		   const char *uri, const char *prefix);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNamespaces_clear (XMLNamespaces_t *ns);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLNamespaces_getIndex (const XMLNamespaces_t *ns, const char *uri);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLNamespaces_getLength (const XMLNamespaces_t *ns);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefix (const XMLNamespaces_t *ns, int index);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefixByURI (const XMLNamespaces_t *ns, const char *uri);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getURI (const XMLNamespaces_t *ns, int index);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getURIByPrefix (const XMLNamespaces_t *ns, const char *prefix);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLNamespaces_isEmpty (const XMLNamespaces_t *ns);



END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLNamespaces_h */
