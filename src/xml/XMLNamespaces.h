/**
 * @file    XMLNamespaces.h
 * @brief   A list of XMLNamespace declarations (URI/prefix pairs)
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
 * @class XMLNamespaces.
 * @brief Implementation of %XMLNamespaces construct.
 */


#ifndef XMLNamespaces_h
#define XMLNamespaces_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <string>
#include <vector>

/** @cond doxygen-libsbml-internal */
class XMLOutputStream;
/** @endcond doxygen-libsbml-internal */


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
   * Copy constructor; creates a copy of this XMLNamespaces set.
   */
  XMLNamespaces(const XMLNamespaces& orig);


  /**
   * Assignment operator for XMLNamespaces.
   */
  XMLNamespaces& operator=(const XMLNamespaces& orig);


  /**
   * Creates and returns a deep copy of this XMLNamespaces set.
   * 
   * @return a (deep) copy of this XMLNamespaces set.
   */
  XMLNamespaces* clone () const;


  /**
   * Appends an XML namespace prefix/URI pair to this list of namespace
   * declarations.
   *
   * @param uri a string, the uri for the namespace
   * @param prefix a string, the prefix for the namespace
   */
  void add (const std::string& uri, const std::string& prefix = "");


  /**
   * Clears (deletes) all XML namespace declarations.
   */
  void clear ();


  /**
   * Lookup the index of an XML namespace declaration by URI.
   *
   * @param uri a string, uri of the required namespace.
   *
   * @return the index of the given declaration, or -1 if not present.
   */
  int getIndex (const std::string uri) const;


  /**
   * Returns the number of namespaces in the list.
   *
   * @return the number of namespaces in this list.
   */
  int getLength () const;


  /**
   * Lookup the prefix of an XML namespace declaration by position.
   *
   * @param index an integer, position of the required prefix.
   *
   * @return the prefix of an XML namespace declaration in this list (by
   * position).  
   *
   * @note If index is out of range, an empty string will be
   * returned.
   */
  std::string getPrefix (int index) const;


  /**
   * Lookup the prefix of an XML namespace declaration by URI.
   *
   * @param uri a string, uri of the required prefix
   *
   * @return the prefix of an XML namespace declaration given its URI.  
   *
   * @note If
   * URI does not exist, an empty string will be returned.
   */
  std::string getPrefix (const std::string& uri) const;


  /**
   * Lookup the URI of an XML namespace declaration by position.
   *
   * @param index an integer, position of the required URI.
   *
   * @return the URI of an XML namespace declaration in this list (by
   * position).  
   *
   * @note If index is out of range, an empty string will be
   * returned.
   */
  std::string getURI (int index) const;


  /**
   * Lookup the URI of an XML namespace declaration by prefix.
   *
   * @param prefix a string, prefix of the required URI
   *
   * @return the URI of an XML namespace declaration given its prefix.  
   *
   * @note If
   * prefix does not exist, an empty string will be returned.
   */
  std::string getURI (const std::string& prefix = "") const;


  /**
   * Predicate returning @c true or @c false depending on whether 
   * this XMLNamespaces set is empty.
   * 
   * @return @c true if this XMLNamespaces set is empty, @c false otherwise.
   */
  bool isEmpty () const;


#ifndef SWIG

  /** @cond doxygen-libsbml-internal */

  /**
   * Writes this XMLNamespaces set to stream.
   *
   * @param stream XMLOutputStream, stream to which this XMLNamespaces
   * set is to be written.
   */
  void write (XMLOutputStream& stream) const;


  /**
   * Inserts this XMLNamespaces set into stream.
   *
   * @param stream XMLOutputStream, stream to which the XMLNamespaces
   * set is to be written.
   * @param namespaces XMLNamespaces, namespaces to be written to stream.
   *
   * @return the stream with the namespaces inserted.
   */
  LIBLAX_EXTERN
  friend XMLOutputStream&
  operator<< (XMLOutputStream& stream, const XMLNamespaces& namespaces);

  /** @endcond doxygen-libsbml-internal */

#endif  /* !SWIG */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Removes the default XML namespace.
   */
  void removeDefault ();


  typedef std::pair<std::string, std::string> PrefixURIPair;
  std::vector<PrefixURIPair> mNamespaces;

  /** @endcond doxygen-libsbml-internal */
};


#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBLAX_EXTERN
XMLNamespaces_t *
XMLNamespaces_create (void);


LIBLAX_EXTERN
void
XMLNamespaces_free (XMLNamespaces_t *ns);


LIBLAX_EXTERN
XMLNamespaces_t *
XMLNamespaces_clone (const XMLNamespaces_t* c);


LIBLAX_EXTERN
void
XMLNamespaces_add (XMLNamespaces_t *ns, 
		   const char *uri, const char *prefix);


LIBLAX_EXTERN
void
XMLNamespaces_clear (XMLNamespaces_t *ns);


LIBLAX_EXTERN
int
XMLNamespaces_getIndex (const XMLNamespaces_t *ns, const char *uri);


LIBLAX_EXTERN
int
XMLNamespaces_getLength (const XMLNamespaces_t *ns);


LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefix (const XMLNamespaces_t *ns, int index);


LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefixByURI (const XMLNamespaces_t *ns, const char *uri);


LIBLAX_EXTERN
const char *
XMLNamespaces_getURI (const XMLNamespaces_t *ns, int index);


LIBLAX_EXTERN
const char *
XMLNamespaces_getURIByPrefix (const XMLNamespaces_t *ns, const char *prefix);


LIBLAX_EXTERN
int
XMLNamespaces_isEmpty (const XMLNamespaces_t *ns);



END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLNamespaces_h */
