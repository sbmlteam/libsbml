/**
 * @file    XMLNamespaces.cpp
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLTriple.h>

#include <sbml/xml/XMLNamespaces.h>


/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new empty list of XML namespace declarations.
 */
XMLNamespaces::XMLNamespaces ()
{
}


/**
 * Destroys this list of XML namespace declarations.
 */
XMLNamespaces::~XMLNamespaces ()
{
}


/**
 * Copy constructor; creates a copy of this XMLNamespaces set.
 */
XMLNamespaces::XMLNamespaces(const XMLNamespaces& orig)
{
  this->mNamespaces.assign( orig.mNamespaces.begin(), orig.mNamespaces.end() ); 
}


/**
 * Assignment operator for XMLNamespaces.
 */
XMLNamespaces& 
XMLNamespaces::operator=(const XMLNamespaces& orig)
{
  this->mNamespaces.assign( orig.mNamespaces.begin(), orig.mNamespaces.end() ); 
  return *this;
}

/**
 * Creates and returns a deep copy of this XMLNamespaces set.
 * 
 * @return a (deep) copy of this XMLNamespaces set.
 */
XMLNamespaces* 
XMLNamespaces::clone () const
{
  return new XMLNamespaces(*this);
}


/**
 * Appends an XML namespace prefix/URI pair to this list of namespace
 * declarations.
 */
void
XMLNamespaces::add (const std::string& uri, const std::string& prefix)
{
  if ( prefix.empty() ) removeDefault();
  mNamespaces.push_back( make_pair(prefix, uri) );
}


/**
 * Clears (deletes) all XML namespace declarations.
 */
void
XMLNamespaces::clear ()
{
  mNamespaces.clear();
}


/**
 * Lookup the index of an XML namespace declaration by URI.
 *
 * @return the index of the given declaration, or -1 if not present.
 */
int
XMLNamespaces::getIndex (const std::string uri) const
{
  for (int index = 0; index < getLength(); ++index)
  {
    if (getURI(index) == uri) return index;
  }
  
  return -1;
}


/**
 * @return the number of attributes in this list.
 */
int
XMLNamespaces::getLength () const
{
  return mNamespaces.size();
}


/**
 * @return the prefix of an XML namespace declaration in this list (by
 * position).  If index is out of range, an empty string will be
 * returned.
 */
string
XMLNamespaces::getPrefix (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNamespaces[index].first;
}


/**
 * @return the prefix of an XML namespace declaration given its URI.  If
 * URI does not exist, an empty string will be returned.
 */
string
XMLNamespaces::getPrefix (const std::string& uri) const
{
  return getPrefix( getIndex(uri) );
}


/**
 * @return the URI of an XML namespace declaration in this list (by
 * position).  If index is out of range, an empty string will be
 * returned.
 */
string
XMLNamespaces::getURI (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNamespaces[index].second;
}


/**
 * @return the URI of an XML namespace declaration given its prefix.  If
 * no prefix is given and a default namespace exists it will be returned.
 * If prefix does not exist, an empty string will be returned.
 */
string
XMLNamespaces::getURI (const std::string& prefix) const
{
  for (int index = 0; index < getLength(); ++index)
  {
    if (getPrefix(index) == prefix) return getURI(index);
  }
  
  return "";
}


/**
 * @return true if this XMLNamespaces set is empty, false otherwise.
 */
bool
XMLNamespaces::isEmpty () const
{
  return (getLength() == 0);
}


/** @cond doxygen-libsbml-internal */
/**
 * Removes the default XML namespace.
 */
void
XMLNamespaces::removeDefault ()
{
  vector<PrefixURIPair>::iterator i;

  for (i = mNamespaces.begin(); i != mNamespaces.end(); ++i)
  {
    if (i->first.empty())
    {
      mNamespaces.erase(i);
      break;
    }
  }
}
/** @endcond doxygen-libsbml-internal */

#ifndef SWIG

/**
 * Writes the XML namespace declarations to stream.
 */
void
XMLNamespaces::write (XMLOutputStream& stream) const
{
  for (int n = 0; n < getLength(); ++n)
  {
    if ( getPrefix(n).empty() )
    {
      stream.writeAttribute( "xmlns", getURI(n) );
    }
    else
    {
      const XMLTriple triple(getPrefix(n), "", "xmlns");
      stream.writeAttribute( triple, getURI(n) );
    }
  }
}


/**
 * Inserts this XML namespace declarations into stream.
 */
LIBLAX_EXTERN
XMLOutputStream&
operator<< (XMLOutputStream& stream, const XMLNamespaces& namespaces)
{
  namespaces.write(stream);
  return stream;
}

#endif  /* !SWIG */


/** @cond doxygen-c-only */

/**
 * Creates a new empty XMLNamespaces_t structure.
 */
LIBLAX_EXTERN
XMLNamespaces_t *
XMLNamespaces_create (void)
{
  return new(nothrow) XMLNamespaces;
}


/**
 * Frees the given XMLNamespaces_t structure.
 *
 * @param ns XMLNamespaces structure to be freed.
 **/
LIBLAX_EXTERN
void
XMLNamespaces_free (XMLNamespaces_t *ns)
{
  delete ns;
}


/**
 * Creates a deep copy of the given XMLNamespaces_t structure
 * 
 * @param ns the XMLNamespaces_t structure to be copied
 * 
 * @return a (deep) copy of the given XMLNamespaces_t structure.
 */
LIBLAX_EXTERN
XMLNamespaces_t *
XMLNamespaces_clone (const XMLNamespaces_t* ns)
{
  return static_cast<XMLNamespaces*>( ns->clone() );
}


/**
 * Appends an XML namespace prefix/URI pair to this XMLNamespaces_t 
 * structure.
 *
 * @param ns XMLNamespaces structure.
 * @param uri a string, the uri for the namespace.
 * @param prefix a string, the prefix for the namespace.
 */
LIBLAX_EXTERN
void
XMLNamespaces_add (XMLNamespaces_t *ns, 
		   const char *uri, const char *prefix)
{
  ns->add(uri, prefix);
}


/**
 * Clears this XMLNamespaces_t structure.
 *
 * @param ns XMLNamespaces structure.
 **/
LIBLAX_EXTERN
void
XMLNamespaces_clear (XMLNamespaces_t *ns)
{
  ns->clear();
}


/**
 * Lookup the index of an XML namespace declaration by URI.
 *
 * @param uri a string, uri of the required namespace.
 *
 * @return the index of the given declaration, or -1 if not present.
 */
LIBLAX_EXTERN
int
XMLNamespaces_getIndex (const XMLNamespaces_t *ns, const char *uri)
{
  return ns->getIndex(uri);
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLNamespaces_getLength (const XMLNamespaces_t *ns)
{
  return ns->getLength();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefix (const XMLNamespaces_t *ns, int index)
{
  return ns->getPrefix(index).empty() ? NULL : ns->getPrefix(index).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefixByURI (const XMLNamespaces_t *ns, const char *uri)
{
  return ns->getPrefix(uri).empty() ? NULL : ns->getPrefix(uri).c_str();
}

/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getURI (const XMLNamespaces_t *ns, int index)
{
  return ns->getURI(index).empty() ? NULL : ns->getURI(index).c_str();
}

/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getURIByPrefix (const XMLNamespaces_t *ns, const char *prefix)
{
  return ns->getURI(prefix).empty() ? NULL : ns->getURI(prefix).c_str();
}

/**
 * 
 **/
LIBLAX_EXTERN
int
XMLNamespaces_isEmpty (const XMLNamespaces_t *ns)
{
  return ns->isEmpty();
}

/** @endcond doxygen-c-only */
