/**
 * \file    XMLNamespaces.cpp
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


#include "XMLOutputStream.h"
#include "XMLTriple.h"

#include "XMLNamespaces.h"


using namespace std;


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
 * Appends an XML namespace prefix/URI pair to this list of namespace
 * declarations.
 */
void
XMLNamespaces::add (const string& uri, const string& prefix)
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
XMLNamespaces::getIndex (const string uri) const
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
XMLNamespaces::getPrefix (const string& uri) const
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
XMLNamespaces::getURI (const string& prefix) const
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


/**
 * 
 **/
LIBLAX_EXTERN
XMLNamespaces_t *
XMLNamespaces_create (void)
{
  return new(nothrow) XMLNamespaces;
}


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNamespaces_free (XMLNamespaces_t *ns)
{
  delete ns;
}


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNamespaces_add (XMLNamespaces_t *ns, 
		   const char *uri, const char *prefix)
{
  ns->add(uri, prefix);
}


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNamespaces_clear (XMLNamespaces_t *ns)
{
  ns->clear();
}


/**
 * 
 **/
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
  if (ns->getPrefix(index).empty())
    return NULL;
  else
    return ns->getPrefix(index).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getPrefixByURI (const XMLNamespaces_t *ns, const char *uri)
{
  if (ns->getPrefix(uri).empty())
    return NULL;
  else
    return ns->getPrefix(uri).c_str();
}

/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getURI (const XMLNamespaces_t *ns, int index)
{
  if (ns->getURI(index).empty())
    return NULL;
  else
    return ns->getURI(index).c_str();
}

/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLNamespaces_getURIByPrefix (const XMLNamespaces_t *ns, const char *prefix)
{
  if (ns->getURI(prefix).empty())
    return NULL;
  else
    return ns->getURI(prefix).c_str();
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
