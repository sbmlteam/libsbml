/**
 * Filename    : XMLNamespaceList.cpp
 * Description : A list of XMLNamepaces
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-09-15
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <algorithm>
#include <functional>
#include <iterator>

#include "sbml/XMLNamespace.hpp"
#include "sbml/XMLNamespaceList.hpp"


using namespace std;


typedef list<XMLNamespace>::const_iterator ListIter;


/**
 * This will be returned by the methods below if no matching XMLNamespace
 * can be found.
 */
const XMLNamespace XMLNamespaceList::mEmptyNamespace("", "");




/**
 * Function Object: Compares two XMLNamespaces for prefix equality.
 */
struct URIEquals: public binary_function<XMLNamespace, XMLNamespace, bool>
{
  bool operator() (const XMLNamespace& ns1, const XMLNamespace& ns2) const
  {
    return ns1.getURI() == ns2.getURI();
  }
};


/**
 * Function Object: Compares two XMLNamespaces for URI equality.
 */
struct PrefixEquals: public binary_function<XMLNamespace, XMLNamespace, bool>
{
  bool operator() (const XMLNamespace& ns1, const XMLNamespace& ns2) const
  {
    return ns1.getPrefix() == ns2.getPrefix();
  }
};




/**
 * Adds XMLNamespace to this list of XML namespaces.
 */
LIBSBML_EXTERN
void
XMLNamespaceList::add (const XMLNamespace& ns)
{
  mNamespaces.push_back(ns);
}


/**
 * Adds (prefix, URI) to this list of XML namespaces.
 *
 * If prefix starts with 'xmlns:' (case-insensitive), it will be removed.
 */
LIBSBML_EXTERN
void
XMLNamespaceList::add (const string& prefix, const string& URI)
{
  add( XMLNamespace(prefix, URI) );
}


/**
 * @return the number of XML namespaces in this list.
 */
LIBSBML_EXTERN
unsigned int
XMLNamespaceList::getLength () const
{
  return mNamespaces.size();
}


/**
 * @return the nth XMLNamespace in this list.
 */
const XMLNamespace&
XMLNamespaceList::getNamespace (unsigned int n) const
{
  if (n >= getLength()) return mEmptyNamespace;


  ListIter iter = mNamespaces.begin();
  advance(iter, n);

  return *iter;
}


/**
 * @return the prefix of the nth XML namespace in this list.
 */
LIBSBML_EXTERN
const string&
XMLNamespaceList::getPrefix (unsigned int n) const
{
  return getNamespace(n).getPrefix();
}


/**
 * @return the prefix of the XML namespace with the given URI.  If URI is
 * not in this list of namespaces, an empty string is returned.
 */
LIBSBML_EXTERN
const string& 
XMLNamespaceList::getPrefix (const string& URI) const
{
  ListIter begin  = mNamespaces.begin();
  ListIter end    = mNamespaces.end();
  ListIter result =
    find_if( begin, end, bind2nd(URIEquals(), XMLNamespace("", URI)) );
             

  if (result == mNamespaces.end()) return mEmptyNamespace.getPrefix();
  else return result->getPrefix();
}


/**
 * @return the URI of the nth XML namespace in this list.
 */
LIBSBML_EXTERN
const string&
XMLNamespaceList::getURI (unsigned int n) const
{
  return getNamespace(n).getURI();
}


/**
 * @return the URI of the XML namespace with the given prefix.  If prefix
 * was not found, an empty string is returned.
 */
LIBSBML_EXTERN
const string&
XMLNamespaceList::getURI (const string& prefix) const
{
  ListIter begin  = mNamespaces.begin();
  ListIter end    = mNamespaces.end();
  ListIter result =
    find_if( begin, end, bind2nd(PrefixEquals(), XMLNamespace(prefix, "")) );


  if (result == mNamespaces.end()) return mEmptyNamespace.getPrefix();
  else return result->getURI();
}
