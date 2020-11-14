/**
 * @file    ListOfDeletions.cpp
 * @brief   Implementation of ListOfDeletions.
 * @author  Lucian Smith
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/ListOfDeletions.h>
#include <sbml/Model.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

ListOfDeletions::ListOfDeletions(CompPkgNamespaces* compns)
  : ListOf(compns)
{
  // set the element namespace of this object
  setElementNamespace(compns->getURI());
  loadPlugins(compns);
}


ListOfDeletions::ListOfDeletions(unsigned int level, unsigned int version, unsigned int pkgVersion)
  : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new CompPkgNamespaces(level,version,pkgVersion));
  loadPlugins(mSBMLNamespaces);
};


ListOfDeletions*
ListOfDeletions::clone () const
{
  return new ListOfDeletions(*this);
}


/* return nth item in list */
Deletion *
ListOfDeletions::get(unsigned int n)
{
  return static_cast<Deletion*>(ListOf::get(n));
}


/* return nth item in list */
const Deletion *
ListOfDeletions::get(unsigned int n) const
{
  return static_cast<const Deletion*>(ListOf::get(n));
}


/* return item by symbol */
Deletion*
ListOfDeletions::get (const std::string& symbol)
{
  return const_cast<Deletion*>( 
    static_cast<const ListOfDeletions&>(*this).get(symbol) );
}


/* return item by symbol */
const Deletion*
ListOfDeletions::get (const std::string& symbol) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Model>(symbol) );
  return (result == mItems.end()) ? 0 : static_cast <Deletion*> (*result);
}

/* Removes the nth item from this list */
Deletion*
ListOfDeletions::remove (unsigned int n)
{
  return static_cast<Deletion*>(ListOf::remove(n));
}


Deletion*
ListOfDeletions::remove (const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Deletion>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast<Deletion*>(item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf 
 */
int
ListOfDeletions::getItemTypeCode () const
{
  return SBML_COMP_DELETION;
}


const std::string&
ListOfDeletions::getElementName () const
{
  static const std::string name = "listOfDeletions";
  return name;
}


/** @cond doxygenLibsbmlInternal */
SBase*
ListOfDeletions::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "deletion")
  {
    COMP_CREATE_NS(compns, getSBMLNamespaces());
    object = new Deletion(compns);
    appendAndOwn(object);
    delete compns;
  }

  return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
ListOfDeletions::writeXMLNS (XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(CompExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(CompExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}
/** @endcond */


#endif  /* __cplusplus */

/*
 * Get a Deletion_t from the ListOf_t.
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_getDeletion(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDeletions*>(lo)->get(n);
}


/*
 * Get a Deletion_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDeletions*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Deletion_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDeletions*>(lo)->remove(n);
}


/*
 * Removes the Deletion_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
Deletion_t*
ListOfDeletions_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDeletions*>(lo)->remove(sid) :
    NULL;
}


LIBSBML_CPP_NAMESPACE_END
