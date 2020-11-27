/**
 * @file ListOfIndices.cpp
 * @brief Implementation of the ListOfIndices class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/arrays/sbml/ListOfIndices.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfIndices using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 */
ListOfIndices::ListOfIndices(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfIndices using the given ArraysPkgNamespaces object.
 */
ListOfIndices::ListOfIndices(ArraysPkgNamespaces *arraysns)
  : ListOf(arraysns)
{
  setElementNamespace(arraysns->getURI());
}


/*
 * Copy constructor for ListOfIndices.
 */
ListOfIndices::ListOfIndices(const ListOfIndices& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfIndices.
 */
ListOfIndices&
ListOfIndices::operator=(const ListOfIndices& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfIndices object.
 */
ListOfIndices*
ListOfIndices::clone() const
{
  return new ListOfIndices(*this);
}


/*
 * Destructor for ListOfIndices.
 */
ListOfIndices::~ListOfIndices()
{
}


/*
 * Get an Index from the ListOfIndices.
 */
Index*
ListOfIndices::get(unsigned int n)
{
  return static_cast<Index*>(ListOf::get(n));
}


/*
 * Get an Index from the ListOfIndices.
 */
const Index*
ListOfIndices::get(unsigned int n) const
{
  return static_cast<const Index*>(ListOf::get(n));
}


/*
 * Get an Index from the ListOfIndices based on its identifier.
 */
Index*
ListOfIndices::get(const std::string& sid)
{
  return const_cast<Index*>(static_cast<const ListOfIndices&>(*this).get(sid));
}


/*
 * Get an Index from the ListOfIndices based on its identifier.
 */
const Index*
ListOfIndices::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Index>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Index*> (*result);
}


/*
 * Removes the nth Index from this ListOfIndices and returns a pointer to it.
 */
Index*
ListOfIndices::remove(unsigned int n)
{
  return static_cast<Index*>(ListOf::remove(n));
}


/*
 * Removes the Index from this ListOfIndices based on its identifier and
 * returns a pointer to it.
 */
Index*
ListOfIndices::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Index>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Index*> (item);
}


/*
 * Adds a copy of the given Index to this ListOfIndices.
 */
int
ListOfIndices::addIndex(const Index* i)
{
  if (i == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (i->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != i->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != i->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(i)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(i);
  }
}


/*
 * Get the number of Index objects in this ListOfIndices.
 */
unsigned int
ListOfIndices::getNumIndices() const
{
  return size();
}


/*
 * Creates a new Index object, adds it to this ListOfIndices object and returns
 * the Index object created.
 */
Index*
ListOfIndices::createIndex()
{
  Index* i = NULL;

  try
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    i = new Index(arraysns);
    delete arraysns;
  }
  catch (...)
  {
  }

  if (i != NULL)
  {
    appendAndOwn(i);
  }

  return i;
}


/*
* Get a Index from the ListOfIndices based on the ArrayDimension to which it
* refers.
*/
const Index*
ListOfIndices::getByArrayDimension(unsigned int arrayDimension) const
{
  vector<SBase*>::const_iterator result;
  for (result = mItems.begin(); result != mItems.end(); result++)
  {
    if (static_cast <const Index*>(*result)->getArrayDimension() == arrayDimension)
    {
      return static_cast <const Index*>(*result);
    }
  }

  return NULL;
}


/*
* Get a Index from the ListOfIndices based on the ArrayDimension to which it
* refers.
*/
Index*
ListOfIndices::getByArrayDimension(unsigned int arrayDimension)
{
  return const_cast<Index*>(static_cast<const
    ListOfIndices&>(*this).getByArrayDimension(arrayDimension));
}


/*
 * Returns the XML element name of this ListOfIndices object.
 */
const std::string&
ListOfIndices::getElementName() const
{
  static const string name = "listOfIndices";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfIndices object.
 */
int
ListOfIndices::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfIndices object.
 */
int
ListOfIndices::getItemTypeCode() const
{
  return SBML_ARRAYS_INDEX;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Index in this ListOfIndices
 */
SBase*
ListOfIndices::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());

  if (name == "index")
  {
    object = new Index(arraysns);
    appendAndOwn(object);
  }

  delete arraysns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Arrays package
 */
void
ListOfIndices::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(ArraysExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(ArraysExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get an Index_t from the ListOf_t.
 */
LIBSBML_EXTERN
const Index_t*
ListOfIndices_getIndex(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfIndices*>(lo)->get(n);
}


/*
 * Get an Index_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
const Index_t*
ListOfIndices_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfIndices*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Index_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Index_t*
ListOfIndices_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfIndices*>(lo)->remove(n);
}


/*
 * Removes the Index_t from this ListOf_t based on its identifier and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Index_t*
ListOfIndices_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfIndices*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


