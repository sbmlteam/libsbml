/**
 * @file ListOfDimensions.cpp
 * @brief Implementation of the ListOfDimensions class.
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
#include <sbml/packages/arrays/sbml/ListOfDimensions.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDimensions using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 */
ListOfDimensions::ListOfDimensions(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new ArraysPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfDimensions using the given ArraysPkgNamespaces object.
 */
ListOfDimensions::ListOfDimensions(ArraysPkgNamespaces *arraysns)
  : ListOf(arraysns)
{
  setElementNamespace(arraysns->getURI());
}


/*
 * Copy constructor for ListOfDimensions.
 */
ListOfDimensions::ListOfDimensions(const ListOfDimensions& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDimensions.
 */
ListOfDimensions&
ListOfDimensions::operator=(const ListOfDimensions& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDimensions object.
 */
ListOfDimensions*
ListOfDimensions::clone() const
{
  return new ListOfDimensions(*this);
}


/*
 * Destructor for ListOfDimensions.
 */
ListOfDimensions::~ListOfDimensions()
{
}


/*
 * Get a Dimension from the ListOfDimensions.
 */
Dimension*
ListOfDimensions::get(unsigned int n)
{
  return static_cast<Dimension*>(ListOf::get(n));
}


/*
 * Get a Dimension from the ListOfDimensions.
 */
const Dimension*
ListOfDimensions::get(unsigned int n) const
{
  return static_cast<const Dimension*>(ListOf::get(n));
}


/*
 * Get a Dimension from the ListOfDimensions based on its identifier.
 */
Dimension*
ListOfDimensions::get(const std::string& sid)
{
  return const_cast<Dimension*>(static_cast<const
    ListOfDimensions&>(*this).get(sid));
}


/*
 * Get a Dimension from the ListOfDimensions based on its identifier.
 */
const Dimension*
ListOfDimensions::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Dimension>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Dimension*>
    (*result);
}


/*
 * Removes the nth Dimension from this ListOfDimensions and returns a pointer
 * to it.
 */
Dimension*
ListOfDimensions::remove(unsigned int n)
{
  return static_cast<Dimension*>(ListOf::remove(n));
}


/*
 * Removes the Dimension from this ListOfDimensions based on its identifier and
 * returns a pointer to it.
 */
Dimension*
ListOfDimensions::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Dimension>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Dimension*> (item);
}


/*
 * Adds a copy of the given Dimension to this ListOfDimensions.
 */
int
ListOfDimensions::addDimension(const Dimension* d)
{
  if (d == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (d->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != d->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != d->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(d)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(d);
  }
}


/*
 * Get the number of Dimension objects in this ListOfDimensions.
 */
unsigned int
ListOfDimensions::getNumDimensions() const
{
  return size();
}


/*
 * Creates a new Dimension object, adds it to this ListOfDimensions object and
 * returns the Dimension object created.
 */
Dimension*
ListOfDimensions::createDimension()
{
  Dimension* d = NULL;

  try
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    d = new Dimension(arraysns);
    delete arraysns;
  }
  catch (...)
  {
  }

  if (d != NULL)
  {
    appendAndOwn(d);
  }

  return d;
}


/*
 * Used by ListOfDimensions::get() to lookup a Dimension based on its Size.
 */
struct IdEqSDimension
{
  const string& id;
   
  IdEqSDimension (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<Dimension*>(sb)->getSize() == id);
  }
};


/*
 * Get a Dimension from the ListOfDimensions based on the Size to which it
 * refers.
 */
const Dimension*
ListOfDimensions::getBySize(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqSDimension(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Dimension*>
    (*result);
}


/*
 * Get a Dimension from the ListOfDimensions based on the Size to which it
 * refers.
 */
Dimension*
ListOfDimensions::getBySize(const std::string& sid)
{
  return const_cast<Dimension*>(static_cast<const
    ListOfDimensions&>(*this).getBySize(sid));
}


/*
* Get a Dimension from the ListOfDimensions based on the ArrayDimension to which it
* refers.
*/
const Dimension*
ListOfDimensions::getByArrayDimension(unsigned int arrayDimension) const
{
  vector<SBase*>::const_iterator result;
  for (result = mItems.begin(); result != mItems.end(); result++)
  {
    if (static_cast <const Dimension*>(*result)->getArrayDimension() == arrayDimension)
    {
      return static_cast <const Dimension*>(*result);
    }
  }

  return NULL;
}


/*
* Get a Dimension from the ListOfDimensions based on the ArrayDimension to which it
* refers.
*/
Dimension*
ListOfDimensions::getByArrayDimension(unsigned int arrayDimension)
{
  return const_cast<Dimension*>(static_cast<const
    ListOfDimensions&>(*this).getByArrayDimension(arrayDimension));
}


/*
 * Returns the XML element name of this ListOfDimensions object.
 */
const std::string&
ListOfDimensions::getElementName() const
{
  static const string name = "listOfDimensions";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDimensions object.
 */
int
ListOfDimensions::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDimensions object.
 */
int
ListOfDimensions::getItemTypeCode() const
{
  return SBML_ARRAYS_DIMENSION;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Dimension in this ListOfDimensions
 */
SBase*
ListOfDimensions::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());

  if (name == "dimension")
  {
    object = new Dimension(arraysns);
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
ListOfDimensions::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a Dimension_t from the ListOf_t.
 */
LIBSBML_EXTERN
const Dimension_t*
ListOfDimensions_getDimension(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDimensions*>(lo)->get(n);
}


/*
 * Get a Dimension_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
const Dimension_t*
ListOfDimensions_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDimensions*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Dimension_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Dimension_t*
ListOfDimensions_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDimensions*>(lo)->remove(n);
}


/*
 * Removes the Dimension_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
Dimension_t*
ListOfDimensions_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDimensions*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


