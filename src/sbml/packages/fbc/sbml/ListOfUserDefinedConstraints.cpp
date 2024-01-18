/**
 * @file ListOfUserDefinedConstraints.cpp
 * @brief Implementation of the ListOfUserDefinedConstraints class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraints.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfUserDefinedConstraints using the given SBML Level,
 * Version and &ldquo;fbc&rdquo; package version.
 */
ListOfUserDefinedConstraints::ListOfUserDefinedConstraints(unsigned int level,
                                                           unsigned int
                                                             version,
                                                           unsigned int
                                                             pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfUserDefinedConstraints using the given FbcPkgNamespaces
 * object.
 */
ListOfUserDefinedConstraints::ListOfUserDefinedConstraints(FbcPkgNamespaces
  *fbcns)
  : ListOf(fbcns)
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Copy constructor for ListOfUserDefinedConstraints.
 */
ListOfUserDefinedConstraints::ListOfUserDefinedConstraints(const
  ListOfUserDefinedConstraints& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfUserDefinedConstraints.
 */
ListOfUserDefinedConstraints&
ListOfUserDefinedConstraints::operator=(const ListOfUserDefinedConstraints&
  rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfUserDefinedConstraints object.
 */
ListOfUserDefinedConstraints*
ListOfUserDefinedConstraints::clone() const
{
  return new ListOfUserDefinedConstraints(*this);
}


/*
 * Destructor for ListOfUserDefinedConstraints.
 */
ListOfUserDefinedConstraints::~ListOfUserDefinedConstraints()
{
}


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::get(unsigned int n)
{
  return static_cast<UserDefinedConstraint*>(ListOf::get(n));
}


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints.
 */
const UserDefinedConstraint*
ListOfUserDefinedConstraints::get(unsigned int n) const
{
  return static_cast<const UserDefinedConstraint*>(ListOf::get(n));
}


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based on
 * its identifier.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::get(const std::string& sid)
{
  return const_cast<UserDefinedConstraint*>(static_cast<const
    ListOfUserDefinedConstraints&>(*this).get(sid));
}


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based on
 * its identifier.
 */
const UserDefinedConstraint*
ListOfUserDefinedConstraints::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<UserDefinedConstraint>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    UserDefinedConstraint*> (*result);
}


/*
 * Removes the nth UserDefinedConstraint from this ListOfUserDefinedConstraints
 * and returns a pointer to it.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::remove(unsigned int n)
{
  return static_cast<UserDefinedConstraint*>(ListOf::remove(n));
}


/*
 * Removes the UserDefinedConstraint from this ListOfUserDefinedConstraints
 * based on its identifier and returns a pointer to it.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<UserDefinedConstraint>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <UserDefinedConstraint*> (item);
}


/*
 * Adds a copy of the given UserDefinedConstraint to this
 * ListOfUserDefinedConstraints.
 */
int
ListOfUserDefinedConstraints::addUserDefinedConstraint(const
  UserDefinedConstraint* udc)
{
  if (udc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (udc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != udc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(udc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(udc);
  }
}


/*
 * Get the number of UserDefinedConstraint objects in this
 * ListOfUserDefinedConstraints.
 */
unsigned int
ListOfUserDefinedConstraints::getNumUserDefinedConstraints() const
{
  return size();
}


/*
 * Creates a new UserDefinedConstraint object, adds it to this
 * ListOfUserDefinedConstraints object and returns the UserDefinedConstraint
 * object created.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::createUserDefinedConstraint()
{
  UserDefinedConstraint* udc = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(),
      getPackageVersion());
    udc = new UserDefinedConstraint(fbcns);
    delete fbcns;
  }
  catch (...)
  {
  }

  if (udc != NULL)
  {
    appendAndOwn(udc);
  }

  return udc;
}


/*
 * Used by ListOfUserDefinedConstraints::get() to lookup an
 * UserDefinedConstraint based on its LowerBound.
 */
struct IdEqLB
{
  const string& id;
   
  IdEqLB (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<UserDefinedConstraint*>(sb)->getLowerBound() == id);
  }
};


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based on
 * the LowerBound to which it refers.
 */
const UserDefinedConstraint*
ListOfUserDefinedConstraints::getByLowerBound(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqLB(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    UserDefinedConstraint*> (*result);
}


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based on
 * the LowerBound to which it refers.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::getByLowerBound(const std::string& sid)
{
  return const_cast<UserDefinedConstraint*>(static_cast<const
    ListOfUserDefinedConstraints&>(*this).getByLowerBound(sid));
}


/*
 * Used by ListOfUserDefinedConstraints::get() to lookup an
 * UserDefinedConstraint based on its UpperBound.
 */
struct IdEqUB
{
  const string& id;
   
  IdEqUB (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<UserDefinedConstraint*>(sb)->getUpperBound() == id);
  }
};


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based on
 * the UpperBound to which it refers.
 */
const UserDefinedConstraint*
ListOfUserDefinedConstraints::getByUpperBound(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqUB(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    UserDefinedConstraint*> (*result);
}


/*
 * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based on
 * the UpperBound to which it refers.
 */
UserDefinedConstraint*
ListOfUserDefinedConstraints::getByUpperBound(const std::string& sid)
{
  return const_cast<UserDefinedConstraint*>(static_cast<const
    ListOfUserDefinedConstraints&>(*this).getByUpperBound(sid));
}


/*
 * Returns the XML element name of this ListOfUserDefinedConstraints object.
 */
const std::string&
ListOfUserDefinedConstraints::getElementName() const
{
  static const string name = "listOfUserDefinedConstraints";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfUserDefinedConstraints object.
 */
int
ListOfUserDefinedConstraints::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfUserDefinedConstraints object.
 */
int
ListOfUserDefinedConstraints::getItemTypeCode() const
{
  return SBML_FBC_USERDEFINEDCONSTRAINT;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new UserDefinedConstraint in this ListOfUserDefinedConstraints
 */
SBase*
ListOfUserDefinedConstraints::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());

  if (name == "userDefinedConstraint")
  {
    object = new UserDefinedConstraint(fbcns);
    appendAndOwn(object);
  }

  delete fbcns;
  return object;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get an UserDefinedConstraint_t from the ListOf_t.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_getUserDefinedConstraint(ListOf_t* lo,
                                                      unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUserDefinedConstraints*>(lo)->get(n);
}


/*
 * Get an UserDefinedConstraint_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfUserDefinedConstraints*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth UserDefinedConstraint_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUserDefinedConstraints*>(lo)->remove(n);
}


/*
 * Removes the UserDefinedConstraint_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfUserDefinedConstraints*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


