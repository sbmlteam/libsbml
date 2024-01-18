/**
 * @file UserDefinedConstraint.cpp
 * @brief Implementation of the UserDefinedConstraint class.
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
#include <sbml/packages/fbc/sbml/UserDefinedConstraint.h>
#include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraints.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new UserDefinedConstraint using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 */
UserDefinedConstraint::UserDefinedConstraint(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
  : SBase(level, version)
  , mLowerBound ("")
  , mUpperBound ("")
  , mUserDefinedConstraintComponents (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new UserDefinedConstraint using the given FbcPkgNamespaces object.
 */
UserDefinedConstraint::UserDefinedConstraint(FbcPkgNamespaces *fbcns)
  : SBase(fbcns)
  , mLowerBound ("")
  , mUpperBound ("")
  , mUserDefinedConstraintComponents (fbcns)
{
  setElementNamespace(fbcns->getURI());
  connectToChild();
  loadPlugins(fbcns);
}


/*
 * Copy constructor for UserDefinedConstraint.
 */
UserDefinedConstraint::UserDefinedConstraint(const UserDefinedConstraint& orig)
  : SBase( orig )
  , mLowerBound ( orig.mLowerBound )
  , mUpperBound ( orig.mUpperBound )
  , mUserDefinedConstraintComponents ( orig.mUserDefinedConstraintComponents )
{
  connectToChild();
}


/*
 * Assignment operator for UserDefinedConstraint.
 */
UserDefinedConstraint&
UserDefinedConstraint::operator=(const UserDefinedConstraint& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mLowerBound = rhs.mLowerBound;
    mUpperBound = rhs.mUpperBound;
    mUserDefinedConstraintComponents = rhs.mUserDefinedConstraintComponents;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this UserDefinedConstraint object.
 */
UserDefinedConstraint*
UserDefinedConstraint::clone() const
{
  return new UserDefinedConstraint(*this);
}


/*
 * Destructor for UserDefinedConstraint.
 */
UserDefinedConstraint::~UserDefinedConstraint()
{
}


/*
 * Returns the value of the "id" attribute of this UserDefinedConstraint.
 */
const std::string&
UserDefinedConstraint::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this UserDefinedConstraint.
 */
const std::string&
UserDefinedConstraint::getName() const
{
  return mName;
}


/*
 * Returns the value of the "lowerBound" attribute of this
 * UserDefinedConstraint.
 */
const std::string&
UserDefinedConstraint::getLowerBound() const
{
  return mLowerBound;
}


/*
 * Returns the value of the "upperBound" attribute of this
 * UserDefinedConstraint.
 */
const std::string&
UserDefinedConstraint::getUpperBound() const
{
  return mUpperBound;
}


/*
 * Predicate returning @c true if this UserDefinedConstraint's "id" attribute
 * is set.
 */
bool
UserDefinedConstraint::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraint's "name" attribute
 * is set.
 */
bool
UserDefinedConstraint::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraint's "lowerBound"
 * attribute is set.
 */
bool
UserDefinedConstraint::isSetLowerBound() const
{
  return (mLowerBound.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraint's "upperBound"
 * attribute is set.
 */
bool
UserDefinedConstraint::isSetUpperBound() const
{
  return (mUpperBound.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this UserDefinedConstraint.
 */
int
UserDefinedConstraint::setId(const std::string& id)
{
  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    return SyntaxChecker::checkAndSetSId(id, mId);
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "name" attribute of this UserDefinedConstraint.
 */
int
UserDefinedConstraint::setName(const std::string& name)
{
  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "lowerBound" attribute of this UserDefinedConstraint.
 */
int
UserDefinedConstraint::setLowerBound(const std::string& lowerBound)
{
  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    if (!(SyntaxChecker::isValidInternalSId(lowerBound)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mLowerBound = lowerBound;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "upperBound" attribute of this UserDefinedConstraint.
 */
int
UserDefinedConstraint::setUpperBound(const std::string& upperBound)
{
  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    if (!(SyntaxChecker::isValidInternalSId(upperBound)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mUpperBound = upperBound;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Unsets the value of the "id" attribute of this UserDefinedConstraint.
 */
int
UserDefinedConstraint::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "name" attribute of this UserDefinedConstraint.
 */
int
UserDefinedConstraint::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "lowerBound" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::unsetLowerBound()
{
  mLowerBound.erase();

  if (mLowerBound.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "upperBound" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::unsetUpperBound()
{
  mUpperBound.erase();

  if (mUpperBound.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfUserDefinedConstraintComponents from this
 * UserDefinedConstraint.
 */
const ListOfUserDefinedConstraintComponents*
UserDefinedConstraint::getListOfUserDefinedConstraintComponents() const
{
  return &mUserDefinedConstraintComponents;
}


/*
 * Returns the ListOfUserDefinedConstraintComponents from this
 * UserDefinedConstraint.
 */
ListOfUserDefinedConstraintComponents*
UserDefinedConstraint::getListOfUserDefinedConstraintComponents()
{
  return &mUserDefinedConstraintComponents;
}


/*
 * Get an UserDefinedConstraintComponent from the UserDefinedConstraint.
 */
UserDefinedConstraintComponent*
UserDefinedConstraint::getUserDefinedConstraintComponent(unsigned int n)
{
  return mUserDefinedConstraintComponents.get(n);
}


/*
 * Get an UserDefinedConstraintComponent from the UserDefinedConstraint.
 */
const UserDefinedConstraintComponent*
UserDefinedConstraint::getUserDefinedConstraintComponent(unsigned int n) const
{
  return mUserDefinedConstraintComponents.get(n);
}


/*
 * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
 * on its identifier.
 */
UserDefinedConstraintComponent*
UserDefinedConstraint::getUserDefinedConstraintComponent(const std::string&
  sid)
{
  return mUserDefinedConstraintComponents.get(sid);
}


/*
 * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
 * on its identifier.
 */
const UserDefinedConstraintComponent*
UserDefinedConstraint::getUserDefinedConstraintComponent(const std::string&
  sid) const
{
  return mUserDefinedConstraintComponents.get(sid);
}


/*
 * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
 * on the Variable to which it refers.
 */
const UserDefinedConstraintComponent*
UserDefinedConstraint::getUserDefinedConstraintComponentByVariable(const
  std::string& sid) const
{
  return mUserDefinedConstraintComponents.getByVariable(sid);
}


/*
 * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
 * on the Variable to which it refers.
 */
UserDefinedConstraintComponent*
UserDefinedConstraint::getUserDefinedConstraintComponentByVariable(const
  std::string& sid)
{
  return mUserDefinedConstraintComponents.getByVariable(sid);
}


/*
 * Adds a copy of the given UserDefinedConstraintComponent to this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::addUserDefinedConstraintComponent(const
  UserDefinedConstraintComponent* udcc)
{
  if (udcc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (udcc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != udcc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(udcc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (udcc->isSetId() &&
    (mUserDefinedConstraintComponents.get(udcc->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mUserDefinedConstraintComponents.append(udcc);
  }
}


/*
 * Get the number of UserDefinedConstraintComponent objects in this
 * UserDefinedConstraint.
 */
unsigned int
UserDefinedConstraint::getNumUserDefinedConstraintComponents() const
{
  return mUserDefinedConstraintComponents.size();
}


/*
 * Creates a new UserDefinedConstraintComponent object, adds it to this
 * UserDefinedConstraint object and returns the UserDefinedConstraintComponent
 * object created.
 */
UserDefinedConstraintComponent*
UserDefinedConstraint::createUserDefinedConstraintComponent()
{
  UserDefinedConstraintComponent* udcc = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(),
      getPackageVersion());
    udcc = new UserDefinedConstraintComponent(fbcns);
    delete fbcns;
  }
  catch (...)
  {
  }

  if (udcc != NULL)
  {
    mUserDefinedConstraintComponents.appendAndOwn(udcc);
  }

  return udcc;
}


/*
 * Removes the nth UserDefinedConstraintComponent from this
 * UserDefinedConstraint and returns a pointer to it.
 */
UserDefinedConstraintComponent*
UserDefinedConstraint::removeUserDefinedConstraintComponent(unsigned int n)
{
  return mUserDefinedConstraintComponents.remove(n);
}


/*
 * Removes the UserDefinedConstraintComponent from this UserDefinedConstraint
 * based on its identifier and returns a pointer to it.
 */
UserDefinedConstraintComponent*
UserDefinedConstraint::removeUserDefinedConstraintComponent(const std::string&
  sid)
{
  return mUserDefinedConstraintComponents.remove(sid);
}


/*
 * @copydoc doc_renamesidref_common
 */
void
UserDefinedConstraint::renameSIdRefs(const std::string& oldid,
                                     const std::string& newid)
{
  if (isSetLowerBound() && mLowerBound == oldid)
  {
    setLowerBound(newid);
  }

  if (isSetUpperBound() && mUpperBound == oldid)
  {
    setUpperBound(newid);
  }
}


/*
 * Returns the XML element name of this UserDefinedConstraint object.
 */
const std::string&
UserDefinedConstraint::getElementName() const
{
  static const string name = "userDefinedConstraint";
  return name;
}


/*
 * Returns the libSBML type code for this UserDefinedConstraint object.
 */
int
UserDefinedConstraint::getTypeCode() const
{
  return SBML_FBC_USERDEFINEDCONSTRAINT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * UserDefinedConstraint object have been set.
 */
bool
UserDefinedConstraint::hasRequiredAttributes() const
{
  bool allPresent = true;

  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    if (isSetLowerBound() == false)
    {
      allPresent = false;
    }
  }

  if (pkgVersion >= 3)
  {
    if (isSetUpperBound() == false)
    {
      allPresent = false;
    }
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * UserDefinedConstraint object have been set.
 */
bool
UserDefinedConstraint::hasRequiredElements() const
{
  bool allPresent = true;

  if (getNumUserDefinedConstraintComponents() == 0)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
UserDefinedConstraint::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumUserDefinedConstraintComponents() > 0)
  {
    mUserDefinedConstraintComponents.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
UserDefinedConstraint::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mUserDefinedConstraintComponents.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
UserDefinedConstraint::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mUserDefinedConstraintComponents.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
UserDefinedConstraint::connectToChild()
{
  SBase::connectToChild();

  mUserDefinedConstraintComponents.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
UserDefinedConstraint::enablePackageInternal(const std::string& pkgURI,
                                             const std::string& pkgPrefix,
                                             bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mUserDefinedConstraintComponents.enablePackageInternal(pkgURI, pkgPrefix,
    flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
UserDefinedConstraint::updateSBMLNamespace(const std::string& package,
                                           unsigned int level,
                                           unsigned int version)
{
  SBase::updateSBMLNamespace(package, level, version);

  mUserDefinedConstraintComponents.updateSBMLNamespace(package, level,
    version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::getAttribute(const std::string& attributeName,
                                    bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::getAttribute(const std::string& attributeName,
                                    int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::getAttribute(const std::string& attributeName,
                                    double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::getAttribute(const std::string& attributeName,
                                    unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::getAttribute(const std::string& attributeName,
                                    std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "lowerBound")
  {
    value = getLowerBound();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "upperBound")
  {
    value = getUpperBound();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this UserDefinedConstraint's attribute
 * "attributeName" is set.
 */
bool
UserDefinedConstraint::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "lowerBound")
  {
    value = isSetLowerBound();
  }
  else if (attributeName == "upperBound")
  {
    value = isSetUpperBound();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::setAttribute(const std::string& attributeName,
                                    bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::setAttribute(const std::string& attributeName,
                                    int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::setAttribute(const std::string& attributeName,
                                    double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::setAttribute(const std::string& attributeName,
                                    unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::setAttribute(const std::string& attributeName,
                                    const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "lowerBound")
  {
    return_value = setLowerBound(value);
  }
  else if (attributeName == "upperBound")
  {
    return_value = setUpperBound(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * UserDefinedConstraint.
 */
int
UserDefinedConstraint::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "lowerBound")
  {
    value = unsetLowerBound();
  }
  else if (attributeName == "upperBound")
  {
    value = unsetUpperBound();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * UserDefinedConstraint.
 */
SBase*
UserDefinedConstraint::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "userDefinedConstraintComponent")
  {
    return createUserDefinedConstraintComponent();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this UserDefinedConstraint.
 */
int
UserDefinedConstraint::addChildObject(const std::string& elementName,
                                      const SBase* element)
{
  if (elementName == "userDefinedConstraintComponent" && element->getTypeCode()
    == SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT)
  {
    return addUserDefinedConstraintComponent((const
      UserDefinedConstraintComponent*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * UserDefinedConstraint.
 */
SBase*
UserDefinedConstraint::removeChildObject(const std::string& elementName,
                                         const std::string& id)
{
  if (elementName == "userDefinedConstraintComponent")
  {
    return removeUserDefinedConstraintComponent(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this UserDefinedConstraint.
 */
unsigned int
UserDefinedConstraint::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "userDefinedConstraintComponent")
  {
    return getNumUserDefinedConstraintComponents();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this UserDefinedConstraint.
 */
SBase*
UserDefinedConstraint::getObject(const std::string& elementName,
                                 unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "userDefinedConstraintComponent")
  {
    return getUserDefinedConstraintComponent(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
UserDefinedConstraint::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mUserDefinedConstraintComponents.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
UserDefinedConstraint::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUserDefinedConstraintComponents.getMetaId() == metaid)
  {
    return &mUserDefinedConstraintComponents;
  }

  obj = mUserDefinedConstraintComponents.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
UserDefinedConstraint::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mUserDefinedConstraintComponents, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
UserDefinedConstraint::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  if (name == "listOfUserDefinedConstraintComponents")
  {
    if (getErrorLog() && mUserDefinedConstraintComponents.size() != 0)
    {
      getErrorLog()->logPackageError("fbc",
        FbcUserDefinedConstraintAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), "", getLine(), getColumn());
    }

    obj = &mUserDefinedConstraintComponents;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
UserDefinedConstraint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    attributes.add("id");
    attributes.add("name");
    attributes.add("lowerBound");
    attributes.add("upperBound");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UserDefinedConstraint::readAttributes(const XMLAttributes& attributes,
                                      const ExpectedAttributes&
                                        expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();


  if (log && getParentSBMLObject() && static_cast<ListOfUserDefinedConstraints*>(getParentSBMLObject())->size()
    < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc", FbcLOUserConstraintsAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc",
          FbcLOUserConstraintsAllowedAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc", FbcUserDefinedConstraintAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc",
          FbcUserDefinedConstraintAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  if (pkgVersion >= 3)
  {
    readL3V1V3Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UserDefinedConstraint::readL3V1V3Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<UserDefinedConstraint>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version,
        "The id on the <" + getElementName() + "> is '" + mId + "', which does "
          "not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<UserDefinedConstraint>");
    }
  }

  // 
  // lowerBound SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("lowerBound", mLowerBound);

  if (assigned == true)
  {
    if (mLowerBound.empty() == true)
    {
      logEmptyString(mLowerBound, level, version, "<UserDefinedConstraint>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mLowerBound) == false)
    {
      std::string msg = "The lowerBound attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mLowerBound + "', which does not conform to the "
        "syntax.";
      log->logPackageError("fbc",
        FbcUserDefinedConstraintLowerBoundMustBeParameter, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Fbc attribute 'lowerBound' is missing from the "
        "<UserDefinedConstraint> element.";
      log->logPackageError("fbc", FbcUserDefinedConstraintAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // upperBound SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("upperBound", mUpperBound);

  if (assigned == true)
  {
    if (mUpperBound.empty() == true)
    {
      logEmptyString(mUpperBound, level, version, "<UserDefinedConstraint>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mUpperBound) == false)
    {
      std::string msg = "The upperBound attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mUpperBound + "', which does not conform to the "
        "syntax.";
      log->logPackageError("fbc",
        FbcUserDefinedConstraintUpperBoundMustBeParameter, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Fbc attribute 'upperBound' is missing from the "
        "<UserDefinedConstraint> element.";
      log->logPackageError("fbc", FbcUserDefinedConstraintAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UserDefinedConstraint::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  unsigned int pkgVersion = getPackageVersion();

  if (pkgVersion >= 3)
  {
    writeL3V1V3Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UserDefinedConstraint::writeL3V1V3Attributes(XMLOutputStream& stream) const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetLowerBound() == true)
  {
    stream.writeAttribute("lowerBound", getPrefix(), mLowerBound);
  }

  if (isSetUpperBound() == true)
  {
    stream.writeAttribute("upperBound", getPrefix(), mUpperBound);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new UserDefinedConstraint_t using the given SBML Level, Version
 * and &ldquo;fbc&rdquo; package version.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t *
UserDefinedConstraint_create(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
{
  return new UserDefinedConstraint(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this UserDefinedConstraint_t object.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
UserDefinedConstraint_clone(const UserDefinedConstraint_t* udc)
{
  if (udc != NULL)
  {
    return static_cast<UserDefinedConstraint_t*>(udc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this UserDefinedConstraint_t object.
 */
LIBSBML_EXTERN
void
UserDefinedConstraint_free(UserDefinedConstraint_t* udc)
{
  if (udc != NULL)
  {
    delete udc;
  }
}


/*
 * Returns the value of the "id" attribute of this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getId(const UserDefinedConstraint_t * udc)
{
  if (udc == NULL)
  {
    return NULL;
  }

  return udc->getId().empty() ? NULL : safe_strdup(udc->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getName(const UserDefinedConstraint_t * udc)
{
  if (udc == NULL)
  {
    return NULL;
  }

  return udc->getName().empty() ? NULL : safe_strdup(udc->getName().c_str());
}


/*
 * Returns the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getLowerBound(const UserDefinedConstraint_t * udc)
{
  if (udc == NULL)
  {
    return NULL;
  }

  return udc->getLowerBound().empty() ? NULL :
    safe_strdup(udc->getLowerBound().c_str());
}


/*
 * Returns the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getUpperBound(const UserDefinedConstraint_t * udc)
{
  if (udc == NULL)
  {
    return NULL;
  }

  return udc->getUpperBound().empty() ? NULL :
    safe_strdup(udc->getUpperBound().c_str());
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetId(const UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? static_cast<int>(udc->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetName(const UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? static_cast<int>(udc->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's
 * "lowerBound" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetLowerBound(const UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? static_cast<int>(udc->isSetLowerBound()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's
 * "upperBound" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetUpperBound(const UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? static_cast<int>(udc->isSetUpperBound()) : 0;
}


/*
 * Sets the value of the "id" attribute of this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setId(UserDefinedConstraint_t * udc, const char * id)
{
  return (udc != NULL) ? udc->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setName(UserDefinedConstraint_t * udc,
                              const char * name)
{
  return (udc != NULL) ? udc->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setLowerBound(UserDefinedConstraint_t * udc,
                                    const char * lowerBound)
{
  return (udc != NULL) ? udc->setLowerBound(lowerBound) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setUpperBound(UserDefinedConstraint_t * udc,
                                    const char * upperBound)
{
  return (udc != NULL) ? udc->setUpperBound(upperBound) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetId(UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? udc->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetName(UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? udc->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetLowerBound(UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? udc->unsetLowerBound() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetUpperBound(UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? udc->unsetUpperBound() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing UserDefinedConstraintComponent_t objects
 * from this UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
ListOf_t*
UserDefinedConstraint_getListOfUserDefinedConstraintComponents(UserDefinedConstraint_t*
  udc)
{
  return (udc != NULL) ? udc->getListOfUserDefinedConstraintComponents() :
    NULL;
}


/*
 * Get an UserDefinedConstraintComponent_t from the UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_getUserDefinedConstraintComponent(
                                                        UserDefinedConstraint_t*
                                                          udc,
                                                        unsigned int n)
{
  return (udc != NULL) ? udc->getUserDefinedConstraintComponent(n) : NULL;
}


/*
 * Get an UserDefinedConstraintComponent_t from the UserDefinedConstraint_t
 * based on its identifier.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_getUserDefinedConstraintComponentById(
                                                            UserDefinedConstraint_t*
                                                              udc,
                                                            const char *sid)
{
  return (udc != NULL && sid != NULL) ?
    udc->getUserDefinedConstraintComponent(sid) : NULL;
}


/*
 * Get an UserDefinedConstraintComponent_t from the UserDefinedConstraint_t
 * based on the Variable to which it refers.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_getUserDefinedConstraintComponentByVariable(
                                                                  UserDefinedConstraint_t*
                                                                    udc,
                                                                  const char
                                                                    *sid)
{
  return (udc != NULL && sid != NULL) ?
    udc->getUserDefinedConstraintComponentByVariable(sid) : NULL;
}


/*
 * Adds a copy of the given UserDefinedConstraintComponent_t to this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_addUserDefinedConstraintComponent(
                                                        UserDefinedConstraint_t*
                                                          udc,
                                                        const UserDefinedConstraintComponent_t*
                                                          udcc)
{
  return (udc != NULL) ? udc->addUserDefinedConstraintComponent(udcc) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of UserDefinedConstraintComponent_t objects in this
 * UserDefinedConstraint_t.
 */
LIBSBML_EXTERN
unsigned int
UserDefinedConstraint_getNumUserDefinedConstraintComponents(UserDefinedConstraint_t*
  udc)
{
  return (udc != NULL) ? udc->getNumUserDefinedConstraintComponents() :
    SBML_INT_MAX;
}


/*
 * Creates a new UserDefinedConstraintComponent_t object, adds it to this
 * UserDefinedConstraint_t object and returns the
 * UserDefinedConstraintComponent_t object created.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_createUserDefinedConstraintComponent(UserDefinedConstraint_t*
  udc)
{
  return (udc != NULL) ? udc->createUserDefinedConstraintComponent() : NULL;
}


/*
 * Removes the nth UserDefinedConstraintComponent_t from this
 * UserDefinedConstraint_t and returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_removeUserDefinedConstraintComponent(
                                                           UserDefinedConstraint_t*
                                                             udc,
                                                           unsigned int n)
{
  return (udc != NULL) ? udc->removeUserDefinedConstraintComponent(n) : NULL;
}


/*
 * Removes the UserDefinedConstraintComponent_t from this
 * UserDefinedConstraint_t based on its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_removeUserDefinedConstraintComponentById(
                                                               UserDefinedConstraint_t*
                                                                 udc,
                                                               const char* sid)
{
  return (udc != NULL && sid != NULL) ?
    udc->removeUserDefinedConstraintComponent(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UserDefinedConstraint_t object have been set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_hasRequiredAttributes(const UserDefinedConstraint_t *
  udc)
{
  return (udc != NULL) ? static_cast<int>(udc->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * UserDefinedConstraint_t object have been set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_hasRequiredElements(const UserDefinedConstraint_t * udc)
{
  return (udc != NULL) ? static_cast<int>(udc->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


