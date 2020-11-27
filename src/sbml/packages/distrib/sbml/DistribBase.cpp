/**
 * @file DistribBase.cpp
 * @brief Implementation of the DistribBase class.
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
#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/Uncertainty.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribBase using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribBase::DistribBase(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : SBase(level, version)
  , mElementName("distribBase")
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribBase using the given DistribPkgNamespaces object.
 */
DistribBase::DistribBase(DistribPkgNamespaces *distribns)
  : SBase(distribns)
  , mElementName("distribBase")
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribBase.
 */
DistribBase::DistribBase(const DistribBase& orig)
  : SBase( orig )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for DistribBase.
 */
DistribBase&
DistribBase::operator=(const DistribBase& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribBase object.
 */
DistribBase*
DistribBase::clone() const
{
  return new DistribBase(*this);
}


/*
 * Destructor for DistribBase.
 */
DistribBase::~DistribBase()
{
}


/*
 * Returns the value of the "id" attribute of this DistribBase.
 */
const std::string&
DistribBase::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribBase.
 */
const std::string&
DistribBase::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribBase's "id" attribute is set.
 */
bool
DistribBase::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribBase's "name" attribute is set.
 */
bool
DistribBase::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribBase.
 */
int
DistribBase::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribBase.
 */
int
DistribBase::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribBase.
 */
int
DistribBase::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribBase.
 */
int
DistribBase::unsetName()
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
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * Uncertainty
 */
bool
DistribBase::isUncertainty() const
{
  return dynamic_cast<const Uncertainty*>(this) != NULL;
}


/*
 * Returns the XML element name of this DistribBase object.
 */
const std::string&
DistribBase::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this DistribBase object.
 */
void
DistribBase::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this DistribBase object.
 */
int
DistribBase::getTypeCode() const
{
  return SBML_DISTRIB_DISTRIBBASE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribBase object have been set.
 */
bool
DistribBase::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribBase::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribBase::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribBase::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribBase::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
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

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribBase's attribute "attributeName"
 * is set.
 */
bool
DistribBase::isSetAttribute(const std::string& attributeName) const
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

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName,
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

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::unsetAttribute(const std::string& attributeName)
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

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribBase::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribBase::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

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
        log->logPackageError("distrib", DistribDistribBaseAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribBaseAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribBase>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DistribBase>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribBase::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Uncertainty using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
Uncertainty_t *
DistribBase_createUncertainty(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion)
{
  return new Uncertainty(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribBase_t object.
 */
LIBSBML_EXTERN
DistribBase_t*
DistribBase_clone(const DistribBase_t* db)
{
  if (db != NULL)
  {
    return static_cast<DistribBase_t*>(db->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribBase_t object.
 */
LIBSBML_EXTERN
void
DistribBase_free(DistribBase_t* db)
{
  if (db != NULL)
  {
    delete db;
  }
}


/*
 * Returns the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
char *
DistribBase_getId(const DistribBase_t * db)
{
  if (db == NULL)
  {
    return NULL;
  }

  return db->getId().empty() ? NULL : safe_strdup(db->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
char *
DistribBase_getName(const DistribBase_t * db)
{
  if (db == NULL)
  {
    return NULL;
  }

  return db->getName().empty() ? NULL : safe_strdup(db->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribBase_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DistribBase_isSetId(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribBase_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DistribBase_isSetName(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_setId(DistribBase_t * db, const char * id)
{
  return (db != NULL) ? db->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_setName(DistribBase_t * db, const char * name)
{
  return (db != NULL) ? db->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_unsetId(DistribBase_t * db)
{
  return (db != NULL) ? db->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_unsetName(DistribBase_t * db)
{
  return (db != NULL) ? db->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type Uncertainty_t
 */
LIBSBML_EXTERN
int
DistribBase_isUncertainty(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isUncertainty()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBase_t object have been set.
 */
LIBSBML_EXTERN
int
DistribBase_hasRequiredAttributes(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


