/**
 * @file Distribution.cpp
 * @brief Implementation of the Distribution class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/distrib/sbml/Distribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Distribution using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
Distribution::Distribution(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mMath (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new Distribution using the given DistribPkgNamespaces object.
 */
Distribution::Distribution(DistribPkgNamespaces *distribns)
  : DistribBase(distribns)
  , mMath (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for Distribution.
 */
Distribution::Distribution(const Distribution& orig)
  : DistribBase( orig )
  , mMath ( NULL )
{
  if (orig.mMath != NULL)
  {
    mMath = orig.mMath->deepCopy();
  }

  connectToChild();
}


/*
 * Assignment operator for Distribution.
 */
Distribution&
Distribution::operator=(const Distribution& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    delete mMath;
    if (rhs.mMath != NULL)
    {
      mMath = rhs.mMath->deepCopy();
    }
    else
    {
      mMath = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Distribution object.
 */
Distribution*
Distribution::clone() const
{
  return new Distribution(*this);
}


/*
 * Destructor for Distribution.
 */
Distribution::~Distribution()
{
  delete mMath;
  mMath = NULL;
}


/*
 * Returns the value of the "math" element of this Distribution.
 */
const ASTNode*
Distribution::getMath() const
{
  return mMath;
}


/*
 * Returns the value of the "math" element of this Distribution.
 */
ASTNode*
Distribution::getMath()
{
  return mMath;
}


/*
 * Predicate returning @c true if this Distribution's "math" element is set.
 */
bool
Distribution::isSetMath() const
{
  return (mMath != NULL);
}


/*
 * Sets the value of the "math" element of this Distribution.
 */
int
Distribution::setMath(const ASTNode* math)
{
  if (mMath == math)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(math->isWellFormedASTNode()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mMath;
    mMath = (math != NULL) ? math->deepCopy() : NULL;
    if (mMath != NULL)
    {
      mMath->setParentSBMLObject(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "math" element of this Distribution.
 */
int
Distribution::unsetMath()
{
  delete mMath;
  mMath = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this Distribution object.
 */
const std::string&
Distribution::getElementName() const
{
  static const string name = "distribution";
  return name;
}


/*
 * Returns the libSBML type code for this Distribution object.
 */
int
Distribution::getTypeCode() const
{
  return SBML_DISTRIB_DISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * Distribution object have been set.
 */
bool
Distribution::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * Distribution object have been set.
 */
bool
Distribution::hasRequiredElements() const
{
  bool allPresent = DistribBase::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Distribution::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  if (isSetMath() == true)
  {
    writeMathML(getMath(), stream, getSBMLNamespaces());
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Distribution::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Distribution::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Distribution::connectToChild()
{
  DistribBase::connectToChild();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Distribution::enablePackageInternal(const std::string& pkgURI,
                                    const std::string& pkgPrefix,
                                    bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
Distribution::updateSBMLNamespace(const std::string& package,
                                  unsigned int level,
                                  unsigned int version)
{
  DistribBase::updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::getAttribute(const std::string& attributeName,
                           bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::getAttribute(const std::string& attributeName,
                           double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::getAttribute(const std::string& attributeName,
                           unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::getAttribute(const std::string& attributeName,
                           std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Distribution's attribute "attributeName"
 * is set.
 */
bool
Distribution::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::setAttribute(const std::string& attributeName, double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::setAttribute(const std::string& attributeName,
                           unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::setAttribute(const std::string& attributeName,
                           const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Distribution.
 */
int
Distribution::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Distribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
Distribution::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Distribution::readAttributes(const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistributionAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads other XML such as math/notes etc.
 */
bool
Distribution::readOtherXML(XMLInputStream& stream)
{
  bool read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    const XMLToken elem = stream.peek();
    const std::string prefix = checkMathMLNamespace(elem);
    if (stream.getSBMLNamespaces() == NULL)
    {
      stream.setSBMLNamespaces(new SBMLNamespaces(getLevel(), getVersion()));
    }

    delete mMath;
    mMath = readMathML(stream, prefix);
    read = true;
  }

  if (DistribBase::readOtherXML(stream))
  {
    read = true;
  }

  return read;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Distribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Distribution_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
Distribution_t *
Distribution_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion)
{
  return new Distribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Distribution_t object.
 */
LIBSBML_EXTERN
Distribution_t*
Distribution_clone(const Distribution_t* d)
{
  if (d != NULL)
  {
    return static_cast<Distribution_t*>(d->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Distribution_t object.
 */
LIBSBML_EXTERN
void
Distribution_free(Distribution_t* d)
{
  if (d != NULL)
  {
    delete d;
  }
}


/*
 * Returns the value of the "math" element of this Distribution_t.
 */
LIBSBML_EXTERN
const ASTNode_t*
Distribution_getMath(const Distribution_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return (ASTNode_t*)(d->getMath());
}


/*
 * Predicate returning @c 1 (true) if this Distribution_t's "math" element is
 * set.
 */
LIBSBML_EXTERN
int
Distribution_isSetMath(const Distribution_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetMath()) : 0;
}


/*
 * Sets the value of the "math" element of this Distribution_t.
 */
LIBSBML_EXTERN
int
Distribution_setMath(Distribution_t * d, const ASTNode_t* math)
{
  return (d != NULL) ? d->setMath(math) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "math" element of this Distribution_t.
 */
LIBSBML_EXTERN
int
Distribution_unsetMath(Distribution_t * d)
{
  return (d != NULL) ? d->unsetMath() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Distribution_t object have been set.
 */
LIBSBML_EXTERN
int
Distribution_hasRequiredAttributes(const Distribution_t * d)
{
  return (d != NULL) ? static_cast<int>(d->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * Distribution_t object have been set.
 */
LIBSBML_EXTERN
int
Distribution_hasRequiredElements(const Distribution_t * d)
{
  return (d != NULL) ? static_cast<int>(d->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


