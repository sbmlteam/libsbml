/**
 * @file ListOfLocalRenderInformation.cpp
 * @brief Implementation of the ListOfLocalRenderInformation class.
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
#include <sbml/packages/render/sbml/ListOfLocalRenderInformation.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfLocalRenderInformation using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
ListOfLocalRenderInformation::ListOfLocalRenderInformation(unsigned int level,
                                                           unsigned int
                                                             version,
                                                           unsigned int
                                                             pkgVersion)
  : ListOf(level, version)
  , mMajorVersion (SBML_INT_MAX)
  , mIsSetMajorVersion (false)
  , mMinorVersion (SBML_INT_MAX)
  , mIsSetMinorVersion (false)
  , mDefaultValues (NULL)
{
  //if (level == 3)
  //  mDefaultValues = new DefaultValues(level, version, pkgVersion);
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new ListOfLocalRenderInformation using the given
 * RenderPkgNamespaces object.
 */
ListOfLocalRenderInformation::ListOfLocalRenderInformation(RenderPkgNamespaces
  *renderns)
  : ListOf(renderns)
  , mMajorVersion (SBML_INT_MAX)
  , mIsSetMajorVersion (false)
  , mMinorVersion (SBML_INT_MAX)
  , mIsSetMinorVersion (false)
  , mDefaultValues (NULL)
{
  //if (getLevel() == 3)
  //  mDefaultValues = new DefaultValues(renderns);
  setElementNamespace(renderns->getURI());
  connectToChild();
}


/*
 * Copy constructor for ListOfLocalRenderInformation.
 */
ListOfLocalRenderInformation::ListOfLocalRenderInformation(const
  ListOfLocalRenderInformation& orig)
  : ListOf( orig )
  , mMajorVersion ( orig.mMajorVersion )
  , mIsSetMajorVersion ( orig.mIsSetMajorVersion )
  , mMinorVersion ( orig.mMinorVersion )
  , mIsSetMinorVersion ( orig.mIsSetMinorVersion )
  , mDefaultValues ( NULL )
{
  if (orig.mDefaultValues != NULL)
  {
    mDefaultValues = orig.mDefaultValues->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for ListOfLocalRenderInformation.
 */
ListOfLocalRenderInformation&
ListOfLocalRenderInformation::operator=(const ListOfLocalRenderInformation&
  rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
    mMajorVersion = rhs.mMajorVersion;
    mIsSetMajorVersion = rhs.mIsSetMajorVersion;
    mMinorVersion = rhs.mMinorVersion;
    mIsSetMinorVersion = rhs.mIsSetMinorVersion;
    delete mDefaultValues;
    if (rhs.mDefaultValues != NULL)
    {
      mDefaultValues = rhs.mDefaultValues->clone();
    }
    else
    {
      mDefaultValues = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfLocalRenderInformation object.
 */
ListOfLocalRenderInformation*
ListOfLocalRenderInformation::clone() const
{
  return new ListOfLocalRenderInformation(*this);
}


/*
 * Destructor for ListOfLocalRenderInformation.
 */
ListOfLocalRenderInformation::~ListOfLocalRenderInformation()
{
  delete mDefaultValues;
  mDefaultValues = NULL;
}


/*
 * Returns the value of the "majorVersion" attribute of this
 * ListOfLocalRenderInformation.
 */
unsigned int
ListOfLocalRenderInformation::getMajorVersion() const
{
  return mMajorVersion;
}


/*
 * Returns the value of the "minorVersion" attribute of this
 * ListOfLocalRenderInformation.
 */
unsigned int
ListOfLocalRenderInformation::getMinorVersion() const
{
  return mMinorVersion;
}


std::string ListOfLocalRenderInformation::getVersionString() const
{
  std::ostringstream os;
  os << this->mMajorVersion << "." << this->mMinorVersion;
  return os.str();
}


/*
 * Predicate returning @c true if this ListOfLocalRenderInformation's
 * "majorVersion" attribute is set.
 */
bool
ListOfLocalRenderInformation::isSetMajorVersion() const
{
  return mIsSetMajorVersion;
}

/** @cond doxygenLibsbmlInternal */
bool
ListOfLocalRenderInformation::isSetVersionMajor() const
{
  return mIsSetMajorVersion;
}
/** @endcond */


/*
 * Predicate returning @c true if this ListOfLocalRenderInformation's
 * "minorVersion" attribute is set.
 */
bool
ListOfLocalRenderInformation::isSetMinorVersion() const
{
  return mIsSetMinorVersion;
}


/** @cond doxygenLibsbmlInternal */
bool
ListOfLocalRenderInformation::isSetVersionMinor() const
{
  return mIsSetMinorVersion;
}
/** @endcond */


/*
 * Sets the value of the "majorVersion" attribute of this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::setMajorVersion(unsigned int majorVersion)
{
  mMajorVersion = majorVersion;
  mIsSetMajorVersion = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int
ListOfLocalRenderInformation::setVersionMajor(unsigned int majorVersion)
{
  return setMajorVersion(majorVersion);
}
/** @endcond */

/*
 * Sets the value of the "minorVersion" attribute of this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::setMinorVersion(unsigned int minorVersion)
{
  mMinorVersion = minorVersion;
  mIsSetMinorVersion = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int
ListOfLocalRenderInformation::setVersionMinor(unsigned int minorVersion)
{
  return setMinorVersion(minorVersion);
}
/** @endcond */


void ListOfLocalRenderInformation::setVersion(unsigned int major, unsigned int minor)
{
  setMajorVersion(major);
  setMinorVersion(minor);
}


/*
 * Unsets the value of the "majorVersion" attribute of this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::unsetMajorVersion()
{
  mMajorVersion = SBML_INT_MAX;
  mIsSetMajorVersion = false;

  if (isSetMajorVersion() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/** @cond doxygenLibsbmlInternal */
int
ListOfLocalRenderInformation::unsetVersionMajor()
{
  return unsetMajorVersion();
}
/** @endcond */

/*
 * Unsets the value of the "minorVersion" attribute of this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::unsetMinorVersion()
{
  mMinorVersion = SBML_INT_MAX;
  mIsSetMinorVersion = false;

  if (isSetMinorVersion() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/** @cond doxygenLibsbmlInternal */
int
ListOfLocalRenderInformation::unsetVersionMinor()
{
  return unsetMinorVersion();
}
/** @endcond */


/*
 * Returns the value of the "defaultValues" element of this
 * ListOfLocalRenderInformation.
 */
const DefaultValues*
ListOfLocalRenderInformation::getDefaultValues() const
{
  return mDefaultValues;
}


/*
 * Returns the value of the "defaultValues" element of this
 * ListOfLocalRenderInformation.
 */
DefaultValues*
ListOfLocalRenderInformation::getDefaultValues()
{
  return mDefaultValues;
}


/*
 * Predicate returning @c true if this ListOfLocalRenderInformation's
 * "defaultValues" element is set.
 */
bool
ListOfLocalRenderInformation::isSetDefaultValues() const
{
  return (mDefaultValues != NULL);
}


/*
 * Sets the value of the "defaultValues" element of this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::setDefaultValues(const DefaultValues*
  defaultValues)
{
  if (mDefaultValues == defaultValues)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (defaultValues == NULL)
  {
    delete mDefaultValues;
    mDefaultValues = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mDefaultValues;
    mDefaultValues = (defaultValues != NULL) ? defaultValues->clone() : NULL;
    if (mDefaultValues != NULL)
    {
      mDefaultValues->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DefaultValues object, adds it to this
 * ListOfLocalRenderInformation object and returns the DefaultValues object
 * created.
 */
DefaultValues*
ListOfLocalRenderInformation::createDefaultValues()
{
  if (mDefaultValues != NULL)
  {
    delete mDefaultValues;
  }

  RENDER_CREATE_NS(renderns, getSBMLNamespaces());
  mDefaultValues = new DefaultValues(renderns);

  delete renderns;

  connectToChild();

  return mDefaultValues;
}


/*
 * Unsets the value of the "defaultValues" element of this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::unsetDefaultValues()
{
  delete mDefaultValues;
  mDefaultValues = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Get a LocalRenderInformation from the ListOfLocalRenderInformation.
 */
LocalRenderInformation*
ListOfLocalRenderInformation::get(unsigned int n)
{
  return static_cast<LocalRenderInformation*>(ListOf::get(n));
}


/*
 * Get a LocalRenderInformation from the ListOfLocalRenderInformation.
 */
const LocalRenderInformation*
ListOfLocalRenderInformation::get(unsigned int n) const
{
  return static_cast<const LocalRenderInformation*>(ListOf::get(n));
}


/*
 * Get a LocalRenderInformation from the ListOfLocalRenderInformation based on
 * its identifier.
 */
LocalRenderInformation*
ListOfLocalRenderInformation::get(const std::string& sid)
{
  return const_cast<LocalRenderInformation*>(static_cast<const
    ListOfLocalRenderInformation&>(*this).get(sid));
}


/*
 * Get a LocalRenderInformation from the ListOfLocalRenderInformation based on
 * its identifier.
 */
const LocalRenderInformation*
ListOfLocalRenderInformation::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<LocalRenderInformation>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    LocalRenderInformation*> (*result);
}


/*
 * Removes the nth LocalRenderInformation from this
 * ListOfLocalRenderInformation and returns a pointer to it.
 */
LocalRenderInformation*
ListOfLocalRenderInformation::remove(unsigned int n)
{
  return static_cast<LocalRenderInformation*>(ListOf::remove(n));
}


/*
 * Removes the LocalRenderInformation from this ListOfLocalRenderInformation
 * based on its identifier and returns a pointer to it.
 */
LocalRenderInformation*
ListOfLocalRenderInformation::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<LocalRenderInformation>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <LocalRenderInformation*> (item);
}


/*
 * Adds a copy of the given LocalRenderInformation to this
 * ListOfLocalRenderInformation.
 */
int
ListOfLocalRenderInformation::addLocalRenderInformation(const
  LocalRenderInformation* lri)
{
  if (lri == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (lri->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != lri->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != lri->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(lri)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(lri);
  }
}


/*
 * Get the number of LocalRenderInformation objects in this
 * ListOfLocalRenderInformation.
 */
unsigned int
ListOfLocalRenderInformation::getNumLocalRenderInformation() const
{
  return size();
}


/*
 * Creates a new LocalRenderInformation object, adds it to this
 * ListOfLocalRenderInformation object and returns the LocalRenderInformation
 * object created.
 */
LocalRenderInformation*
ListOfLocalRenderInformation::createLocalRenderInformation()
{
  LocalRenderInformation* lri = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    lri = new LocalRenderInformation(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (lri != NULL)
  {
    appendAndOwn(lri);
  }

  return lri;
}


/*
 * Returns the XML element name of this ListOfLocalRenderInformation object.
 */
const std::string&
ListOfLocalRenderInformation::getElementName() const
{
  static const string name = "listOfRenderInformation";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfLocalRenderInformation object.
 */
int
ListOfLocalRenderInformation::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfLocalRenderInformation object.
 */
int
ListOfLocalRenderInformation::getItemTypeCode() const
{
  return SBML_RENDER_LOCALRENDERINFORMATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ListOfLocalRenderInformation object have been set.
 */
bool
ListOfLocalRenderInformation::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
ListOfLocalRenderInformation::writeElements(XMLOutputStream& stream) const
{
  ListOf::writeElements(stream);

  if (isSetDefaultValues() == true)
  {
    mDefaultValues->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
ListOfLocalRenderInformation::connectToChild()
{
  ListOf::connectToChild();

  if (mDefaultValues != NULL)
  {
    mDefaultValues->connectToParent(this);
  }
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
ListOfLocalRenderInformation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDefaultValues != NULL)
  {
    if (mDefaultValues->getId() == id)
    {
      return mDefaultValues;
    }

    obj = mDefaultValues->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return ListOf::getElementBySId(id);
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
ListOfLocalRenderInformation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDefaultValues != NULL)
  {
    if (mDefaultValues->getMetaId() == metaid)
    {
      return mDefaultValues;
    }

    obj = mDefaultValues->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return ListOf::getElementByMetaId(metaid);
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
ListOfLocalRenderInformation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = ListOf::getAllElements(filter);

  ADD_FILTERED_POINTER(ret, sublist, mDefaultValues, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



XMLNode ListOfLocalRenderInformation::toXML() const
{
  return getXmlNodeForSBase(this);
}


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new LocalRenderInformation in this ListOfLocalRenderInformation
 */
SBase*
ListOfLocalRenderInformation::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "renderInformation")
  {
    object = new LocalRenderInformation(renderns);
    appendAndOwn(object);
  }

  if (name == "defaultValues")
  {
    DefaultValues newDV(renderns);
    setDefaultValues(&newDV);
    object = getDefaultValues();
  }

  delete renderns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ListOfLocalRenderInformation::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  ListOf::addExpectedAttributes(attributes);

  attributes.add("versionMajor");

  attributes.add("versionMinor");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ListOfLocalRenderInformation::readAttributes(const XMLAttributes& attributes,
                                             const ExpectedAttributes&
                                               expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  ListOf::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render",
          RenderLayoutLOLocalRenderInformationAllowedAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderLayoutLOLocalRenderInformationAllowedCoreAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // majorVersion uint (use = "optional" )
  // 

  if (log)
  {
    numErrs = log->getNumErrors();
  }
  mIsSetMajorVersion = attributes.readInto("versionMajor", mMajorVersion);

  if ( mIsSetMajorVersion == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Render attribute 'versionMajor' from the "
        "<ListOfLocalRenderInformation> element must be an integer.";
      log->logPackageError("render", 
        RenderLayoutVersionMajorMustBeNonNegativeInteger, pkgVersion, level, version,
        message, getLine(), getColumn());
    }
  }

  // 
  // minorVersion uint (use = "optional" )
  // 

  if (log)
  {
    numErrs = log->getNumErrors();
  }
  mIsSetMinorVersion = attributes.readInto("versionMinor", mMinorVersion);

  if ( mIsSetMinorVersion == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Render attribute 'versionMinor' from the "
        "<ListOfLocalRenderInformation> element must be an integer.";
      log->logPackageError("render", 
        RenderLayoutVersionMinorMustBeNonNegativeInteger, pkgVersion, level, version,
        message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ListOfLocalRenderInformation::writeAttributes(XMLOutputStream& stream) const
{
  ListOf::writeAttributes(stream);

  if (isSetMajorVersion() == true)
  {
    stream.writeAttribute("versionMajor", getPrefix(), mMajorVersion);
  }

  if (isSetMinorVersion() == true)
  {
    stream.writeAttribute("versionMinor", getPrefix(), mMinorVersion);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Render package
 */
void
ListOfLocalRenderInformation::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(RenderExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(RenderExtension::getXmlnsL3V1V1(), prefix);
    }
  }
  else
  {
    xmlns.add(getURI(), getPrefix());
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "majorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
unsigned int
ListOfLocalRenderInformation_getMajorVersion(const ListOf_t * lo)
{
  return (lo != NULL) ? static_cast<const ListOfLocalRenderInformation*>(lo)->getMajorVersion() : SBML_INT_MAX;
}


/*
 * Returns the value of the "minorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
unsigned int
ListOfLocalRenderInformation_getMinorVersion(const ListOf_t * lo)
{
  return (lo != NULL) ? static_cast<const ListOfLocalRenderInformation*>(lo)->getMinorVersion() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "majorVersion" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ListOfLocalRenderInformation_isSetMajorVersion(const ListOf_t * lo)
{
  return (static_cast<const ListOfLocalRenderInformation*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfLocalRenderInformation*>(lo)->isSetMajorVersion()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ListOf_t's "minorVersion" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ListOfLocalRenderInformation_isSetMinorVersion(const ListOf_t * lo)
{
  return (static_cast<const ListOfLocalRenderInformation*>(lo) != NULL) ?
    static_cast<int>(static_cast<const
      ListOfLocalRenderInformation*>(lo)->isSetMinorVersion()) : 0;
}


/*
 * Sets the value of the "majorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfLocalRenderInformation_setMajorVersion(ListOf_t * lo,
                                             unsigned int majorVersion)
{
  return (static_cast<ListOfLocalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfLocalRenderInformation*>(lo)->setMajorVersion(majorVersion)
      : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "minorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfLocalRenderInformation_setMinorVersion(ListOf_t * lo,
                                             unsigned int minorVersion)
{
  return (static_cast<ListOfLocalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfLocalRenderInformation*>(lo)->setMinorVersion(minorVersion)
      : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "majorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfLocalRenderInformation_unsetMajorVersion(ListOf_t * lo)
{
  return (static_cast<ListOfLocalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfLocalRenderInformation*>(lo)->unsetMajorVersion() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "minorVersion" attribute of this ListOf_t.
 */
LIBSBML_EXTERN
int
ListOfLocalRenderInformation_unsetMinorVersion(ListOf_t * lo)
{
  return (static_cast<ListOfLocalRenderInformation*>(lo) != NULL) ?
    static_cast<ListOfLocalRenderInformation*>(lo)->unsetMinorVersion() :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Get a LocalRenderInformation_t from the ListOf_t.
 */
LIBSBML_EXTERN
LocalRenderInformation_t*
ListOfLocalRenderInformation_getLocalRenderInformation(ListOf_t* lo,
                                                       unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfLocalRenderInformation*>(lo)->get(n);
}


/*
 * Get a LocalRenderInformation_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
LocalRenderInformation_t*
ListOfLocalRenderInformation_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfLocalRenderInformation*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth LocalRenderInformation_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
LocalRenderInformation_t*
ListOfLocalRenderInformation_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfLocalRenderInformation*>(lo)->remove(n);
}


/*
 * Removes the LocalRenderInformation_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
LocalRenderInformation_t*
ListOfLocalRenderInformation_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfLocalRenderInformation*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


