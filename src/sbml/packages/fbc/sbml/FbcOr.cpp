/**
 * @file   FbcOr.cpp
 * @brief  Implementation of the FbcOr class
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
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
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/fbc/sbml/FbcAnd.h>
#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>



using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new FbcOr with the given level, version, and package version.
 */
FbcOr::FbcOr (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : FbcAssociation(level, version)
  , mAssociations (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new FbcOr with the given FbcPkgNamespaces object.
 */
FbcOr::FbcOr (FbcPkgNamespaces* fbcns)
  : FbcAssociation(fbcns)
  , mAssociations (fbcns)
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for FbcOr.
 */
FbcOr::FbcOr (const FbcOr& orig)
  : FbcAssociation(orig)
{
  mAssociations  = orig.mAssociations;

  // connect to child objects
  connectToChild();
}


/*
 * Assignment for FbcOr.
 */
FbcOr&
FbcOr::operator=(const FbcOr& rhs)
{
  if (&rhs != this)
  {
    FbcAssociation::operator=(rhs);
    mAssociations  = rhs.mAssociations;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for FbcOr.
 */
FbcOr*
FbcOr::clone () const
{
  return new FbcOr(*this);
}


/*
 * Destructor for FbcOr.
 */
FbcOr::~FbcOr ()
{
}


/*
 * Returns the  "ListOfFbcAssociations" in this FbcOr object.
 */
const ListOfFbcAssociations*
FbcOr::getListOfAssociations() const
{
  return &mAssociations;
}


/*
 * Returns the  "ListOfFbcAssociations" in this FbcOr object.
 */
ListOfFbcAssociations*
FbcOr::getListOfAssociations()
{
  return &mAssociations;
}


/*
 * Removes the nth Association from the ListOfFbcAssociations.
 */
FbcAssociation*
FbcOr::removeAssociation(unsigned int n)
{
  return mAssociations.remove(n);
}


/*
 * Removes the a Association with given id from the ListOfFbcAssociations.
 */
FbcAssociation*
FbcOr::removeAssociation(const std::string& sid)
{
  return mAssociations.remove(sid);
}


/*
 * Return the nth Association in the ListOfFbcAssociations within this FbcOr.
 */
FbcAssociation*
FbcOr::getAssociation(unsigned int n)
{
  return mAssociations.get(n);
}


/*
 * Return the nth Association in the ListOfFbcAssociations within this FbcOr.
 */
const FbcAssociation*
FbcOr::getAssociation(unsigned int n) const
{
  return mAssociations.get(n);
}


/*
 * Return a Association from the ListOfFbcAssociations by id.
 */
FbcAssociation*
FbcOr::getAssociation(const std::string& sid)
{
  return mAssociations.get(sid);
}


/*
 * Return a Association from the ListOfFbcAssociations by id.
 */
const FbcAssociation*
FbcOr::getAssociation(const std::string& sid) const
{
  return mAssociations.get(sid);
}


/*
 * Adds a copy the given "FbcAssociation" to this FbcOr.
 *
 * @param fa; the FbcAssociation object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * Adds a copy the given FbcAssociation to this FbcOr.
 */
int
FbcOr::addAssociation(const FbcAssociation* fa)
{
  if (fa == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (fa->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != fa->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != fa->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != fa->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(fa)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mAssociations.append(fa);
  }
}


/*
 * Get the number of FbcAssociation objects in this FbcOr.
 *
 * @return the number of FbcAssociation objects in this FbcOr
 */
unsigned int
FbcOr::getNumAssociations() const
{
  return mAssociations.size();
}


std::string 
FbcOr::toInfix(bool usingId) const
{
  if (mAssociations.size() == 0) return "";

  stringstream str;
  str << "(";
  str << mAssociations.get(0)->toInfix(usingId);
  for (size_t pos = 1; pos < mAssociations.size(); ++pos)
  {
    str << " or ";
    str << mAssociations.get((unsigned int)pos)->toInfix(usingId);
  }
  str << ")";
  return str.str();

}


/*
 * Creates a new FbcAnd object, adds it to this FbcOr's
 * ListOfFbcAssociations and returns the FbcAnd object created. 
 *
 * @return a new FbcAnd object instance
 *
 * @see addFbcAssociation(const FbcAssociation*)
 */
FbcAnd* 
FbcOr::createAnd()
{
  FbcAnd* fa = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    fa = new FbcAnd(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(fa != NULL)
  {
    mAssociations.appendAndOwn(fa);
  }

  return fa;
}


/*
 * Creates a new FbcOr object, adds it to this FbcOr's
 * ListOfFbcAssociations and returns the FbcOr object created. 
 *
 * @return a new FbcOr object instance
 *
 * @see addFbcAssociation(const FbcAssociation*)
 */
FbcOr* 
FbcOr::createOr()
{
  FbcOr* fo = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    fo = new FbcOr(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(fo != NULL)
  {
    mAssociations.appendAndOwn(fo);
  }

  return fo;
}


/*
 * Creates a new GeneProductRef object, adds it to this FbcOr's
 * ListOfFbcAssociations and returns the GeneProductRef object created. 
 *
 * @return a new GeneProductRef object instance
 *
 * @see addFbcAssociation(const FbcAssociation*)
 */
GeneProductRef* 
FbcOr::createGeneProductRef()
{
  GeneProductRef* gpr = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    gpr = new GeneProductRef(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(gpr != NULL)
  {
    mAssociations.appendAndOwn(gpr);
  }

  return gpr;
}


List*
FbcOr::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mAssociations, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
FbcOr::getElementName () const
{
  static const string name = "or";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
FbcOr::getTypeCode () const
{
  return SBML_FBC_OR;
}


/*
 * check if all the required attributes are set
 */
bool
FbcOr::hasRequiredAttributes () const
{
  bool allPresent = FbcAssociation::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
FbcOr::hasRequiredElements () const
{
  return getNumAssociations() >= 2;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
FbcOr::writeElements (XMLOutputStream& stream) const
{
  FbcAssociation::writeElements(stream);
  if (getNumAssociations() > 0)
  {
    mAssociations.writeElements(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
FbcOr::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */
  mAssociations.accept(v);

  v.leave(*this);

  return true;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
FbcOr::setSBMLDocument (SBMLDocument* d)
{
  FbcAssociation::setSBMLDocument(d);
  mAssociations.setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
FbcOr::connectToChild()
{
  FbcAssociation::connectToChild();

  mAssociations.connectToParent(this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
FbcOr::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  FbcAssociation::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mAssociations.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::getAttribute(const std::string& attributeName,
                    unsigned int& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::getAttribute(const std::string& attributeName,
                    std::string& value) const
{
  int return_value = FbcAssociation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this FbcOr's attribute "attributeName" is
 * set.
 */
bool
FbcOr::isSetAttribute(const std::string& attributeName) const
{
  bool value = FbcAssociation::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::setAttribute(const std::string& attributeName, int value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::setAttribute(const std::string& attributeName, double value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::setAttribute(const std::string& attributeName,
                    const std::string& value)
{
  int return_value = FbcAssociation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this FbcOr.
 */
int
FbcOr::unsetAttribute(const std::string& attributeName)
{
  int value = FbcAssociation::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this FbcOr.
 */
SBase*
FbcOr::createChildObject(const std::string& elementName)
{
  FbcAssociation* obj = NULL;

  if (elementName == "and")
  {
    return createAnd();
  }
  else if (elementName == "or")
  {
    return createOr();
  }
  else if (elementName == "geneProductRef")
  {
    return createGeneProductRef();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this FbcOr.
 */
unsigned int
FbcOr::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "association" || elementName == "and"
    || elementName == "or" || elementName == "geneProductRef")
  {
    return getNumAssociations();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this FbcOr.
 */
SBase*
FbcOr::getObject(const std::string& elementName, unsigned int index)
{
  FbcAssociation* obj = NULL;

  if (elementName == "association" || elementName == "and"
    || elementName == "or" || elementName == "geneProductRef")
  {
    return getAssociation(index);
  }

  return obj;
}

/** @endcond */
  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
FbcOr::createObject(XMLInputStream& stream)
{
  SBase* object = FbcAssociation::createObject(stream);

  //const string& name = stream.peek().getName();

  object = mAssociations.createObject(stream);
  connectToChild();


  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
FbcOr::addExpectedAttributes(ExpectedAttributes& attributes)
{
  FbcAssociation::addExpectedAttributes(attributes);

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FbcOr::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  FbcAssociation::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FbcOr::writeAttributes (XMLOutputStream& stream) const
{
  FbcAssociation::writeAttributes(stream);

}


  /** @endcond */


LIBSBML_EXTERN
FbcOr_t *
FbcOr_create(unsigned int level, unsigned int version,
             unsigned int pkgVersion)
{
  return new FbcOr(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
FbcOr_free(FbcOr_t * fo)
{
  if (fo != NULL)
    delete fo;
}


LIBSBML_EXTERN
FbcOr_t *
FbcOr_clone(FbcOr_t * fo)
{
  if (fo != NULL)
  {
    return static_cast<FbcOr_t*>(fo->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
FbcOr_addAssociation(FbcOr_t * fo, FbcAssociation_t * fa)
{
  return  (fo != NULL) ? fo->addAssociation(fa) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
FbcAnd_t *
FbcOr_createAnd(FbcOr_t * fo)
{
  return  (fo != NULL) ? fo->createAnd() : NULL;
}

LIBSBML_EXTERN
FbcOr_t *
FbcOr_createOr(FbcOr_t * fo)
{
  return  (fo != NULL) ? fo->createOr() : NULL;
}

LIBSBML_EXTERN
GeneProductRef_t *
FbcOr_createGeneProductRef(FbcOr_t * fo)
{
  return  (fo != NULL) ? fo->createGeneProductRef() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
FbcOr_getListOfFbcAssociations(FbcOr_t * fo)
{
  return  (fo != NULL) ? (ListOf_t *)fo->getListOfAssociations() : NULL;
}

LIBSBML_EXTERN
FbcAssociation_t *
FbcOr_getAssociation(FbcOr_t * fo, unsigned int n)
{
  return  (fo != NULL) ? fo->getAssociation(n) : NULL;
}

LIBSBML_EXTERN
FbcAssociation_t *
FbcOr_getAssociationById(FbcOr_t * fo, const char * sid)
{
  return  (fo != NULL) ? fo->getAssociation(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
FbcOr_getNumAssociations(FbcOr_t * fo)
{
  return  (fo != NULL) ? fo->getNumAssociations() : SBML_INT_MAX;
}

LIBSBML_EXTERN
FbcAssociation_t *
FbcOr_removeAssociation(FbcOr_t * fo, unsigned int n)
{
  return  (fo != NULL) ? fo->removeAssociation(n) : NULL;
}

LIBSBML_EXTERN
FbcAssociation_t *
FbcOr_removeAssociationById(FbcOr_t * fo, const char * sid)
{
  return  (fo != NULL) ? fo->removeAssociation(sid) : NULL;
}

LIBSBML_EXTERN
int
FbcOr_hasRequiredAttributes(const FbcOr_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
FbcOr_hasRequiredElements(const FbcOr_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */



