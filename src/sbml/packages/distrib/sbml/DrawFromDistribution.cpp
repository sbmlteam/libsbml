/**
 * @file:   DrawFromDistribution.cpp
 * @brief:  Implementation of the DrawFromDistribution class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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


#include <sbml/packages/distrib/sbml/DrawFromDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DrawFromDistribution with the given level, version, and package version.
 */
DrawFromDistribution::DrawFromDistribution (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mDistribInputs (level, version, pkgVersion)
   ,mUncertML (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new DrawFromDistribution with the given DistribPkgNamespaces object.
 */
DrawFromDistribution::DrawFromDistribution (DistribPkgNamespaces* distribns)
  : SBase(distribns)
   ,mDistribInputs (distribns)
   ,mUncertML (NULL)
{
  // set the element namespace of this object
  setElementNamespace(distribns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(distribns);
}


/*
 * Copy constructor for DrawFromDistribution.
 */
DrawFromDistribution::DrawFromDistribution (const DrawFromDistribution& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mDistribInputs  = orig.mDistribInputs;
    if (orig.mUncertML != NULL)
    {
      mUncertML = orig.mUncertML->clone();
    }
    else
    {
      mUncertML = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for DrawFromDistribution.
 */
DrawFromDistribution&
DrawFromDistribution::operator=(const DrawFromDistribution& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mDistribInputs  = rhs.mDistribInputs;
    if (rhs.mUncertML != NULL)
    {
      mUncertML = rhs.mUncertML->clone();
    }
    else
    {
      mUncertML = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for DrawFromDistribution.
 */
DrawFromDistribution*
DrawFromDistribution::clone () const
{
  return new DrawFromDistribution(*this);
}


/*
 * Destructor for DrawFromDistribution.
 */
DrawFromDistribution::~DrawFromDistribution ()
{
  if (mUncertML != NULL)
    delete mUncertML;
}


/*
 * Returns the value of the "UncertML" attribute of this DrawFromDistribution.
 */
const UncertMLNode*
DrawFromDistribution::getUncertML() const
{
  return mUncertML;
}


/*
 * Returns true/false if UncertML is set.
 */
bool
DrawFromDistribution::isSetUncertML() const
{
  return (mUncertML != NULL);
}


/*
 * Sets UncertML and returns value indicating success.
 */
int
DrawFromDistribution::setUncertML(UncertMLNode* uncertML)
{
  if (mUncertML == uncertML)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  delete mUncertML;
  mUncertML = (uncertML != NULL) ? uncertML->clone() : NULL;
  //if (mUncertML != NULL)
  //{
  //  mUncertML->connectToParent(this);
  //}
  return LIBSBML_OPERATION_SUCCESS;
}

UncertMLNode*
DrawFromDistribution::createUncertML()
{
  delete mUncertML;
  mUncertML = new UncertMLNode();
  return mUncertML;
}


/*
 * Unsets UncertML and returns value indicating success.
 */
int
DrawFromDistribution::unsetUncertML()
{
  delete mUncertML;
  mUncertML = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the  "ListOfDistribInputs" in this DrawFromDistribution object.
 */
const ListOfDistribInputs*
DrawFromDistribution::getListOfDistribInputs() const
{
  return &mDistribInputs;
}


/*
 * Returns the  "ListOfDistribInputs" in this DrawFromDistribution object.
 */
ListOfDistribInputs*
DrawFromDistribution::getListOfDistribInputs()
{
  return &mDistribInputs;
}


/*
 * Removes the nth DistribInput from the ListOfDistribInputs.
 */
DistribInput*
DrawFromDistribution::removeDistribInput(unsigned int n)
{
  return mDistribInputs.remove(n);
}


/*
 * Removes the a DistribInput with given id from the ListOfDistribInputs.
 */
DistribInput*
DrawFromDistribution::removeDistribInput(const std::string& sid)
{
  return mDistribInputs.remove(sid);
}


/*
 * Return the nth DistribInput in the ListOfDistribInputs within this DrawFromDistribution.
 */
DistribInput*
DrawFromDistribution::getDistribInput(unsigned int n)
{
  return mDistribInputs.get(n);
}


/*
 * Return the nth DistribInput in the ListOfDistribInputs within this DrawFromDistribution.
 */
const DistribInput*
DrawFromDistribution::getDistribInput(unsigned int n) const
{
  return mDistribInputs.get(n);
}


/*
 * Return a DistribInput from the ListOfDistribInputs by id.
 */
DistribInput*
DrawFromDistribution::getDistribInput(const std::string& sid)
{
  return mDistribInputs.get(sid);
}


/*
 * Return a DistribInput from the ListOfDistribInputs by id.
 */
const DistribInput*
DrawFromDistribution::getDistribInput(const std::string& sid) const
{
  return mDistribInputs.get(sid);
}


/*
 * Adds a copy the given "DistribInput" to this DrawFromDistribution.
 */
int
DrawFromDistribution::addDistribInput(const DistribInput* di)
{
  if (di == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (di->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != di->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != di->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(di)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mDistribInputs.append(di);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of DistribInput objects in this DrawFromDistribution.
 */
unsigned int
DrawFromDistribution::getNumDistribInputs() const
{
  return mDistribInputs.size();
}


/*
 * Creates a new DistribInput object, adds it to this DrawFromDistributions
 */
DistribInput*
DrawFromDistribution::createDistribInput()
{
  DistribInput* di = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    di = new DistribInput(distribns);
    delete distribns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(di != NULL)
  {
    mDistribInputs.appendAndOwn(di);
  }

  return di;
}


List*
DrawFromDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  // ADD_FILTERED_ELEMENT(ret, sublist, mUncertML, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
DrawFromDistribution::getElementName () const
{
  static const string name = "drawFromDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
DrawFromDistribution::getTypeCode () const
{
  return SBML_DISTRIB_DRAW_FROM_DISTRIBUTION;
}


/*
 * check if all the required attributes are set
 */
bool
DrawFromDistribution::hasRequiredAttributes () const
{
  bool allPresent = true;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
DrawFromDistribution::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetUncertML() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
DrawFromDistribution::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumDistribInputs() > 0)
  {
    mDistribInputs.write(stream);
  }

  if (isSetUncertML() == true)
  {
     mUncertML->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
DrawFromDistribution::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
DrawFromDistribution::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mDistribInputs.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
DrawFromDistribution::connectToChild()
{
  SBase::connectToChild();
  mDistribInputs.connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
DrawFromDistribution::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mDistribInputs.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
DrawFromDistribution::createObject(XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  SBase* object = NULL;

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "listOfDistribInputs")
  {
    object = &mDistribInputs;
  }
  
  delete distribns;

  return object;
}

bool
DrawFromDistribution::readOtherXML(XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "UncertML")
  {
    delete mUncertML;
    XMLNode * xml = new XMLNode(stream);
    mUncertML = new UncertMLNode(xml);
    delete xml;
    return true;
  }

  return false;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
DrawFromDistribution::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DrawFromDistribution::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  //bool assigned = false;

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
DrawFromDistribution::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * 
 */
LIBSBML_EXTERN
DrawFromDistribution_t *
DrawFromDistribution_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion)
{
  return new DrawFromDistribution(level, version, pkgVersion);
}


/*
 * 
 */
LIBSBML_EXTERN
void
DrawFromDistribution_free(DrawFromDistribution_t * dfd)
{
  if (dfd != NULL)
    delete dfd;
}


/*
 *
 */
LIBSBML_EXTERN
DrawFromDistribution_t *
DrawFromDistribution_clone(DrawFromDistribution_t * dfd)
{
  if (dfd != NULL)
  {
    return static_cast<DrawFromDistribution_t*>(dfd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 *
 */
LIBSBML_EXTERN
int
DrawFromDistribution_hasRequiredAttributes(DrawFromDistribution_t * dfd)
{
  return (dfd != NULL) ? static_cast<int>(dfd->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


