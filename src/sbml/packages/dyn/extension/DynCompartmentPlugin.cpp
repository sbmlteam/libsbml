/**
 * @file:   DynCompartmentPlugin.cpp
 * @brief:  Implementation of the DynCompartmentPlugin class
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


#include <sbml/packages/dyn/extension/DynCompartmentPlugin.h>
#include <sbml/packages/dyn/validator/DynSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DynCompartmentPlugin
 */
DynCompartmentPlugin::DynCompartmentPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               DynPkgNamespaces* dynns) :
    SBasePlugin(uri, prefix, dynns)
  , mCboTerm ("")
  , mSpatialComponents (dynns)
{
}


/*
 * Copy constructor for DynCompartmentPlugin.
 */
DynCompartmentPlugin::DynCompartmentPlugin(const DynCompartmentPlugin& orig) :
    SBasePlugin(orig)
{
    mCboTerm  = orig.mCboTerm;
    mSpatialComponents  = orig.mSpatialComponents;
}


/*
 * Assignment operator for DynCompartmentPlugin.
 */
DynCompartmentPlugin& 
DynCompartmentPlugin::operator=(const DynCompartmentPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mCboTerm  = rhs.mCboTerm;
    mSpatialComponents  = rhs.mSpatialComponents;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DynCompartmentPlugin object.
 */
DynCompartmentPlugin* 
DynCompartmentPlugin::clone () const
{
  return new DynCompartmentPlugin(*this);
}


/*
 * Destructor for DynCompartmentPlugin.
 */
DynCompartmentPlugin::~DynCompartmentPlugin()
{
}


//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------

/*
 * create object
 */
SBase*
DynCompartmentPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    if (name == "listOfSpatialComponents" ) 
    { 
      object = &mSpatialComponents;

      if (targetPrefix.empty() == true) 
      { 
        mSpatialComponents.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
    } 

    delete dynns;
  } 

  return object; 
}


/*
 * write elements
 */
void
DynCompartmentPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumSpatialComponents() > 0) 
  { 
    mSpatialComponents.write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
DynCompartmentPlugin::hasRequiredElements () const
{
  bool allPresent = true; 

  // TO DO 

  return allPresent; 
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
DynCompartmentPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("cboTerm");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DynCompartmentPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBasePlugin::readAttributes(attributes, expectedAttributes);

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
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // cboTerm string   ( use = "optional" )
  //
  assigned = attributes.readInto("cboTerm", mCboTerm);

  if (assigned == true)
  {
    // check string is not empty

    if (mCboTerm.empty() == true)
    {
      logEmptyString(mCboTerm, getLevel(), getVersion(), getPackageVersion(), "<DynCompartmentPlugin>");
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
DynCompartmentPlugin::writeAttributes (XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

  if (isSetCboTerm() == true)
    stream.writeAttribute("cboTerm", getPrefix(), mCboTerm);

}


  /** @endcond doxygenLibsbmlInternal */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
DynCompartmentPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mSpatialComponents, filter);

  return ret;
}


/*
 * Returns the value of the "cboTerm" attribute of this DynCompartmentPlugin.
 */
const std::string&
DynCompartmentPlugin::getCboTerm() const
{
  return mCboTerm;
}


/*
 * Returns true/false if cboTerm is set.
 */
bool
DynCompartmentPlugin::isSetCboTerm() const
{
  return (mCboTerm.empty() == false);
}


/*
 * Sets cboTerm and returns value indicating success.
 */
int
DynCompartmentPlugin::setCboTerm(const std::string& cboTerm)
{
  if (&(cboTerm) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCboTerm = cboTerm;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets cboTerm and returns value indicating success.
 */
int
DynCompartmentPlugin::unsetCboTerm()
{
  mCboTerm.erase();

  if (mCboTerm.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the  "ListOfSpatialComponents" in this DynCompartmentPlugin object.
 */
const ListOfSpatialComponents*
DynCompartmentPlugin::getListOfSpatialComponents() const
{
  return &mSpatialComponents;
}


/*
 * Returns the  "ListOfSpatialComponents" in this DynCompartmentPlugin object.
 */
ListOfSpatialComponents*
DynCompartmentPlugin::getListOfSpatialComponents()
{
  return &mSpatialComponents;
}


/*
 * Removes the nth SpatialComponent from the ListOfSpatialComponents.
 */
SpatialComponent*
DynCompartmentPlugin::removeSpatialComponent(unsigned int n)
{
	return mSpatialComponents.remove(n);
}


/*
 * Removes the a SpatialComponent with given id from the ListOfSpatialComponents.
 */
SpatialComponent*
DynCompartmentPlugin::removeSpatialComponent(const std::string& sid)
{
	return mSpatialComponents.remove(sid);
}


/*
 * Return the nth SpatialComponent in the ListOfSpatialComponents within this DynCompartmentPlugin.
 */
SpatialComponent*
DynCompartmentPlugin::getSpatialComponent(unsigned int n)
{
	return mSpatialComponents.get(n);
}


/*
 * Return the nth SpatialComponent in the ListOfSpatialComponents within this DynCompartmentPlugin.
 */
const SpatialComponent*
DynCompartmentPlugin::getSpatialComponent(unsigned int n) const
{
	return mSpatialComponents.get(n);
}


/*
 * Return a SpatialComponent from the ListOfSpatialComponents by id.
 */
SpatialComponent*
DynCompartmentPlugin::getSpatialComponent(const std::string& sid)
{
	return mSpatialComponents.get(sid);
}


/*
 * Return a SpatialComponent from the ListOfSpatialComponents by id.
 */
const SpatialComponent*
DynCompartmentPlugin::getSpatialComponent(const std::string& sid) const
{
	return mSpatialComponents.get(sid);
}


/*
 * Adds a copy the given "SpatialComponent" to this DynCompartmentPlugin.
 *
 * @param sc; the SpatialComponent object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
DynCompartmentPlugin::addSpatialComponent(const SpatialComponent* sc)
{
  if (sc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    mSpatialComponents.append(sc);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of SpatialComponent objects in this DynCompartmentPlugin.
 *
 * @return the number of SpatialComponent objects in this DynCompartmentPlugin
 */
unsigned int
DynCompartmentPlugin::getNumSpatialComponents() const
{
  return mSpatialComponents.size();
}


/*
 * Creates a new SpatialComponent object, adds it to this DynCompartmentPlugins
 * DynCompartmentPlugin and returns the SpatialComponent object created. 
 *
 * @return a new SpatialComponent object instance
 *
 * @see addSpatialComponent(const SpatialComponent* sc)
 */
SpatialComponent*
DynCompartmentPlugin::createSpatialComponent()
{
  SpatialComponent* sc = NULL;

  try
  {
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    sc = new SpatialComponent(dynns);
    delete dynns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sc != NULL)
  {
    mSpatialComponents.appendAndOwn(sc);
  }

  return sc;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
DynCompartmentPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  {
    mSpatialComponents.setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
DynCompartmentPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (getNumSpatialComponents() > 0)
  {
    mSpatialComponents.connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
DynCompartmentPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (getNumSpatialComponents() > 0)
  {
    mSpatialComponents.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
DynCompartmentPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  for(unsigned int i = 0; i < getNumSpatialComponents(); i++)
  {
    getSpatialComponent(i)->accept(v);
  }

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


