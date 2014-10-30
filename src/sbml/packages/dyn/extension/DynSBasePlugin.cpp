/**
 * @file:   DynSBasePlugin.cpp
 * @brief:  Implementation of the DynSBasePlugin class
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


#include <sbml/packages/dyn/extension/DynSBasePlugin.h>
#include <sbml/packages/dyn/validator/DynSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DynSBasePlugin
 */
DynSBasePlugin::DynSBasePlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               DynPkgNamespaces* dynns) :
    SBasePlugin(uri, prefix, dynns)
  , mCboTerm ("")
{
}


/*
 * Copy constructor for DynSBasePlugin.
 */
DynSBasePlugin::DynSBasePlugin(const DynSBasePlugin& orig) :
    SBasePlugin(orig)
{
    mCboTerm  = orig.mCboTerm;
}


/*
 * Assignment operator for DynSBasePlugin.
 */
DynSBasePlugin& 
DynSBasePlugin::operator=(const DynSBasePlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mCboTerm  = rhs.mCboTerm;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DynSBasePlugin object.
 */
DynSBasePlugin* 
DynSBasePlugin::clone () const
{
  return new DynSBasePlugin(*this);
}


/*
 * Destructor for DynSBasePlugin.
 */
DynSBasePlugin::~DynSBasePlugin()
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
DynSBasePlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  //const std::string&      name   = stream.peek().getName();
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    DYN_CREATE_NS(dynns, getSBMLNamespaces());

    delete dynns;
  } 

  return object; 
}


/*
 * write elements
 */
void
DynSBasePlugin::writeElements (XMLOutputStream& stream) const
{
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
DynSBasePlugin::hasRequiredElements () const
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
DynSBasePlugin::addExpectedAttributes(ExpectedAttributes& attributes)
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
DynSBasePlugin::readAttributes (const XMLAttributes& attributes,
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
      logEmptyString(mCboTerm, getLevel(), getVersion(), getPackageVersion(), "<DynSBasePlugin>");
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
DynSBasePlugin::writeAttributes (XMLOutputStream& stream) const
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
DynSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  //List* sublist = NULL;


  return ret;
}


/*
 * Returns the value of the "cboTerm" attribute of this DynSBasePlugin.
 */
const std::string&
DynSBasePlugin::getCboTerm() const
{
  return mCboTerm;
}


/*
 * Returns true/false if cboTerm is set.
 */
bool
DynSBasePlugin::isSetCboTerm() const
{
  return (mCboTerm.empty() == false);
}


/*
 * Sets cboTerm and returns value indicating success.
 */
int
DynSBasePlugin::setCboTerm(const std::string& cboTerm)
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
DynSBasePlugin::unsetCboTerm()
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


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
DynSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

}


/*
 * Connect to parent.
 */
void
DynSBasePlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

}


/*
 * Enables the given package.
 */
void
DynSBasePlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
}


/*
 * Accept the SBMLVisitor.
 */
bool
DynSBasePlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


