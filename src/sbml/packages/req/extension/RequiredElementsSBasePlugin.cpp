/**
 * @file    RequiredElementsSBasePlugin.cpp
 * @brief   Implementation of RequiredElementsSBasePlugin, the plugin class of
 *          requiredElements package for the SBase element.
 * @author  
 *
 * $Id: RequiredElementsSBasePlugin.cpp 10673 2010-01-17 07:18:20Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/requiredElements/extension/RequiredElementsSBasePlugin.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <sbml/packages/req/extension/RequiredElementsSBasePlugin.h>

#include <iostream>

#ifdef __cplusplus

//using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
RequiredElementsSBasePlugin::RequiredElementsSBasePlugin (const std::string &uri, const std::string &prefix, RequiredElementsPkgNamespaces *requiredElementsns)
  : SBasePlugin(uri,prefix, requiredElementsns)
  , mMathOverridden("")
  , mCoreHasAlternateMath(true)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
RequiredElementsSBasePlugin::RequiredElementsSBasePlugin(const RequiredElementsSBasePlugin& orig)
  : SBasePlugin(orig)
  , mMathOverridden(orig.mMathOverridden)
  , mCoreHasAlternateMath(orig.mCoreHasAlternateMath)
{
}


/*
 * Destroy this object.
 */
RequiredElementsSBasePlugin::~RequiredElementsSBasePlugin () {}

/*
 * Assignment operator for RequiredElementsSBasePlugin.
 */
RequiredElementsSBasePlugin& 
RequiredElementsSBasePlugin::operator=(const RequiredElementsSBasePlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    this->mMathOverridden = orig.mMathOverridden;
    this->mCoreHasAlternateMath = orig.mCoreHasAlternateMath;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this RequiredElementsSBasePlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
RequiredElementsSBasePlugin* 
RequiredElementsSBasePlugin::clone () const
{
  return new RequiredElementsSBasePlugin(*this);  
}

// -----------------------------------------------
//
// virtual functions for attributes
//
// ------------------------------------------------


/** @cond doxygen-libsbml-internal */

/**
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
RequiredElementsSBasePlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  //
  // required attribute is not defined for SBML Level 2 or lesser.
  //
	if ( mSBMLExt->getLevel(mURI) > 2  && (!getMathOverridden().empty()) )
  {    
    attributes.add("mathOverridden");
	attributes.add("coreHasAlternateMath");
  }
}

/**
 *
 */
void 
RequiredElementsSBasePlugin::readAttributes (const XMLAttributes& attributes,
                                    const ExpectedAttributes& expectedAttributes)
{
  SBasePlugin::readAttributes(attributes, expectedAttributes);

  if ( mSBMLExt->getLevel(mURI) > 2 )
  {    
    XMLTriple tripleMathOverridden("mathOverridden", mURI, mPrefix);
    attributes.readInto(tripleMathOverridden, mMathOverridden, getErrorLog(), true, getLine(), getColumn());
    XMLTriple tripleAltMath("coreHasAlternateMath", mURI, mPrefix);
    attributes.readInto(tripleAltMath, mCoreHasAlternateMath, getErrorLog(), true, getLine(), getColumn());

  }
}


/**
 *
 */
void 
RequiredElementsSBasePlugin::writeAttributes (XMLOutputStream& stream) const
{
  //
  // required attribute is not defined for SBML Level 2 .
  //
  if ( mSBMLExt->getLevel(mURI) < 3)
    return;

  //cout << "[DEBUG] SBMLDocumentPlugin::writeAttributes() " << endl;
  if ( !getMathOverridden().empty() ) 
  {
	  XMLTriple tripleMathOverridden("mathOverridden", mURI, mPrefix);
	  stream.writeAttribute(tripleMathOverridden, mMathOverridden);

	  XMLTriple tripleAltMath("coreHasAlternateMath", mURI, mPrefix);
	  stream.writeAttribute(tripleAltMath, mCoreHasAlternateMath);
  }
}

/** @endcond doxygen-libsbml-internal */

/*
 *
 *  (EXTENSION) Additional public functions
 *
 */  

const std::string&
RequiredElementsSBasePlugin::getMathOverridden() const
{
	return mMathOverridden;
}
  

int 
RequiredElementsSBasePlugin::setMathOverridden(const std::string& pkgPrefix) 
{
	return SyntaxChecker::checkAndSetSId(pkgPrefix ,mMathOverridden);
}


bool
RequiredElementsSBasePlugin::getCoreHasAlternateMath() const
{
	return mCoreHasAlternateMath;
}
  
int 
RequiredElementsSBasePlugin::setCoreHasAlternateMath(bool value) 
{
  //
  // required attribute is not defined for SBML Level 2 or less.
  //
  if ( mSBMLExt->getLevel(mURI) < 3)
    return LIBSBML_UNEXPECTED_ATTRIBUTE;

  mCoreHasAlternateMath = value;
  return LIBSBML_OPERATION_SUCCESS;
}



LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
