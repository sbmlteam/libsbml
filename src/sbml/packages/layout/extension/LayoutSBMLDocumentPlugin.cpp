/**
 * @file    LayoutSBMLDocumentPlugin.cpp
 * @brief   Implementation of LayoutSBMLDocumentPlugin, the plugin class of 
 *          layout package for the Model element.
 * @author  Akiya Jouraku
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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

#include <sbml/packages/layout/extension/LayoutSBMLDocumentPlugin.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>

#include <iostream>
#include <string>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * 
 */
LayoutSBMLDocumentPlugin::LayoutSBMLDocumentPlugin (const std::string &uri, 
                                                    const std::string &prefix,
                                                    LayoutPkgNamespaces *layoutns)
  : SBMLDocumentPlugin(uri,prefix,layoutns)
{
  mRequired = false;
  mIsSetRequired = true;
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
LayoutSBMLDocumentPlugin::LayoutSBMLDocumentPlugin(const LayoutSBMLDocumentPlugin& orig)
  : SBMLDocumentPlugin(orig)
{
  mRequired = false;
  mIsSetRequired = true;
}


LayoutSBMLDocumentPlugin& 
LayoutSBMLDocumentPlugin::operator=(const LayoutSBMLDocumentPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    mRequired = orig.mRequired;
    mIsSetRequired = orig.mIsSetRequired;
  }    

  return *this;
}


/*
 * Destroy this object.
 */
LayoutSBMLDocumentPlugin::~LayoutSBMLDocumentPlugin () {}


void 
LayoutSBMLDocumentPlugin::readAttributes (const XMLAttributes& attributes,
                                          const ExpectedAttributes& expectedAttributes)
{
  //If we're reading from a file, the file might erroneously not have set the 'required' flag:
  mIsSetRequired = false;
  SBMLDocumentPlugin::readAttributes(attributes, expectedAttributes);

  //Alternatively, it might have set the 'required' flag to be 'false':
  if (mIsSetRequired && mRequired==false) {
    std::ostringstream msg;
    msg << "Package '" << getPrefix() << 
      "' may not be set 'required=true', as there is no way to change the mathematical interpretation of the model using the constructs in the package.";
    //LS DEBUG:  'Not Schema Conformant' is a generic error code; we really need a better one here.
    getErrorLog()->logError(NotSchemaConformant, getLevel(), getVersion(), msg.str());
  }
}


int 
LayoutSBMLDocumentPlugin::setRequired(bool required)
{
  if ( mSBMLExt->getLevel(mURI) < 3) {
    // required attribute is not defined for SBML Level 2 .
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  if (required==true) {
    //Illegal to set the layout package to 'required', as it cannot change the math.
    return LIBSBML_OPERATION_FAILED;
  }
  mRequired = required;
  mIsSetRequired = true;
  return LIBSBML_OPERATION_SUCCESS;
}

int 
LayoutSBMLDocumentPlugin::unsetRequired()
{
  return LIBSBML_OPERATION_FAILED;
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
