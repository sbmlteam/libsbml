/**
 * @file    LayoutSBMLDocumentPlugin.h
 * @brief   Definition of LayoutSBMLDocumentPlugin, the plugin class of 
 *          layout package for Model element.
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

#ifndef LayoutSBMLDocumentPlugin_h
#define LayoutSBMLDocumentPlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/layout/common/layoutfwd.h>

#ifdef __cplusplus

#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/packages/layout/sbml/Layout.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN LayoutSBMLDocumentPlugin : public SBMLDocumentPlugin
{
public:

  /**
   * Constructor
   */
  LayoutSBMLDocumentPlugin (const std::string &uri, const std::string &prefix,
                            LayoutPkgNamespaces* layoutns);


  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  LayoutSBMLDocumentPlugin(const LayoutSBMLDocumentPlugin& orig);


  /**
   * Assignment operator for SBMLDocumentPlugin.
   */
  LayoutSBMLDocumentPlugin& operator=(const LayoutSBMLDocumentPlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~LayoutSBMLDocumentPlugin ();

#ifndef SWIG

  /** @cond doxygen-libsbml-internal */

  /**
   * Reads the attributes of corresponding package in SBMLDocument element.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond */

#endif //SWIG

  /**
   *
   * Sets the bool value of "required" attribute of corresponding package
   * in SBMLDocument element.  The layout package is required to set this value
   * to 'false'; attempting to set it to 'true' will result in an error.
   *
   * @param value the bool value of "required" attribute of corresponding 
   * package in SBMLDocument element.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  virtual int setRequired(bool value);

  /**
   * Doesn't do anything:  it is illegal to unset the 'required' attribute in the layout package,
   * as it must always be set to 'false'.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetRequired();


};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* LayoutSBMLDocumentPlugin_h */
