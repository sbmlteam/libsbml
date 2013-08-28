/**
 * @file    RequiredElementsSBasePlugin.h
 * @brief   Definition of RequiredElementsSBasePlugin, the plugin class of
 *          requiredElements package for the Model element.
 * @author  
 *
 * $Id: RequiredElementsSBasePlugin.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/requiredElements/extension/RequiredElementsSBasePlugin.h $
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

#ifndef RequiredElementsSBasePlugin_h
#define RequiredElementsSBasePlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBase.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#ifdef __cplusplus

#include <iostream>
#include <string>

#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/req/extension/RequiredElementsExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN RequiredElementsSBasePlugin : public SBasePlugin
{
public:

  /**
   * Constructor
   */
	RequiredElementsSBasePlugin (const std::string &uri, const std::string &prefix, RequiredElementsPkgNamespaces *requiredElementsns);


  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  RequiredElementsSBasePlugin(const RequiredElementsSBasePlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~RequiredElementsSBasePlugin ();


  /**
   * Assignment operator for RequiredElementsSBasePlugin.
   */
  RequiredElementsSBasePlugin& operator=(const RequiredElementsSBasePlugin& orig);


  /**
   * Creates and returns a deep copy of this RequiredElementsSBasePlugin object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual RequiredElementsSBasePlugin* clone () const;

#ifndef SWIG

  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /**
   * Reads the attributes of corresponding package in SBMLDocument element.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /**
   * Writes the attributes of corresponding package in SBMLDocument element.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /** @endcond doxygenLibsbmlInternal */


#endif //SWIG
 

/**
  * ============================
  * Additional public functions
  * ============================
  */

  /**
   *
   * Returns the value of "mathOverridden" attribute of corresponding 
   * package in SBase element.
   *
   * @return the value of "mathOverridden" attribute of corresponding
   * package in SBase element.
   */
  virtual const std::string& getMathOverridden() const;
  

  /**
   *
   * Sets the value of "mathOverridden" attribute of corresponding package
   * in SBase element.
   *
   * @param value the value of "mathOverridden" attribute of corresponding 
   * package in SBase element.
   *
   * @return 
   */
  virtual int setMathOverridden(const std::string& pkgPrefix);

  /**
   *
   * Returns the bool value of "coreHasAlternateMath" attribute of corresponding 
   * package in SBase element.
   *
   * @return the bool value of "coreHasAlternateMath" attribute of corresponding
   * package in SBase element.
   */
  virtual bool getCoreHasAlternateMath() const;
  

  /**
   *
   * Sets the bool value of "coreHasAlternateMath" attribute of corresponding package
   * in SBase element.
   *
   * @param value the bool value of "coreHasAlternateMath" attribute of corresponding 
   * package in SBase element.
   *
   * @return 
   */
  virtual int setCoreHasAlternateMath(bool value);


protected:
  /** @cond doxygenLibsbmlInternal */

  /*-- data members --*/

  //
  // 'mathOverridden' - represents the namespace prefix(?) of a package that
  // redefines given element's math (element whose math is directly affected)
  //
  std::string          mMathOverridden;

  //
  // boolean that is set to T or F depending on whether an interpreter that
  // only understands the core package would have a workable/complete (could 
  // be different) version of the math for given element.
  //
  bool				   mCoreHasAlternateMath;

  /** @endcond doxygenLibsbmlInternal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* RequiredElementsSBasePlugin_h */
