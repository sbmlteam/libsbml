/**
 * @file    SpatialSpeciesRxnPlugin.h
 * @brief   Definition of SpatialSpeciesRxnPlugin, the plugin class of
 *          spatial package for the Species/Reaction element.
 * @author  
 *
 * $Id: SpatialSpeciesRxnPlugin.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialSpeciesRxnPlugin.h $
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

#ifndef SpatialSpeciesRxnPlugin_h
#define SpatialSpeciesRxnPlugin_h


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
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SpatialSpeciesRxnPlugin : public SBasePlugin
{
public:

  /**
   * Constructor
   */
	SpatialSpeciesRxnPlugin (const std::string &uri, const std::string &prefix, SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  SpatialSpeciesRxnPlugin(const SpatialSpeciesRxnPlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~SpatialSpeciesRxnPlugin ();


  /**
   * Assignment operator for SpatialSpeciesRxnPlugin.
   */
  SpatialSpeciesRxnPlugin& operator=(const SpatialSpeciesRxnPlugin& orig);


  /**
   * Creates and returns a deep copy of this SpatialSpeciesRxnPlugin object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual SpatialSpeciesRxnPlugin* clone () const;

#ifndef SWIG

  /** @cond doxygen-libsbml-internal */

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

  /** @endcond doxygen-libsbml-internal */


#endif //SWIG
 

/**
  * ============================
  * Additional public functions
  * ============================
  */

  /**
   *
   * Returns the value of "isSpatial" attribute of corresponding 
   * package in SBase element.
   *
   * @return the value of "isSpatial" attribute of corresponding
   * package in SBase element.
   */
  virtual bool getIsSpatial() const;
  

  /**
   *
   * Sets the value of "isSpatial" attribute of corresponding package
   * in SBase element.
   *
   * @param value the value of "isSpatial" attribute of corresponding 
   * package in SBase element.
   *
   * @return 
   */
  virtual int setIsSpatial(bool value);

  /**
   *
   * Returns the bool value of "isLocal" attribute of corresponding 
   * package in SBase element.
   *
   * @return the bool value of "isLocal" attribute of corresponding
   * package in SBase element.
   */
  virtual bool getIsLocal() const;
  

  /**
   *
   * Sets the bool value of "IsLocal" attribute of corresponding package
   * in SBase element.
   *
   * @param value the bool value of "IsLocal" attribute of corresponding 
   * package in SBase element.
   *
   * @return 
   */
  virtual int setIsLocal(bool value);


protected:
  /** @cond doxygen-libsbml-internal */

  /*-- data members --*/

  //
  // 'mathOverridden' - represents the namespace prefix(?) of a package that
  // redefines given element's math (element whose math is directly affected)
  //
  bool          mIsSpatial;	// for species

  //
  // boolean that is set to T or F depending on whether an interpreter that
  // only understands the core package would have a workable/complete (could 
  // be different) version of the math for given element.
  //
  bool			mIsLocal;	// for reaction

  /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* SpatialSpeciesRxnPlugin_h */
