/**
 * @file    SpatialCompartmentPlugin.h
 * @brief   Definition of SpatialCompartmentPlugin, the plugin class of
 *          spatial package for the Parameter element.
 * @author  
 *
 * $Id: SpatialCompartmentPlugin.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialCompartmentPlugin.h $
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

#ifndef SpatialCompartmentPlugin_h
#define SpatialCompartmentPlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/Parameter.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/spatial/sbml/CompartmentMapping.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SpatialCompartmentPlugin : public SBasePlugin
{
public:

  /**
   * Constructor
   */
  SpatialCompartmentPlugin (const std::string &uri, const std::string &prefix,
                    SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  SpatialCompartmentPlugin(const SpatialCompartmentPlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~SpatialCompartmentPlugin ();


  /**
   * Assignment operator for SpatialCompartmentPlugin.
   */
  SpatialCompartmentPlugin& operator=(const SpatialCompartmentPlugin& orig);


  /**
   * Creates and returns a deep copy of this SpatialCompartmentPlugin object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual SpatialCompartmentPlugin* clone () const;


  // --------------------------------------------------------
  //
  // overridden virtual functions for reading/writing/checking 
  // elements
  //
  // --------------------------------------------------------

  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

 /**
   * Parses Spatial Extension of SBML Level 3
   */
 virtual bool readOtherXML (SBase* parentObject, XMLInputStream& stream);

 
  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /**
   * Checks if this plugin object has all the required elements.
   *
   * Subclasses should override this function if they have their specific
   * elements.
   *
   * @return true if this pugin object has all the required elements,
   * otherwise false will be returned.
   */
  virtual bool hasRequiredElements() const ;


  /** ------------------------------------------------------------------
   *
   *  Additional public functions
   *
   * ------------------------------------------------------------------
   */

   /**
   * Returns the CompartmentMapping object in this plugin object.
   *
   * @return CompartmentMapping object in this plugin object.
   */
  const CompartmentMapping* getCompartmentMapping () const;


  /**
   * Returns the CompartmentMapping in this plugin object.
   *
   * @return CompartmentMapping object in this plugin object.
   */
  CompartmentMapping* getCompartmentMapping ();
 
  // ---------------------------------------------------------
  //
  // virtual functions (internal implementation) which should
  // be overridden by subclasses.
  //
  // ---------------------------------------------------------

  /** @cond doxygen-libsbml-internal */

  /**
   * Sets the parent SBMLDocument of this plugin object.
   *
   * Subclasses which contain one or more SBase derived elements must
   * override this function.
   *
   * @param d the SBMLDocument object to use
   *
   * @see connectToParent
   * @see enablePackageInternal
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * Sets the parent SBML object of this plugin object to
   * this object and child elements (if any).
   * (Creates a child-parent relationship by this plugin object)
   *
   * This function is called when this object is created by
   * the parent element.
   * Subclasses must override this this function if they have one
   * or more child elements.Also, SBasePlugin::connectToParent()
   * must be called in the overridden function.
   *
   * @param sbase the SBase object to use
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  virtual void connectToParent (SBase *sbase);


  /**
   * Enables/Disables the given package with child elements in this plugin
   * object (if any).
   * (This is an internal implementation invoked from
   *  SBase::enablePakcageInternal() function)
   *
   * @note Subclasses in which one or more SBase derived elements are
   * defined must override this function.
   *
   * @see setSBMLDocument
   * @see connectToParent
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond doxygen-libsbml-internal */

protected:
  /** @cond doxygen-libsbml-internal */

  /*-- data members --*/

  CompartmentMapping mCompartmentMapping;

  /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* SpatialCompartmentPlugin_h */
