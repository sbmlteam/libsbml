/**
 * @file    LayoutExtension.h
 * @brief   Definition of LayoutExtension, the core module of layout package. 
 * @author  Akiya Jouraku
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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

#ifndef LayoutExtension_h
#define LayoutExtension_h

#include <sbml/SBMLTypeCodes.h>
#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>

#ifdef __cplusplus

#include <vector>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN LayoutExtension : public SBMLExtension
{
public:

  //---------------------------------------------------------------
  //
  // Required class methods
  //
  //---------------------------------------------------------------

  /**
   * Returns the package name of this extension.
   */
  static const std::string& getPackageName ();

  /**
   * Returns the default SBML Level this extension.
   */
  static unsigned int getDefaultLevel();

  /**
   * Returns the default SBML Version this extension.
   */
  static unsigned int getDefaultVersion();

  /**
   * Returns the default SBML version this extension.
   */
  static unsigned int getDefaultPackageVersion();

  /**
   * Returns URI of supported versions of this package.
   */
  static const std::string&  getXmlnsL3V1V1();

  static const std::string&  getXmlnsL2();

  //
  // Other URI needed in this package (if any)
  //
  static const std::string&  getXmlnsXSI();

  //---------------------------------------------------------------


  /**
   * Constructor
   */
  LayoutExtension ();


  /**
   * Copy constructor.
   */
  LayoutExtension(const LayoutExtension&);


  /**
   * Destroy this object.
   */
  virtual ~LayoutExtension ();


  /**
   * Assignment operator for LayoutExtension.
   */
  LayoutExtension& operator=(const LayoutExtension&);


  /**
   * Creates and returns a deep copy of this LayoutExtension object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual LayoutExtension* clone () const;


  /**
   * Returns the name of this package ("layout")
   *
   * @pram the name of this package ("layout")
   */
  virtual const std::string& getName() const;


  /**
   * Returns the URI (namespace) of the package corresponding to the combination of 
   * the given sbml level, sbml version, and package version.
   * Empty string will be returned if no corresponding URI exists.
   *
   * @param sbmlLevel the level of SBML
   * @param sbmlVersion the version of SBML
   * @param pkgVersion the version of package
   *
   * @return a string of the package URI
   */
  virtual const std::string& getURI(unsigned int sbmlLevel, unsigned int sbmlVersion, 
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML level with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * This method takes a type code of layout package and returns a string representing
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /**
   * Returns an SBMLExtensionNamespaces<LayoutExtension> object whose alias type is 
   * LayoutPkgNamespace.
   * Null will be returned if the given uri is not defined in the layout package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return an LayoutPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in layout package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /** @cond doxygen-libsbml-internal */
  /**
   * Initializes layout extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in LayoutExtension.cpp
   *
   *        static SBMLExtensionRegister<LayoutExtension> layoutExtensionRegistry;
   *
   */

  static void init();

  /** @endcond doxygen-libsbml-internal */

};


// --------------------------------------------------------------------
//
// Required typedef definitions 
//
// LayoutPkgNamespaces is derived from the SBMLNamespaces class and
// used when creating an object of SBase derived classes defined in
// layout package.
//
// --------------------------------------------------------------------

//
// (NOTE) 
//
// SBMLExtensionNamespaces<LayoutExtension> must be instantiated
// in LayoutExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<LayoutExtension> LayoutPkgNamespaces; 

typedef enum
{
   SBML_LAYOUT_BOUNDINGBOX           = 100
 , SBML_LAYOUT_COMPARTMENTGLYPH      = 101
 , SBML_LAYOUT_CUBICBEZIER           = 102
 , SBML_LAYOUT_CURVE                 = 103
 , SBML_LAYOUT_DIMENSIONS            = 104
 , SBML_LAYOUT_GRAPHICALOBJECT       = 105
 , SBML_LAYOUT_LAYOUT                = 106   
 , SBML_LAYOUT_LINESEGMENT           = 107   
 , SBML_LAYOUT_POINT                 = 108    
 , SBML_LAYOUT_REACTIONGLYPH         = 109    
 , SBML_LAYOUT_SPECIESGLYPH          = 110    
 , SBML_LAYOUT_SPECIESREFERENCEGLYPH = 111
 , SBML_LAYOUT_TEXTGLYPH             = 112
} SBMLLayoutTypeCode_t;


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* LayoutExtension_h */
