/**
 * @file    SBMLNamespaces.h
 * @brief   SBMLNamespaces class to store level/version and namespace 
 * @author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL:  $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->
 *
 * @class SBMLNamespaces
 * @brief Class to store the SBML level, version and namespace information.
 *
 * @htmlinclude libsbml-not-sbml-warning.html
 *
 */

#ifndef SBMLNamespaces_h
#define SBMLNamespaces_h

#include <sbml/xml/XMLNamespaces.h>



#ifdef __cplusplus



/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

#define SBML_DEFAULT_LEVEL   2
#define SBML_DEFAULT_VERSION 4
#define SBML_XMLNS_L1        "http://www.sbml.org/sbml/level1"
#define SBML_XMLNS_L2V1      "http://www.sbml.org/sbml/level2"
#define SBML_XMLNS_L2V2      "http://www.sbml.org/sbml/level2/version2"
#define SBML_XMLNS_L2V3      "http://www.sbml.org/sbml/level2/version3"
#define SBML_XMLNS_L2V4      "http://www.sbml.org/sbml/level2/version4"

class LIBSBML_EXTERN SBMLNamespaces
{
public:

  /**
   * Creates a new SBMLNamespaces object from the given level
   * and version.
   *
   * The SBMLNamespaces object stores the SBML level, version
   * and appropriate namespace.
   *
   * @param level, the SBML level
   * @param version, the SBML version
   */
  SBMLNamespaces(unsigned int level = SBML_DEFAULT_LEVEL, 
                 unsigned int version = SBML_DEFAULT_VERSION);

  
  /**
   * Destroys this SBMLNamespaces object.
   */
  ~SBMLNamespaces();

  
  /**
   * Creates a new SBMLNamespaces object from the arguments
   * and returns it.
   *
   * @param level, the SBML level
   * @param version, the SBML version
   *
   * @return an SBMLNamespaces object that reflects the level
   * and version specified.
   */
  static SBMLNamespaces * getSBMLNamespaces(unsigned int level,
                                            unsigned int version);

  
  /**
   * Returns a string representing the SBML namespace for the 
   * level and version specified.
   *
   * @param level, the SBML level
   * @param version, the SBML version
   *
   * @return a string representing the SBML namespace that reflects the level
   * and version specified.
   */
  static std::string getSBMLNamespaceURI(unsigned int level,
                                            unsigned int version);
  
  
  /**
   * Get the SBML level of this SBMLNamespaces object.
   *
   * @return the SBML level of this SBMLNamespaces object.
   */
  unsigned int getLevel();



  /**
   * Get the SBML version of this SBMLNamespaces object.
   *
   * @return the SBML version of this SBMLNamespaces object.
   */
  unsigned int getVersion();



  /**
   * Get the XML namespaces of this SBMLNamespaces object.
   *
   * @return the XML namespaces of this SBMLNamespaces object.
   */
  XMLNamespaces * getNamespaces();


protected:  
  /** @cond doxygen-libsbml-internal */

  unsigned int mLevel;

  unsigned int mVersion;

  XMLNamespaces * mNamespaces;

  /** @endcond doxygen-libsbml-internal */
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


END_C_DECLS


#endif  /* !SWIG */
#endif  /* SBMLNamespaces_h */
