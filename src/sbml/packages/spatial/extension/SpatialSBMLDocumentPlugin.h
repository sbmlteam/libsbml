/**
 * @file:   SpatialSBMLDocumentPlugin.h
 * @brief:  Implementation of the SpatialSBMLDocumentPlugin class
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


#ifndef SpatialSBMLDocumentPlugin_H__
#define SpatialSBMLDocumentPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpatialSBMLDocumentPlugin : public SBMLDocumentPlugin
{
public:

  /**
   * Creates a new SpatialSBMLDocumentPlugin
   */
  SpatialSBMLDocumentPlugin(const std::string& uri, const std::string& prefix, 
                                 SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor for SpatialSBMLDocumentPlugin.
   *
   * @param orig; the SpatialSBMLDocumentPlugin instance to copy.
   */
  SpatialSBMLDocumentPlugin(const SpatialSBMLDocumentPlugin& orig);


   /**
   * Assignment operator for SpatialSBMLDocumentPlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpatialSBMLDocumentPlugin& operator=(const SpatialSBMLDocumentPlugin& rhs);


   /**
   * Creates and returns a deep copy of this SpatialSBMLDocumentPlugin object.
   *
   * @return a (deep) copy of this SpatialSBMLDocumentPlugin object.
   */
  virtual SpatialSBMLDocumentPlugin* clone () const;


   /**
   * Destructor for SpatialSBMLDocumentPlugin.
   */
  virtual ~SpatialSBMLDocumentPlugin();


 #ifndef SWIG

  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the attributes of corresponding package in SBMLDocument element
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


#endif // SWIG

  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns boolean based on whether flattening of a comp model has been implemented.
   *
   * @returns @c true if flattening for composed models has been implemented,
   * false otherwise.
   */
  virtual bool isCompFlatteningImplemented() const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Check consistency function.
   */
  virtual unsigned int checkConsistency();


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* SpatialSBMLDocumentPlugin_H__ */


