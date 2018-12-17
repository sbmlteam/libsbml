/**
 * @file    DistribAnnotationConverter.h
 * @brief   Definition of a first flattening converter.
 * @author  Sarah M Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 *
 * @class DistribAnnotationConverter
 * @sbmlbrief{arrays} Converts a model with custom annotations for distributions into distrib.
 *
 * @htmlinclude libsbml-facility-only-warning.html
 *
 */


#ifndef DistribAnnotationConverter_h
#define DistribAnnotationConverter_h

#include <sbml/math/ASTNode.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegister.h>

#include <set>

#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN DistribAnnotationConverter : public SBMLConverter
{
public:

  /** @cond doxygenLibsbmlInternal */
  /**
   * Register with the ConversionRegistry.
   */
  static void init();

  /** @endcond */


  /**
   * Creates a new DistribAnnotationConverter object.
   */
  DistribAnnotationConverter();


  /**
   * Copy constructor.
   *
   * This creates a copy of a DistribAnnotationConverter object.
   *
   * @param orig the DistribAnnotationConverter instance to copy.
   */
  DistribAnnotationConverter(const DistribAnnotationConverter& orig);


  /**
   * Creates and returns a deep copy of this DistribAnnotationConverter.
   *
   * @return a (deep) copy of this DistribAnnotationConverter.
   */
  virtual DistribAnnotationConverter* clone() const;


  /**
   * Destroy this DistribAnnotationConverter object.
   */
  virtual ~DistribAnnotationConverter ();


  /**
   * Returns @c true if this converter matches the given properties.
   *
   * Given a ConversionProperties object @p props, this method checks that @p
   * props possesses an option value to enable the DistribAnnotationConverter.  If
   * it does, this method returns @c true.
   *
   * @param props the properties to match.
   *
   * @return @c true if the properties @p props would match the necessary
   * properties for DistribAnnotationConverter type of converter, @c false
   * otherwise.
   */
  virtual bool matchesProperties(const ConversionProperties &props) const;


  /**
   * Performs the conversion.
   *
   * This method causes DistribAnnotationConverter to do the actual conversion
   * work, that is, to convert the SBMLDocument object set by
   * SBMLConverter::setDocument(@if java const SBMLDocument@endif) and with
   * the configuration options set by SBMLConverter::setProperties(@if java
   * const ConversionProperties@endif).
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   */
  virtual int convert();


  /** @cond doxygenLibsbmlInternal */
  /**
   * Performs the conversion.
   *
   * This method causes DistribAnnotationConverter to do the actual conversion
   * work, that is, to convert the SBMLDocument object set by
   * SBMLConverter::setDocument(@if java const SBMLDocument@endif) and with
   * the configuration options set by SBMLConverter::setProperties(@if java
   * const ConversionProperties@endif).
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   */
  virtual int performConversion();
  /** @endcond */


  /**
   * Returns the default properties of this converter.
   *
   * A given converter exposes one or more properties that can be adjusted in
   * order to influence the behavior of the converter.  This method returns
   * the @em default property settings for DistribAnnotationConverter.  It is
   * meant to be called in order to be able to programmatically discover all
   * the settings for the converter object.
   *
   * @return the ConversionProperties object describing the default properties
   * for this converter.
   *
   * @note Previously, DistribAnnotationConverter also offered an @em
   * "ignorePackages" option, whose name proved to be confusing.  This option
   * has been deprecated and replaced by the @em "stripUnflattenablePackages"
   * option.
   */
  virtual ConversionProperties getDefaultProperties() const;


private:
  bool convertModel(Model* model);
  void adjustMath(SBase* element, const std::string& function, ASTNodeType_t type);
  bool replaceAnnotatedFunctionWith(ASTNode * astn, const std::string & function, ASTNodeType_t type);

  std::set<std::string> mKeepFunctions;

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* DistribAnnotationConverter_h*/

