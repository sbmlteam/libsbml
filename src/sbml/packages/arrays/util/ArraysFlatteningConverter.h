/**
 * @file    ArraysFlatteningConverter.h
 * @brief   Definition of a first flattening converter.
 * @author  Sarah M Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class ArraysFlatteningConverter
 * @sbmlbrief{arrays} "Flattens" a model, removing arrays.
 *
 * @htmlinclude libsbml-facility-only-warning.html
 *
 */


#ifndef ArraysFlatteningConverter_h
#define ArraysFlatteningConverter_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegister.h>

#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>
#include <sbml/SBMLTransforms.h>
#include <sbml/util/IdList.h>
#include <sbml/util/ElementFilter.h>

#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN

class VariableFilter : public ElementFilter
{
public:
  VariableFilter();

  VariableFilter(const SBase* parent);

  virtual ~VariableFilter();

  void setParentType(const SBase* parent);

  virtual bool filter(const SBase* element);

protected:

  int mParentType;
};

class ArraysMathFilter : public ElementFilter
{
public:
  ArraysMathFilter() { };

  virtual ~ArraysMathFilter() { };

  virtual bool filter(const SBase* element)
  {
    bool hasMath = false;
    // for the math filter we want elements that have 
    // dimensions and indices and a math element
    if (element->getMath() != NULL)
    {
      const ArraysSBasePlugin * plugin =
        static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));

      if (plugin != NULL)
      {
        if (plugin->getNumDimensions() > 0)
        {
          hasMath = true;
        }
      }
    }
    return hasMath;
  };
};

class ArraysChildFilter : public ElementFilter
{
public:
  ArraysChildFilter() { };

  virtual ~ArraysChildFilter() { };

  virtual bool filter(const SBase* element)
  {
    bool nonArraysElement = true;
    if (element->getPackageName() == "arrays")
    {
      nonArraysElement = false;
    }
    return nonArraysElement;
  };
};


class LIBSBML_EXTERN ArraysFlatteningConverter : public SBMLConverter
{
public:

  /** @cond doxygenLibsbmlInternal */
  /**
   * Register with the ConversionRegistry.
   */
  static void init();

  /** @endcond */


  /**
   * Creates a new ArraysFlatteningConverter object.
   */
  ArraysFlatteningConverter();


  /**
   * Copy constructor.
   *
   * This creates a copy of a ArraysFlatteningConverter object.
   *
   * @param orig the ArraysFlatteningConverter instance to copy.
   */
  ArraysFlatteningConverter(const ArraysFlatteningConverter& orig);


  /**
   * Creates and returns a deep copy of this ArraysFlatteningConverter.
   *
   * @return a (deep) copy of this ArraysFlatteningConverter.
   */
  virtual ArraysFlatteningConverter* clone() const;


  /**
   * Destroy this ArraysFlatteningConverter object.
   */
  virtual ~ArraysFlatteningConverter ();


  /**
   * Returns @c true if this converter matches the given properties.
   *
   * Given a ConversionProperties object @p props, this method checks that @p
   * props possesses an option value to enable the ArraysFlatteningConverter.  If
   * it does, this method returns @c true.
   *
   * @param props the properties to match.
   *
   * @return @c true if the properties @p props would match the necessary
   * properties for ArraysFlatteningConverter type of converter, @c false
   * otherwise.
   */
  virtual bool matchesProperties(const ConversionProperties &props) const;


  /**
   * Performs the conversion.
   *
   * This method causes ArraysFlatteningConverter to do the actual conversion
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
   * This method causes ArraysFlatteningConverter to do the actual conversion
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
   * the @em default property settings for ArraysFlatteningConverter.  It is
   * meant to be called in order to be able to programmatically discover all
   * the settings for the converter object.
   *
   * @return the ConversionProperties object describing the default properties
   * for this converter.
   *
   * @note Previously, ArraysFlatteningConverter also offered an @em
   * "ignorePackages" option, whose name proved to be confusing.  This option
   * has been deprecated and replaced by the @em "stripUnflattenablePackages"
   * option.
   */
  virtual ConversionProperties getDefaultProperties() const;


private:

  /** @cond doxygenLibsbmlInternal */
  void addDimensionToModelValues();

  void removeDimensionFromModelValues();

  bool expandVariableElement(const SBase* element, bool notMath);

  bool expandVariable(const SBase* element, bool notMath);

  bool expandNonDimensionedVariable(SBase* element);

  bool dealWithMathChild(SBase* element);

  bool dealWithChildObjects(SBase* parent, SBase* element, const Index* index);
  
  bool replaceSelector(ASTNode* math, bool& adjusted, const Index* index);

  unsigned int evaluateIndex(const Index* index);

  bool adjustMath(SBase* newElement, const Index* index);

  bool adjustIdentifiers(SBase* newElement);

  bool adjustReferencedAttribute(SBase* newElement, bool calcIndex=true);

  unsigned int getNumElements(const Dimension* dim);

  unsigned int getNumEntries(const ArraysSBasePlugin* plugin, const Model* model = NULL);

  int validateOriginalDocument();

  int validateFlatDocument(Model* flatmodel, unsigned int pkgVersion,
                           unsigned int level, unsigned int version);

  bool getPerformValidation() const;

  bool getArraySize(const SBase* element);

  void updateArrayEntry(unsigned int index);

  std::vector<unsigned int> mArraySize;
  unsigned int mNoDimensions;
  unsigned int mCurrentDimension;
  std::vector<unsigned int> mArrayEntry;

  IdList mDimensionIndex;

  SBMLTransforms::IdValueMap mValues;
  unsigned int mValuesSize;

  bool isPopulatedValueMap();

  SBMLTransforms::IdValueMap getValueMap();

  void clearValueMap();

  bool populateValueMap();

  void viewArray(std::vector<unsigned int> arrayE)
  {
    for (unsigned int i = 0; i < arrayE.size(); ++i)
    {
      std::cout << i << ": " << arrayE.at(i) << " ";
    }
    std::cout << "\n";
  };

  /** @endcond */

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* ArraysFlatteningConverter_h*/

