/**
 * @file    RenderInformationBase.h
 * @brief Definition of the RenderInformationBase class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
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
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class RenderInformationBase
 * @sbmlbrief{render} Abstract base class for local and global rendering information.
 *
 * In the SBML Level&nbsp;3 Render package, local and global render
 * information representations share many attributes. These are implemented
 * in this abstract base class.  GlobalRenderInformation and
 * LocalRenderInformation are the classes that are derived from this base
 * class.
 *
 * All render information objects have the following things in common:
 *
 * @li a set of color definitions
 * @li a set of gradient definitions
 * @li a set of line endings
 *
 * In addition to those, they share attributes for background color and some
 * meta information as to which program created the render information etc.
 */

#ifndef RenderInformationBase_H__
#define RenderInformationBase_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ListOfColorDefinitions.h>
#include <sbml/packages/render/sbml/ListOfGradientDefinitions.h>
#include <sbml/packages/render/sbml/ListOfLineEndings.h>
#include <sbml/packages/render/sbml/ColorDefinition.h>
#include <sbml/packages/render/sbml/GradientBase.h>
#include <sbml/packages/render/sbml/LinearGradient.h>
#include <sbml/packages/render/sbml/RadialGradient.h>
#include <sbml/packages/render/sbml/LineEnding.h>
#include <sbml/xml/XMLNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class GlobalRenderInformation;
class LocalRenderInformation;

class LIBSBML_EXTERN RenderInformationBase : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mProgramName;
  std::string mProgramVersion;
  std::string mReferenceRenderInformation;
  std::string mBackgroundColor;
  ListOfColorDefinitions mColorDefinitions;
  ListOfGradientDefinitions mGradientBases;
  ListOfLineEndings mLineEndings;

  /** @endcond */

public:

  /**
   * Creates a new RenderInformationBase using the given SBML Level, Version
   * and &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * RenderInformationBase.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RenderInformationBase.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this RenderInformationBase.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderInformationBase(unsigned int level =
    RenderExtension::getDefaultLevel(),
                        unsigned int version =
                          RenderExtension::getDefaultVersion(),
                        unsigned int pkgVersion =
                          RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new RenderInformationBase using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderInformationBase(RenderPkgNamespaces *renderns);


  /**
   * Copy constructor for RenderInformationBase.
   *
   * @param orig the RenderInformationBase instance to copy.
   */
  RenderInformationBase(const RenderInformationBase& orig);


  /**
   * Assignment operator for RenderInformationBase.
   *
   * @param rhs the RenderInformationBase object whose values are to be used as
   * the basis of the assignment.
   */
  RenderInformationBase& operator=(const RenderInformationBase& rhs);


  /**
   * Creates and returns a deep copy of this RenderInformationBase object.
   *
   * @return a (deep) copy of this RenderInformationBase object.
   */
  virtual RenderInformationBase* clone() const;


  /**
   * Destructor for RenderInformationBase.
   */
  virtual ~RenderInformationBase();


  /**
   * Parses the XML information in the given node and sets the attributes.
   * This method should never be called by the user. It is only used to read render 
   * information from annotations.
   *
   * @param pNode the XMLNode object reference that describes the RenderinformationBase
   * object to be instantiated.
   */
  void parseXML(const XMLNode& pNode); 



#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a RenderInformationBase object
   * empty color definition, gradient definition
   * and line endings set.
   * For the object to be valid a valid background color value.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  RenderInformationBase(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED



  /**
   * Returns the value of the "id" attribute of this RenderInformationBase.
   *
   * @return the value of the "id" attribute of this RenderInformationBase as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this RenderInformationBase.
   *
   * @return the value of the "name" attribute of this RenderInformationBase as
   * a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "programName" attribute of this
   * RenderInformationBase.
   *
   * @return the value of the "programName" attribute of this
   * RenderInformationBase as a string.
   */
  const std::string& getProgramName() const;


  /**
   * Returns the value of the "programVersion" attribute of this
   * RenderInformationBase.
   *
   * @return the value of the "programVersion" attribute of this
   * RenderInformationBase as a string.
   */
  const std::string& getProgramVersion() const;


  /**
   * Returns the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase.
   * Returns the id of the referenced render information object.
   * Renderinformation objects can reference other render information objects
   * and information that is not found in the current render information is then
   * expected to be in the referenced render information object.
   *
   * Global render information objects can only reference other global 
   * render information objects, local render information objects can reference other local
   * render information objects from the same list of local render information or other
   * global render information.
   *
   * @return the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase as a string.
   */
  const std::string& getReferenceRenderInformationId() const;


  /**
   * Returns the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase.
   *
   * @return the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase as a string.
   */
  const std::string& getReferenceRenderInformation() const;


  /**
   * Returns the value of the "backgroundColor" attribute of this
   * RenderInformationBase.
   *
   * @return the value of the "backgroundColor" attribute of this
   * RenderInformationBase as a string.
   */
  const std::string& getBackgroundColor() const;


  /**
   * Predicate returning @c true if this RenderInformationBase's "id" attribute
   * is set.
   *
   * @return @c true if this RenderInformationBase's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this RenderInformationBase's "name"
   * attribute is set.
   *
   * @return @c true if this RenderInformationBase's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this RenderInformationBase's "programName"
   * attribute is set.
   *
   * @return @c true if this RenderInformationBase's "programName" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetProgramName() const;


  /**
   * Predicate returning @c true if this RenderInformationBase's
   * "programVersion" attribute is set.
   *
   * @return @c true if this RenderInformationBase's "programVersion" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetProgramVersion() const;


  /**
   * Predicate returning @c true if this RenderInformationBase's
   * "referenceRenderInformation" attribute is set.
   *
   * @return @c true if this RenderInformationBase's
   * "referenceRenderInformation" attribute has been set, otherwise @c false is
   * returned.
   */
  bool isSetReferenceRenderInformation() const;


  /**
   * Predicate returning @c true if this RenderInformationBase's
   * "backgroundColor" attribute is set.
   *
   * @return @c true if this RenderInformationBase's "backgroundColor"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetBackgroundColor() const;


  /**
   * Sets the value of the "id" attribute of this RenderInformationBase.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * Calling this function with @p id = @c NULL or an empty string is
   * equivalent to calling unsetId().
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this RenderInformationBase.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Sets the value of the "programName" attribute of this
   * RenderInformationBase.
   *
   * @param programName std::string& value of the "programName" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p programName = @c NULL or an empty string is
   * equivalent to calling unsetProgramName().
   */
  int setProgramName(const std::string& programName);


  /**
   * Sets the value of the "programVersion" attribute of this
   * RenderInformationBase.
   *
   * @param programVersion std::string& value of the "programVersion" attribute
   * to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p programVersion = @c NULL or an empty string
   * is equivalent to calling unsetProgramVersion().
   */
  int setProgramVersion(const std::string& programVersion);


  /**
   * Sets the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase.
   * The user has to make sure that render information referencing 
   * does not create loops.
   *
   * @param id the value for the "referenceRenderInformation" attribute
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  void setReferenceRenderInformationId(const std::string& id);


  /**
   * Sets the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase.
   *
   * @param referenceRenderInformation std::string& value of the
   * "referenceRenderInformation" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setReferenceRenderInformation(const std::string&
    referenceRenderInformation);


  /**
   * Sets the value of the "backgroundColor" attribute of this
   * RenderInformationBase.
   *
   * @param backgroundColor std::string& value of the "backgroundColor"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p backgroundColor = @c NULL or an empty string
   * is equivalent to calling unsetBackgroundColor().
   */
  int setBackgroundColor(const std::string& backgroundColor);


  /**
   * Unsets the value of the "id" attribute of this RenderInformationBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this RenderInformationBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "programName" attribute of this
   * RenderInformationBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetProgramName();


  /**
   * Unsets the value of the "programVersion" attribute of this
   * RenderInformationBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetProgramVersion();


  /**
   * Unsets the value of the "referenceRenderInformation" attribute of this
   * RenderInformationBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetReferenceRenderInformation();


  /**
   * Unsets the value of the "backgroundColor" attribute of this
   * RenderInformationBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBackgroundColor();


  /**
   * Returns the ListOfColorDefinitions from this RenderInformationBase.
   *
   * @return the ListOfColorDefinitions from this RenderInformationBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  const ListOfColorDefinitions* getListOfColorDefinitions() const;


  /**
   * Returns the ListOfColorDefinitions from this RenderInformationBase.
   *
   * @return the ListOfColorDefinitions from this RenderInformationBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  ListOfColorDefinitions* getListOfColorDefinitions();


  /**
   * Get a ColorDefinition from the RenderInformationBase.
   *
   * @param n an unsigned int representing the index of the ColorDefinition to
   * retrieve.
   *
   * @return the nth ColorDefinition in the ListOfColorDefinitions within this
   * RenderInformationBase or @c NULL if no such ColorDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  ColorDefinition* getColorDefinition(unsigned int n);


  /**
   * Get a ColorDefinition from the RenderInformationBase.
   *
   * @param n an unsigned int representing the index of the ColorDefinition to
   * retrieve.
   *
   * @return the nth ColorDefinition in the ListOfColorDefinitions within this
   * RenderInformationBase or @c NULL if no such ColorDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  const ColorDefinition* getColorDefinition(unsigned int n) const;


  /**
   * Get a ColorDefinition from the RenderInformationBase based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ColorDefinition to
   * retrieve.
   *
   * @return the ColorDefinition in the ListOfColorDefinitions within this
   * RenderInformationBase with the given @p sid or @c NULL if no such
   * ColorDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  ColorDefinition* getColorDefinition(const std::string& sid);


  /**
   * Get a ColorDefinition from the RenderInformationBase based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ColorDefinition to
   * retrieve.
   *
   * @return the ColorDefinition in the ListOfColorDefinitions within this
   * RenderInformationBase with the given @p sid or @c NULL if no such
   * ColorDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  const ColorDefinition* getColorDefinition(const std::string& sid) const;


  /**
   * Adds a copy of the given ColorDefinition to this RenderInformationBase.
   *
   * @param cd the ColorDefinition object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  int addColorDefinition(const ColorDefinition* cd);


  /**
   * Get the number of ColorDefinition objects in this RenderInformationBase.
   *
   * @return the number of ColorDefinition objects in this
   * RenderInformationBase.
   *
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  unsigned int getNumColorDefinitions() const;


  /**
   * Creates a new ColorDefinition object, adds it to this
   * RenderInformationBase object and returns the ColorDefinition object
   * created.
   *
   * @return a new ColorDefinition object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   * @see removeColorDefinition(unsigned int n)
   */
  ColorDefinition* createColorDefinition();


  /**
   * Removes the nth ColorDefinition from this RenderInformationBase and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the ColorDefinition to
   * remove.
   *
   * @return a pointer to the nth ColorDefinition in this
   * RenderInformationBase.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(const std::string& sid)
   */
  ColorDefinition* removeColorDefinition(unsigned int n);


  /**
   * Removes the ColorDefinition from this RenderInformationBase based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the ColorDefinition to
   * remove.
   *
   * @return the ColorDefinition in this RenderInformationBase based on the
   * identifier or NULL if no such ColorDefinition exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see getColorDefinition(const std::string& sid)
   * @see getColorDefinition(unsigned int n)
   * @see getNumColorDefinitions()
   * @see removeColorDefinition(unsigned int n)
   */
  ColorDefinition* removeColorDefinition(const std::string& sid);


  /**
   * Returns the ListOfGradientDefinitions from this RenderInformationBase.
   *
   * @return the ListOfGradientDefinitions from this RenderInformationBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGraidentBase(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  const ListOfGradientDefinitions* getListOfGradientDefinitions() const;


  /**
   * Returns the ListOfGradientDefinitions from this RenderInformationBase.
   *
   * @return the ListOfGradientDefinitions from this RenderInformationBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  ListOfGradientDefinitions* getListOfGradientDefinitions();


  /**
   * Get a GradientBase from the RenderInformationBase.
   *
   * @param n an unsigned int representing the index of the GradientBase to
   * retrieve.
   *
   * @return the nth GradientBase in the ListOfGradientDefinitions within this
   * RenderInformationBase or @c NULL if no such GradientBase exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  GradientBase* getGradientDefinition(unsigned int n);


  /**
   * Get a GradientBase from the RenderInformationBase.
   *
   * @param n an unsigned int representing the index of the GradientBase to
   * retrieve.
   *
   * @return the nth GradientBase in the ListOfGradientDefinitions within this
   * RenderInformationBase or @c NULL if no such GradientBase exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  const GradientBase* getGradientDefinition(unsigned int n) const;


  /**
   * Get a GradientBase from the RenderInformationBase based on its identifier.
   *
   * @param sid a string representing the identifier of the GradientBase to
   * retrieve.
   *
   * @return the GradientBase in the ListOfGradientDefinitions within this
   * RenderInformationBase with the given @p sid or @c NULL if no such
   * GradientBase exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  GradientBase* getGradientDefinition(const std::string& sid);


  /**
   * Get a GradientBase from the RenderInformationBase based on its identifier.
   *
   * @param sid a string representing the identifier of the GradientBase to
   * retrieve.
   *
   * @return the GradientBase in the ListOfGradientDefinitions within this
   * RenderInformationBase with the given @p sid or @c NULL if no such
   * GradientBase exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  const GradientBase* getGradientDefinition(const std::string& sid) const;


  /**
   * Adds a copy of the given GradientBase to this RenderInformationBase.
   *
   * @param gb the GradientBase object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  int addGradientDefinition(const GradientBase* gb);


  /**
   * Get the number of GradientBase objects in this RenderInformationBase.
   *
   * @return the number of GradientBase objects in this RenderInformationBase.
   *
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  unsigned int getNumGradientDefinitions() const;


  /**
   * Creates a new LinearGradient object, adds it to this RenderInformationBase
   * object and returns the LinearGradient object created.
   *
   * @return a new LinearGradient object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  LinearGradient* createLinearGradientDefinition();


  /**
   * Creates a new RadialGradient object, adds it to this RenderInformationBase
   * object and returns the RadialGradient object created.
   *
   * @return a new RadialGradient object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   * @see removeGradientDefinition(unsigned int n)
   */
  RadialGradient* createRadialGradientDefinition();


  /**
   * Removes the nth GradientBase from this RenderInformationBase and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the GradientBase to
   * remove.
   *
   * @return a pointer to the nth GradientBase in this RenderInformationBase.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(const std::string& sid)
   */
  GradientBase* removeGradientDefinition(unsigned int n);


  /**
   * Removes the GradientBase from this RenderInformationBase based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GradientBase to
   * remove.
   *
   * @return the GradientBase in this RenderInformationBase based on the
   * identifier or NULL if no such GradientBase exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientDefinition(const GradientBase* object)
   * @see createLinearGradientDefinition()
   * @see createRadialGradientDefinition()
   * @see getGradientDefinition(const std::string& sid)
   * @see getGradientDefinition(unsigned int n)
   * @see getNumGradientDefinitions()
   * @see removeGradientDefinition(unsigned int n)
   */
  GradientBase* removeGradientDefinition(const std::string& sid);


  /**
   * Returns the ListOfLineEndings from this RenderInformationBase.
   *
   * @return the ListOfLineEndings from this RenderInformationBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  const ListOfLineEndings* getListOfLineEndings() const;


  /**
   * Returns the ListOfLineEndings from this RenderInformationBase.
   *
   * @return the ListOfLineEndings from this RenderInformationBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  ListOfLineEndings* getListOfLineEndings();


  /**
   * Get a LineEnding from the RenderInformationBase.
   *
   * @param n an unsigned int representing the index of the LineEnding to
   * retrieve.
   *
   * @return the nth LineEnding in the ListOfLineEndings within this
   * RenderInformationBase or @c NULL if no such LineEnding exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  LineEnding* getLineEnding(unsigned int n);


  /**
   * Get a LineEnding from the RenderInformationBase.
   *
   * @param n an unsigned int representing the index of the LineEnding to
   * retrieve.
   *
   * @return the nth LineEnding in the ListOfLineEndings within this
   * RenderInformationBase or @c NULL if no such LineEnding exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  const LineEnding* getLineEnding(unsigned int n) const;


  /**
   * Get a LineEnding from the RenderInformationBase based on its identifier.
   *
   * @param sid a string representing the identifier of the LineEnding to
   * retrieve.
   *
   * @return the LineEnding in the ListOfLineEndings within this
   * RenderInformationBase with the given @p sid or @c NULL if no such
   * LineEnding exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  LineEnding* getLineEnding(const std::string& sid);


  /**
   * Get a LineEnding from the RenderInformationBase based on its identifier.
   *
   * @param sid a string representing the identifier of the LineEnding to
   * retrieve.
   *
   * @return the LineEnding in the ListOfLineEndings within this
   * RenderInformationBase with the given @p sid or @c NULL if no such
   * LineEnding exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  const LineEnding* getLineEnding(const std::string& sid) const;


  /**
   * Adds a copy of the given LineEnding to this RenderInformationBase.
   *
   * @param le the LineEnding object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  int addLineEnding(const LineEnding* le);


  /**
   * Get the number of LineEnding objects in this RenderInformationBase.
   *
   * @return the number of LineEnding objects in this RenderInformationBase.
   *
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  unsigned int getNumLineEndings() const;


  /**
   * Creates a new LineEnding object, adds it to this RenderInformationBase
   * object and returns the LineEnding object created.
   *
   * @return a new LineEnding object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   * @see removeLineEnding(unsigned int n)
   */
  LineEnding* createLineEnding();


  /**
   * Removes the nth LineEnding from this RenderInformationBase and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the LineEnding to
   * remove.
   *
   * @return a pointer to the nth LineEnding in this RenderInformationBase.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(const std::string& sid)
   */
  LineEnding* removeLineEnding(unsigned int n);


  /**
   * Removes the LineEnding from this RenderInformationBase based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the LineEnding to
   * remove.
   *
   * @return the LineEnding in this RenderInformationBase based on the
   * identifier or NULL if no such LineEnding exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see getLineEnding(const std::string& sid)
   * @see getLineEnding(unsigned int n)
   * @see getNumLineEndings()
   * @see removeLineEnding(unsigned int n)
   */
  LineEnding* removeLineEnding(const std::string& sid);


  /**
   * Predicate returning @c true if this abstract RenderInformationBase is of
   * type GlobalRenderInformation
   *
   * @return @c true if this abstract RenderInformationBase is of type
   * GlobalRenderInformation, @c false otherwise
   */
  virtual bool isGlobalRenderInformation() const;


  /**
   * Predicate returning @c true if this abstract RenderInformationBase is of
   * type LocalRenderInformation
   *
   * @return @c true if this abstract RenderInformationBase is of type
   * LocalRenderInformation, @c false otherwise
   */
  virtual bool isLocalRenderInformation() const;


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);

// render FIX ME
  /**
   * Returns the XML element name of this RenderInformationBase object.
   *
   * For RenderInformationBase, the XML element name is always
   * @c "renderInformationBase".
   *
   * @return the name of this element, i.e. @c "renderInformationBase".
   */
  //virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this RenderInformationBase object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_RENDERINFORMATION_BASE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * RenderInformationBase object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * RenderInformationBase have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the RenderInformationBase object are:
   * @li "id"
   */
  virtual bool hasRequiredAttributes() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this RenderInformationBase's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this RenderInformationBase's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this
   * RenderInformationBase.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this
   * RenderInformationBase.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this RenderInformationBase.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * RenderInformationBase.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this RenderInformationBase.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this RenderInformationBase.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new GlobalRenderInformation (RenderInformationBase_t) using the
 * given SBML Level, Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RenderInformationBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RenderInformationBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RenderInformationBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
RenderInformationBase_t *
RenderInformationBase_createGlobalRenderInformation(unsigned int level,
                                                    unsigned int version,
                                                    unsigned int pkgVersion);


/**
 * Creates a new LocalRenderInformation (RenderInformationBase_t) using the
 * given SBML Level, Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RenderInformationBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RenderInformationBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RenderInformationBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
RenderInformationBase_t *
RenderInformationBase_createLocalRenderInformation(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this RenderInformationBase_t object.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return a (deep) copy of this RenderInformationBase_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
RenderInformationBase_t*
RenderInformationBase_clone(const RenderInformationBase_t* rib);


/**
 * Frees this RenderInformationBase_t object.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
void
RenderInformationBase_free(RenderInformationBase_t* rib);


/**
 * Returns the value of the "id" attribute of this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this RenderInformationBase_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getId(const RenderInformationBase_t * rib);


/**
 * Returns the value of the "name" attribute of this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this RenderInformationBase_t as
 * a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getName(const RenderInformationBase_t * rib);


/**
 * Returns the value of the "programName" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose programName is
 * sought.
 *
 * @return the value of the "programName" attribute of this
 * RenderInformationBase_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getProgramName(const RenderInformationBase_t * rib);


/**
 * Returns the value of the "programVersion" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose programVersion is
 * sought.
 *
 * @return the value of the "programVersion" attribute of this
 * RenderInformationBase_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getProgramVersion(const RenderInformationBase_t * rib);


/**
 * Returns the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose
 * referenceRenderInformation is sought.
 *
 * @return the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getReferenceRenderInformation(const
  RenderInformationBase_t * rib);


/**
 * Returns the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose backgroundColor is
 * sought.
 *
 * @return the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getBackgroundColor(const RenderInformationBase_t * rib);


/**
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's "id"
 * attribute is set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) if this RenderInformationBase_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetId(const RenderInformationBase_t * rib);


/**
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's "name"
 * attribute is set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) if this RenderInformationBase_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetName(const RenderInformationBase_t * rib);


/**
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "programName" attribute is set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) if this RenderInformationBase_t's "programName"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetProgramName(const RenderInformationBase_t * rib);


/**
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "programVersion" attribute is set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) if this RenderInformationBase_t's "programVersion"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetProgramVersion(const RenderInformationBase_t * rib);


/**
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "referenceRenderInformation" attribute is set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) if this RenderInformationBase_t's
 * "referenceRenderInformation" attribute has been set, otherwise @c 0 (false)
 * is returned.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetReferenceRenderInformation(const
  RenderInformationBase_t * rib);


/**
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "backgroundColor" attribute is set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) if this RenderInformationBase_t's "backgroundColor"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetBackgroundColor(const RenderInformationBase_t *
  rib);


/**
 * Sets the value of the "id" attribute of this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling RenderInformationBase_unsetId().
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_setId(RenderInformationBase_t * rib, const char * id);


/**
 * Sets the value of the "name" attribute of this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling RenderInformationBase_unsetName().
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_setName(RenderInformationBase_t * rib,
                              const char * name);


/**
 * Sets the value of the "programName" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @param programName const char * value of the "programName" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p programName = @c NULL or an empty string is
 * equivalent to calling RenderInformationBase_unsetProgramName().
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_setProgramName(RenderInformationBase_t * rib,
                                     const char * programName);


/**
 * Sets the value of the "programVersion" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @param programVersion const char * value of the "programVersion" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p programVersion = @c NULL or an empty string is
 * equivalent to calling RenderInformationBase_unsetProgramVersion().
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_setProgramVersion(RenderInformationBase_t * rib,
                                        const char * programVersion);


/**
 * Sets the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @param referenceRenderInformation const char * value of the
 * "referenceRenderInformation" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_setReferenceRenderInformation(
                                                    RenderInformationBase_t *
                                                      rib,
                                                    const char *
                                                      referenceRenderInformation);


/**
 * Sets the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @param backgroundColor const char * value of the "backgroundColor" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p backgroundColor = @c NULL or an empty string
 * is equivalent to calling RenderInformationBase_unsetBackgroundColor().
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_setBackgroundColor(RenderInformationBase_t * rib,
                                         const char * backgroundColor);


/**
 * Unsets the value of the "id" attribute of this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetId(RenderInformationBase_t * rib);


/**
 * Unsets the value of the "name" attribute of this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetName(RenderInformationBase_t * rib);


/**
 * Unsets the value of the "programName" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetProgramName(RenderInformationBase_t * rib);


/**
 * Unsets the value of the "programVersion" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetProgramVersion(RenderInformationBase_t * rib);


/**
 * Unsets the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetReferenceRenderInformation(RenderInformationBase_t *
  rib);


/**
 * Unsets the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetBackgroundColor(RenderInformationBase_t * rib);


/**
 * Returns a ListOf_t * containing ColorDefinition_t objects from this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose
 * ListOfColorDefinitions is sought.
 *
 * @return the ListOfColorDefinitions from this RenderInformationBase_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see RenderInformationBase_addColorDefinition()
 * @see RenderInformationBase_createColorDefinition()
 * @see RenderInformationBase_getColorDefinitionById()
 * @see RenderInformationBase_getColorDefinition()
 * @see RenderInformationBase_getNumColorDefinitions()
 * @see RenderInformationBase_removeColorDefinitionById()
 * @see RenderInformationBase_removeColorDefinition()
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ListOf_t*
RenderInformationBase_getListOfColorDefinitions(RenderInformationBase_t* rib);


/**
 * Get a ColorDefinition_t from the RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the ColorDefinition_t to
 * retrieve.
 *
 * @return the nth ColorDefinition_t in the ListOfColorDefinitions within this
 * RenderInformationBase or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_getColorDefinition(RenderInformationBase_t* rib,
                                         unsigned int n);


/**
 * Get a ColorDefinition_t from the RenderInformationBase_t based on its
 * identifier.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param sid a string representing the identifier of the ColorDefinition_t to
 * retrieve.
 *
 * @return the ColorDefinition_t in the ListOfColorDefinitions within this
 * RenderInformationBase with the given @p sid or @c NULL if no such
 * ColorDefinition_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_getColorDefinitionById(RenderInformationBase_t* rib,
                                             const char *sid);


/**
 * Adds a copy of the given ColorDefinition_t to this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to which the
 * ColorDefinition_t should be added.
 *
 * @param cd the ColorDefinition_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_addColorDefinition(RenderInformationBase_t* rib,
                                         const ColorDefinition_t* cd);


/**
 * Get the number of ColorDefinition_t objects in this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to query.
 *
 * @return the number of ColorDefinition_t objects in this
 * RenderInformationBase_t.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
unsigned int
RenderInformationBase_getNumColorDefinitions(RenderInformationBase_t* rib);


/**
 * Creates a new ColorDefinition_t object, adds it to this
 * RenderInformationBase_t object and returns the ColorDefinition_t object
 * created.
 *
 * @param rib the RenderInformationBase_t structure to which the
 * ColorDefinition_t should be added.
 *
 * @return a new ColorDefinition_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_createColorDefinition(RenderInformationBase_t* rib);


/**
 * Removes the nth ColorDefinition_t from this RenderInformationBase_t and
 * returns a pointer to it.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the ColorDefinition_t to
 * remove.
 *
 * @return a pointer to the nth ColorDefinition_t in this
 * RenderInformationBase_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_removeColorDefinition(RenderInformationBase_t* rib,
                                            unsigned int n);


/**
 * Removes the ColorDefinition_t from this RenderInformationBase_t based on its
 * identifier and returns a pointer to it.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param sid a string representing the identifier of the ColorDefinition_t to
 * remove.
 *
 * @return the ColorDefinition_t in this RenderInformationBase_t based on the
 * identifier or NULL if no such ColorDefinition_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_removeColorDefinitionById(RenderInformationBase_t* rib,
                                                const char* sid);


/**
 * Returns a ListOf_t * containing GradientBase_t objects from this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose
 * ListOfGradientDefinitions is sought.
 *
 * @return the ListOfGradientDefinitions from this RenderInformationBase_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see RenderInformationBase_addGradientDefinition()
 * @see RenderInformationBase_createLinearGradientDefinition()
 * @see RenderInformationBase_createRadialGradientDefinition()
 * @see RenderInformationBase_getGradientDefinitionById()
 * @see RenderInformationBase_getGradientDefinition()
 * @see RenderInformationBase_getNumGradientDefinitions()
 * @see RenderInformationBase_removeGradientDefinitionById()
 * @see RenderInformationBase_removeGradientDefinition()
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ListOf_t*
RenderInformationBase_getListOfGradientDefinitions(RenderInformationBase_t*
  rib);


/**
 * Get a GradientBase_t from the RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientBase_t to
 * retrieve.
 *
 * @return the nth GradientBase_t in the ListOfGradientDefinitions within this
 * RenderInformationBase or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_getGradientDefinition(RenderInformationBase_t* rib,
                                            unsigned int n);


/**
 * Get a GradientBase_t from the RenderInformationBase_t based on its
 * identifier.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param sid a string representing the identifier of the GradientBase_t to
 * retrieve.
 *
 * @return the GradientBase_t in the ListOfGradientDefinitions within this
 * RenderInformationBase with the given @p sid or @c NULL if no such
 * GradientBase_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_getGradientBaseById(RenderInformationBase_t* rib,
                                          const char *sid);


/**
 * Adds a copy of the given GradientBase_t to this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to which the GradientBase_t
 * should be added.
 *
 * @param gb the GradientBase_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_addGradientDefinition(RenderInformationBase_t* rib,
                                            const GradientBase_t* gb);


/**
 * Get the number of GradientBase_t objects in this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to query.
 *
 * @return the number of GradientBase_t objects in this
 * RenderInformationBase_t.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
unsigned int
RenderInformationBase_getNumGradientDefinitions(RenderInformationBase_t* rib);


/**
 * Creates a new LinearGradient_t object, adds it to this
 * RenderInformationBase_t object and returns the LinearGradient_t object
 * created.
 *
 * @param rib the RenderInformationBase_t structure to which the
 * LinearGradient_t should be added.
 *
 * @return a new LinearGradient_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
LinearGradient_t*
RenderInformationBase_createLinearGradient(RenderInformationBase_t* rib);


/**
 * Creates a new RadialGradient_t object, adds it to this
 * RenderInformationBase_t object and returns the RadialGradient_t object
 * created.
 *
 * @param rib the RenderInformationBase_t structure to which the
 * RadialGradient_t should be added.
 *
 * @return a new RadialGradient_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
RadialGradient_t*
RenderInformationBase_createRadialGradient(RenderInformationBase_t* rib);


/**
 * Removes the nth GradientBase_t from this RenderInformationBase_t and returns
 * a pointer to it.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientBase_t to
 * remove.
 *
 * @return a pointer to the nth GradientBase_t in this RenderInformationBase_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_removeGradientDefinition(RenderInformationBase_t* rib,
                                               unsigned int n);


/**
 * Removes the GradientBase_t from this RenderInformationBase_t based on its
 * identifier and returns a pointer to it.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param sid a string representing the identifier of the GradientBase_t to
 * remove.
 *
 * @return the GradientBase_t in this RenderInformationBase_t based on the
 * identifier or NULL if no such GradientBase_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_removeGradientDefinitionById(RenderInformationBase_t* rib,
                                             const char* sid);


/**
 * Returns a ListOf_t * containing LineEnding_t objects from this
 * RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure whose ListOfLineEndings is
 * sought.
 *
 * @return the ListOfLineEndings from this RenderInformationBase_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see RenderInformationBase_addLineEnding()
 * @see RenderInformationBase_createLineEnding()
 * @see RenderInformationBase_getLineEndingById()
 * @see RenderInformationBase_getLineEnding()
 * @see RenderInformationBase_getNumLineEndings()
 * @see RenderInformationBase_removeLineEndingById()
 * @see RenderInformationBase_removeLineEnding()
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
ListOf_t*
RenderInformationBase_getListOfLineEndings(RenderInformationBase_t* rib);


/**
 * Get a LineEnding_t from the RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the LineEnding_t to
 * retrieve.
 *
 * @return the nth LineEnding_t in the ListOfLineEndings within this
 * RenderInformationBase or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_getLineEnding(RenderInformationBase_t* rib,
                                    unsigned int n);


/**
 * Get a LineEnding_t from the RenderInformationBase_t based on its identifier.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param sid a string representing the identifier of the LineEnding_t to
 * retrieve.
 *
 * @return the LineEnding_t in the ListOfLineEndings within this
 * RenderInformationBase with the given @p sid or @c NULL if no such
 * LineEnding_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_getLineEndingById(RenderInformationBase_t* rib,
                                        const char *sid);


/**
 * Adds a copy of the given LineEnding_t to this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to which the LineEnding_t
 * should be added.
 *
 * @param le the LineEnding_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_addLineEnding(RenderInformationBase_t* rib,
                                    const LineEnding_t* le);


/**
 * Get the number of LineEnding_t objects in this RenderInformationBase_t.
 *
 * @param rib the RenderInformationBase_t structure to query.
 *
 * @return the number of LineEnding_t objects in this RenderInformationBase_t.
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
unsigned int
RenderInformationBase_getNumLineEndings(RenderInformationBase_t* rib);


/**
 * Creates a new LineEnding_t object, adds it to this RenderInformationBase_t
 * object and returns the LineEnding_t object created.
 *
 * @param rib the RenderInformationBase_t structure to which the LineEnding_t
 * should be added.
 *
 * @return a new LineEnding_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_createLineEnding(RenderInformationBase_t* rib);


/**
 * Removes the nth LineEnding_t from this RenderInformationBase_t and returns a
 * pointer to it.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the LineEnding_t to
 * remove.
 *
 * @return a pointer to the nth LineEnding_t in this RenderInformationBase_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_removeLineEnding(RenderInformationBase_t* rib,
                                       unsigned int n);


/**
 * Removes the LineEnding_t from this RenderInformationBase_t based on its
 * identifier and returns a pointer to it.
 *
 * @param rib the RenderInformationBase_t structure to search.
 *
 * @param sid a string representing the identifier of the LineEnding_t to
 * remove.
 *
 * @return the LineEnding_t in this RenderInformationBase_t based on the
 * identifier or NULL if no such LineEnding_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_removeLineEndingById(RenderInformationBase_t* rib,
                                           const char* sid);


/**
 * Predicate returning @c 1 if this RenderInformationBase_t is of type
 * GlobalRenderInformation_t
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 if this RenderInformationBase_t is of type
 * GlobalRenderInformation_t, @c 0 otherwise
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isGlobalRenderInformation(const RenderInformationBase_t *
  rib);


/**
 * Predicate returning @c 1 if this RenderInformationBase_t is of type
 * LocalRenderInformation_t
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 if this RenderInformationBase_t is of type
 * LocalRenderInformation_t, @c 0 otherwise
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isLocalRenderInformation(const RenderInformationBase_t *
  rib);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderInformationBase_t object have been set.
 *
 * @param rib the RenderInformationBase_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * RenderInformationBase_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the RenderInformationBase_t object are:
 * @li "id"
 *
 * @memberof RenderInformationBase_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_hasRequiredAttributes(const RenderInformationBase_t *
  rib);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !RenderInformationBase_H__ */


