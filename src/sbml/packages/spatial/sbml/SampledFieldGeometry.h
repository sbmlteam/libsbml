/**
 * @file SampledFieldGeometry.h
 * @brief Definition of the SampledFieldGeometry class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class SampledFieldGeometry
 * @sbmlbrief{spatial} TODO:Definition of the SampledFieldGeometry class.
 */


#ifndef SampledFieldGeometry_H__
#define SampledFieldGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfSampledVolumes.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SampledFieldGeometry : public GeometryDefinition
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfSampledVolumes mSampledVolumes;
  std::string mSampledField;

  /** @endcond */

public:

  /**
   * Creates a new SampledFieldGeometry using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * SampledFieldGeometry.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * SampledFieldGeometry.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this SampledFieldGeometry.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SampledFieldGeometry(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                       unsigned int version =
                         SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledFieldGeometry using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SampledFieldGeometry(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for SampledFieldGeometry.
   *
   * @param orig the SampledFieldGeometry instance to copy.
   */
  SampledFieldGeometry(const SampledFieldGeometry& orig);


  /**
   * Assignment operator for SampledFieldGeometry.
   *
   * @param rhs the SampledFieldGeometry object whose values are to be used as
   * the basis of the assignment.
   */
  SampledFieldGeometry& operator=(const SampledFieldGeometry& rhs);


  /**
   * Creates and returns a deep copy of this SampledFieldGeometry object.
   *
   * @return a (deep) copy of this SampledFieldGeometry object.
   */
  virtual SampledFieldGeometry* clone() const;


  /**
   * Destructor for SampledFieldGeometry.
   */
  virtual ~SampledFieldGeometry();


  /**
   * Returns the value of the "sampledField" attribute of this
   * SampledFieldGeometry.
   *
   * @return the value of the "sampledField" attribute of this
   * SampledFieldGeometry as a string.
   */
  const std::string& getSampledField() const;


  /**
   * Predicate returning @c true if this SampledFieldGeometry's "sampledField"
   * attribute is set.
   *
   * @return @c true if this SampledFieldGeometry's "sampledField" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetSampledField() const;


  /**
   * Sets the value of the "sampledField" attribute of this
   * SampledFieldGeometry.
   *
   * @param sampledField std::string& value of the "sampledField" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSampledField(const std::string& sampledField);


  /**
   * Unsets the value of the "sampledField" attribute of this
   * SampledFieldGeometry.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSampledField();


  /**
   * Returns the ListOfSampledVolumes from this SampledFieldGeometry.
   *
   * @return the ListOfSampledVolumes from this SampledFieldGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  const ListOfSampledVolumes* getListOfSampledVolumes() const;


  /**
   * Returns the ListOfSampledVolumes from this SampledFieldGeometry.
   *
   * @return the ListOfSampledVolumes from this SampledFieldGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  ListOfSampledVolumes* getListOfSampledVolumes();


  /**
   * Get a SampledVolume from the SampledFieldGeometry.
   *
   * @param n an unsigned int representing the index of the SampledVolume to
   * retrieve.
   *
   * @return the nth SampledVolume in the ListOfSampledVolumes within this
   * SampledFieldGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  SampledVolume* getSampledVolume(unsigned int n);


  /**
   * Get a SampledVolume from the SampledFieldGeometry.
   *
   * @param n an unsigned int representing the index of the SampledVolume to
   * retrieve.
   *
   * @return the nth SampledVolume in the ListOfSampledVolumes within this
   * SampledFieldGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  const SampledVolume* getSampledVolume(unsigned int n) const;


  /**
   * Get a SampledVolume from the SampledFieldGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledVolume to
   * retrieve.
   *
   * @return the SampledVolume in the ListOfSampledVolumes within this
   * SampledFieldGeometry with the given @p sid or @c NULL if no such
   * SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  SampledVolume* getSampledVolume(const std::string& sid);


  /**
   * Get a SampledVolume from the SampledFieldGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledVolume to
   * retrieve.
   *
   * @return the SampledVolume in the ListOfSampledVolumes within this
   * SampledFieldGeometry with the given @p sid or @c NULL if no such
   * SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  const SampledVolume* getSampledVolume(const std::string& sid) const;


  /**
   * Get a SampledVolume from the SampledFieldGeometry based on the DomainType
   * to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * SampledVolume object to retrieve.
   *
   * @return the first SampledVolume in this SampledFieldGeometry based on the
   * given domainType attribute or NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const SampledVolume* getSampledVolumeByDomainType(const std::string& sid)
    const;


  /**
   * Get a SampledVolume from the SampledFieldGeometry based on the DomainType
   * to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * SampledVolume object to retrieve.
   *
   * @return the first SampledVolume in this SampledFieldGeometry based on the
   * given domainType attribute or NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  SampledVolume* getSampledVolumeByDomainType(const std::string& sid);


  /**
   * Adds a copy of the given SampledVolume to this SampledFieldGeometry.
   *
   * @param sv the SampledVolume object to add.
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
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  int addSampledVolume(const SampledVolume* sv);


  /**
   * Get the number of SampledVolume objects in this SampledFieldGeometry.
   *
   * @return the number of SampledVolume objects in this SampledFieldGeometry.
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  unsigned int getNumSampledVolumes() const;


  /**
   * Creates a new SampledVolume object, adds it to this SampledFieldGeometry
   * object and returns the SampledVolume object created.
   *
   * @return a new SampledVolume object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   * @see removeSampledVolume(unsigned int n)
   */
  SampledVolume* createSampledVolume();


  /**
   * Removes the nth SampledVolume from this SampledFieldGeometry and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the SampledVolume to
   * remove.
   *
   * @return a pointer to the nth SampledVolume in this SampledFieldGeometry.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(const std::string& sid)
   */
  SampledVolume* removeSampledVolume(unsigned int n);


  /**
   * Removes the SampledVolume from this SampledFieldGeometry based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the SampledVolume to
   * remove.
   *
   * @return the SampledVolume in this SampledFieldGeometry based on the
   * identifier or NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see getSampledVolume(const std::string& sid)
   * @see getSampledVolume(unsigned int n)
   * @see getNumSampledVolumes()
   * @see removeSampledVolume(unsigned int n)
   */
  SampledVolume* removeSampledVolume(const std::string& sid);


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this SampledFieldGeometry object.
   *
   * For SampledFieldGeometry, the XML element name is always
   * @c "sampledFieldGeometry".
   *
   * @return the name of this element, i.e. @c "sampledFieldGeometry".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this SampledFieldGeometry object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_SAMPLEDFIELDGEOMETRY, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * SampledFieldGeometry object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * SampledFieldGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the SampledFieldGeometry object are:
   * @li "sampledField"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * SampledFieldGeometry object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * SampledFieldGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the SampledFieldGeometry object are:
   */
  virtual bool hasRequiredElements() const;



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



  /** @cond doxygenLibsbmlInternal */

  /**
   * Updates the namespaces when setLevelVersion is used
   */
  virtual void updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * SampledFieldGeometry.
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
   * Gets the value of the "attributeName" attribute of this
   * SampledFieldGeometry.
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
   * Gets the value of the "attributeName" attribute of this
   * SampledFieldGeometry.
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
   * Gets the value of the "attributeName" attribute of this
   * SampledFieldGeometry.
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
   * Gets the value of the "attributeName" attribute of this
   * SampledFieldGeometry.
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
   * Predicate returning @c true if this SampledFieldGeometry's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this SampledFieldGeometry's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * SampledFieldGeometry.
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
   * SampledFieldGeometry.
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
   * SampledFieldGeometry.
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
   * SampledFieldGeometry.
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
   * SampledFieldGeometry.
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
   * SampledFieldGeometry.
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
   * SampledFieldGeometry.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this SampledFieldGeometry.
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
   * SampledFieldGeometry.
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
   * Returns the number of "elementName" in this SampledFieldGeometry.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this SampledFieldGeometry.
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
   * @return a List* pointer of pointers to all SBase child objects with any
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
 * Creates a new SampledFieldGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SampledFieldGeometry_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SampledFieldGeometry_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this SampledFieldGeometry_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this SampledFieldGeometry_t object.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return a (deep) copy of this SampledFieldGeometry_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t*
SampledFieldGeometry_clone(const SampledFieldGeometry_t* sfg);


/**
 * Frees this SampledFieldGeometry_t object.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
void
SampledFieldGeometry_free(SampledFieldGeometry_t* sfg);


/**
 * Returns the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure whose sampledField is
 * sought.
 *
 * @return the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
char *
SampledFieldGeometry_getSampledField(const SampledFieldGeometry_t * sfg);


/**
 * Predicate returning @c 1 (true) if this SampledFieldGeometry_t's
 * "sampledField" attribute is set.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return @c 1 (true) if this SampledFieldGeometry_t's "sampledField"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_isSetSampledField(const SampledFieldGeometry_t * sfg);


/**
 * Sets the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @param sampledField const char * value of the "sampledField" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_setSampledField(SampledFieldGeometry_t * sfg,
                                     const char * sampledField);


/**
 * Unsets the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_unsetSampledField(SampledFieldGeometry_t * sfg);


/**
 * Returns a ListOf_t * containing SampledVolume_t objects from this
 * SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure whose ListOfSampledVolumes
 * is sought.
 *
 * @return the ListOfSampledVolumes from this SampledFieldGeometry_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see SampledFieldGeometry_addSampledVolume()
 * @see SampledFieldGeometry_createSampledVolume()
 * @see SampledFieldGeometry_getSampledVolumeById()
 * @see SampledFieldGeometry_getSampledVolume()
 * @see SampledFieldGeometry_getNumSampledVolumes()
 * @see SampledFieldGeometry_removeSampledVolumeById()
 * @see SampledFieldGeometry_removeSampledVolume()
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
ListOf_t*
SampledFieldGeometry_getListOfSampledVolumes(SampledFieldGeometry_t* sfg);


/**
 * Get a SampledVolume_t from the SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledVolume_t to
 * retrieve.
 *
 * @return the nth SampledVolume_t in the ListOfSampledVolumes within this
 * SampledFieldGeometry.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_getSampledVolume(SampledFieldGeometry_t* sfg,
                                      unsigned int n);


/**
 * Get a SampledVolume_t from the SampledFieldGeometry_t based on its
 * identifier.
 *
 * @param sfg the SampledFieldGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledVolume_t to
 * retrieve.
 *
 * @return the SampledVolume_t in the ListOfSampledVolumes within this
 * SampledFieldGeometry with the given @p sid or @c NULL if no such
 * SampledVolume_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_getSampledVolumeById(SampledFieldGeometry_t* sfg,
                                          const char *sid);


/**
 * Get a SampledVolume_t from the SampledFieldGeometry_t based on the
 * DomainType to which it refers.
 *
 * @param sfg the SampledFieldGeometry_t structure to search.
 *
 * @param sid a string representing the "domainType" attribute of the
 * SampledVolume_t object to retrieve.
 *
 * @return the first SampledVolume_t in this SampledFieldGeometry_t based on
 * the given domainType attribute or NULL if no such SampledVolume_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_getSampledVolumeByDomainType(SampledFieldGeometry_t* sfg,
                                                  const char *sid);


/**
 * Adds a copy of the given SampledVolume_t to this SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure to which the SampledVolume_t
 * should be added.
 *
 * @param sv the SampledVolume_t object to add.
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
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_addSampledVolume(SampledFieldGeometry_t* sfg,
                                      const SampledVolume_t* sv);


/**
 * Get the number of SampledVolume_t objects in this SampledFieldGeometry_t.
 *
 * @param sfg the SampledFieldGeometry_t structure to query.
 *
 * @return the number of SampledVolume_t objects in this
 * SampledFieldGeometry_t.
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
unsigned int
SampledFieldGeometry_getNumSampledVolumes(SampledFieldGeometry_t* sfg);


/**
 * Creates a new SampledVolume_t object, adds it to this SampledFieldGeometry_t
 * object and returns the SampledVolume_t object created.
 *
 * @param sfg the SampledFieldGeometry_t structure to which the SampledVolume_t
 * should be added.
 *
 * @return a new SampledVolume_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_createSampledVolume(SampledFieldGeometry_t* sfg);


/**
 * Removes the nth SampledVolume_t from this SampledFieldGeometry_t and returns
 * a pointer to it.
 *
 * @param sfg the SampledFieldGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledVolume_t to
 * remove.
 *
 * @return a pointer to the nth SampledVolume_t in this SampledFieldGeometry_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_removeSampledVolume(SampledFieldGeometry_t* sfg,
                                         unsigned int n);


/**
 * Removes the SampledVolume_t from this SampledFieldGeometry_t based on its
 * identifier and returns a pointer to it.
 *
 * @param sfg the SampledFieldGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledVolume_t to
 * remove.
 *
 * @return the SampledVolume_t in this SampledFieldGeometry_t based on the
 * identifier or NULL if no such SampledVolume_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_removeSampledVolumeById(SampledFieldGeometry_t* sfg,
                                             const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SampledFieldGeometry_t object have been set.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SampledFieldGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SampledFieldGeometry_t object are:
 * @li "sampledField"
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredAttributes(const SampledFieldGeometry_t * sfg);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * SampledFieldGeometry_t object have been set.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * SampledFieldGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the SampledFieldGeometry_t object are:
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredElements(const SampledFieldGeometry_t * sfg);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !SampledFieldGeometry_H__ */


