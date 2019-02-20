/**
 * @file MixedGeometry.h
 * @brief Definition of the MixedGeometry class.
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
 * @class MixedGeometry
 * @sbmlbrief{spatial} TODO:Definition of the MixedGeometry class.
 */


#ifndef MixedGeometry_H__
#define MixedGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfGeometryDefinitions.h>
#include <sbml/packages/spatial/sbml/ListOfOrdinalMappings.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MixedGeometry : public GeometryDefinition
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfGeometryDefinitions mGeometryDefinitions;
  ListOfOrdinalMappings mOrdinalMappings;

  /** @endcond */

public:

  /**
   * Creates a new MixedGeometry using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * MixedGeometry.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * MixedGeometry.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this MixedGeometry.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  MixedGeometry(unsigned int level = SpatialExtension::getDefaultLevel(),
                unsigned int version = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion =
                  SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new MixedGeometry using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  MixedGeometry(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for MixedGeometry.
   *
   * @param orig the MixedGeometry instance to copy.
   */
  MixedGeometry(const MixedGeometry& orig);


  /**
   * Assignment operator for MixedGeometry.
   *
   * @param rhs the MixedGeometry object whose values are to be used as the
   * basis of the assignment.
   */
  MixedGeometry& operator=(const MixedGeometry& rhs);


  /**
   * Creates and returns a deep copy of this MixedGeometry object.
   *
   * @return a (deep) copy of this MixedGeometry object.
   */
  virtual MixedGeometry* clone() const;


  /**
   * Destructor for MixedGeometry.
   */
  virtual ~MixedGeometry();


  /**
   * Returns the ListOfGeometryDefinitions from this MixedGeometry.
   *
   * @return the ListOfGeometryDefinitions from this MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  const ListOfGeometryDefinitions* getListOfGeometryDefinitions() const;


  /**
   * Returns the ListOfGeometryDefinitions from this MixedGeometry.
   *
   * @return the ListOfGeometryDefinitions from this MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  ListOfGeometryDefinitions* getListOfGeometryDefinitions();


  /**
   * Get a GeometryDefinition from the MixedGeometry.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to retrieve.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within
   * this MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  GeometryDefinition* getGeometryDefinition(unsigned int n);


  /**
   * Get a GeometryDefinition from the MixedGeometry.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to retrieve.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within
   * this MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  const GeometryDefinition* getGeometryDefinition(unsigned int n) const;


  /**
   * Get a GeometryDefinition from the MixedGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to retrieve.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions within
   * this MixedGeometry with the given @p sid or @c NULL if no such
   * GeometryDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  GeometryDefinition* getGeometryDefinition(const std::string& sid);


  /**
   * Get a GeometryDefinition from the MixedGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to retrieve.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions within
   * this MixedGeometry with the given @p sid or @c NULL if no such
   * GeometryDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  const GeometryDefinition* getGeometryDefinition(const std::string& sid)
    const;


  /**
   * Adds a copy of the given GeometryDefinition to this MixedGeometry.
   *
   * @param gd the GeometryDefinition object to add.
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
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  int addGeometryDefinition(const GeometryDefinition* gd);


  /**
   * Get the number of GeometryDefinition objects in this MixedGeometry.
   *
   * @return the number of GeometryDefinition objects in this MixedGeometry.
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  unsigned int getNumGeometryDefinitions() const;


  /**
   * Creates a new AnalyticGeometry object, adds it to this MixedGeometry
   * object and returns the AnalyticGeometry object created.
   *
   * @return a new AnalyticGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  AnalyticGeometry* createAnalyticGeometry();


  /**
   * Creates a new SampledFieldGeometry object, adds it to this MixedGeometry
   * object and returns the SampledFieldGeometry object created.
   *
   * @return a new SampledFieldGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  SampledFieldGeometry* createSampledFieldGeometry();


  /**
   * Creates a new CSGeometry object, adds it to this MixedGeometry object and
   * returns the CSGeometry object created.
   *
   * @return a new CSGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  CSGeometry* createCSGeometry();


  /**
   * Creates a new ParametricGeometry object, adds it to this MixedGeometry
   * object and returns the ParametricGeometry object created.
   *
   * @return a new ParametricGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  ParametricGeometry* createParametricGeometry();


  /**
   * Creates a new MixedGeometry object, adds it to this MixedGeometry object
   * and returns the MixedGeometry object created.
   *
   * @return a new MixedGeometry object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   * @see removeGeometryDefinition(unsigned int n)
   */
  MixedGeometry* createMixedGeometry();


  /**
   * Removes the nth GeometryDefinition from this MixedGeometry and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to remove.
   *
   * @return a pointer to the nth GeometryDefinition in this MixedGeometry.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(const std::string& sid)
   */
  GeometryDefinition* removeGeometryDefinition(unsigned int n);


  /**
   * Removes the GeometryDefinition from this MixedGeometry based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to remove.
   *
   * @return the GeometryDefinition in this MixedGeometry based on the
   * identifier or NULL if no such GeometryDefinition exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addGeometryDefinition(const GeometryDefinition* object)
   * @see createGeometryDefinition()
   * @see getGeometryDefinition(const std::string& sid)
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   * @see removeGeometryDefinition(unsigned int n)
   */
  GeometryDefinition* removeGeometryDefinition(const std::string& sid);


  /**
   * Returns the ListOfOrdinalMappings from this MixedGeometry.
   *
   * @return the ListOfOrdinalMappings from this MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getOrdinalMapping(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  const ListOfOrdinalMappings* getListOfOrdinalMappings() const;


  /**
   * Returns the ListOfOrdinalMappings from this MixedGeometry.
   *
   * @return the ListOfOrdinalMappings from this MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getOrdinalMapping(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  ListOfOrdinalMappings* getListOfOrdinalMappings();


  /**
   * Get an OrdinalMapping from the MixedGeometry.
   *
   * @param n an unsigned int representing the index of the OrdinalMapping to
   * retrieve.
   *
   * @return the nth OrdinalMapping in the ListOfOrdinalMappings within this
   * MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  OrdinalMapping* getOrdinalMapping(unsigned int n);


  /**
   * Get an OrdinalMapping from the MixedGeometry.
   *
   * @param n an unsigned int representing the index of the OrdinalMapping to
   * retrieve.
   *
   * @return the nth OrdinalMapping in the ListOfOrdinalMappings within this
   * MixedGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  const OrdinalMapping* getOrdinalMapping(unsigned int n) const;


  /**
   * Get an OrdinalMapping from the MixedGeometry based on the
   * GeometryDefinition to which it refers.
   *
   * @param sid a string representing the "geometryDefinition" attribute of the
   * OrdinalMapping object to retrieve.
   *
   * @return the first OrdinalMapping in this MixedGeometry based on the given
   * geometryDefinition attribute or NULL if no such OrdinalMapping exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const OrdinalMapping* getOrdinalMappingByGeometryDefinition(const
    std::string& sid) const;


  /**
   * Get an OrdinalMapping from the MixedGeometry based on the
   * GeometryDefinition to which it refers.
   *
   * @param sid a string representing the "geometryDefinition" attribute of the
   * OrdinalMapping object to retrieve.
   *
   * @return the first OrdinalMapping in this MixedGeometry based on the given
   * geometryDefinition attribute or NULL if no such OrdinalMapping exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  OrdinalMapping* getOrdinalMappingByGeometryDefinition(const std::string&
    sid);


  /**
   * Adds a copy of the given OrdinalMapping to this MixedGeometry.
   *
   * @param om the OrdinalMapping object to add.
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
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getOrdinalMapping(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  int addOrdinalMapping(const OrdinalMapping* om);


  /**
   * Get the number of OrdinalMapping objects in this MixedGeometry.
   *
   * @return the number of OrdinalMapping objects in this MixedGeometry.
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getOrdinalMapping(unsigned int n)
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  unsigned int getNumOrdinalMappings() const;


  /**
   * Creates a new OrdinalMapping object, adds it to this MixedGeometry object
   * and returns the OrdinalMapping object created.
   *
   * @return a new OrdinalMapping object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see getOrdinalMapping(const std::string& sid)
   * @see getOrdinalMapping(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   * @see removeOrdinalMapping(unsigned int n)
   */
  OrdinalMapping* createOrdinalMapping();


  /**
   * Removes the nth OrdinalMapping from this MixedGeometry and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the OrdinalMapping to
   * remove.
   *
   * @return a pointer to the nth OrdinalMapping in this MixedGeometry.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addOrdinalMapping(const OrdinalMapping* object)
   * @see createOrdinalMapping()
   * @see getOrdinalMapping(const std::string& sid)
   * @see getOrdinalMapping(unsigned int n)
   * @see getNumOrdinalMappings()
   * @see removeOrdinalMapping(const std::string& sid)
   */
  OrdinalMapping* removeOrdinalMapping(unsigned int n);


  /**
   * Returns the XML element name of this MixedGeometry object.
   *
   * For MixedGeometry, the XML element name is always @c "mixedGeometry".
   *
   * @return the name of this element, i.e. @c "mixedGeometry".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this MixedGeometry object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_MIXEDGEOMETRY, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * MixedGeometry object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * MixedGeometry have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * MixedGeometry object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * MixedGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the MixedGeometry object are:
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
   * Gets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Gets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Gets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Gets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Gets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Predicate returning @c true if this MixedGeometry's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this MixedGeometry's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Sets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Sets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Sets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Sets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Unsets the value of the "attributeName" attribute of this MixedGeometry.
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
   * Creates and returns an new "elementName" object in this MixedGeometry.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this MixedGeometry.
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
   * MixedGeometry.
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
   * Returns the number of "elementName" in this MixedGeometry.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this MixedGeometry.
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
 * Creates a new MixedGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * MixedGeometry_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * MixedGeometry_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this MixedGeometry_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this MixedGeometry_t object.
 *
 * @param mg the MixedGeometry_t structure.
 *
 * @return a (deep) copy of this MixedGeometry_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
MixedGeometry_t*
MixedGeometry_clone(const MixedGeometry_t* mg);


/**
 * Frees this MixedGeometry_t object.
 *
 * @param mg the MixedGeometry_t structure.
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
void
MixedGeometry_free(MixedGeometry_t* mg);


/**
 * Returns a ListOf_t * containing GeometryDefinition_t objects from this
 * MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure whose ListOfGeometryDefinitions is
 * sought.
 *
 * @return the ListOfGeometryDefinitions from this MixedGeometry_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MixedGeometry_addGeometryDefinition()
 * @see MixedGeometry_createGeometryDefinition()
 * @see MixedGeometry_getGeometryDefinitionById()
 * @see MixedGeometry_getGeometryDefinition()
 * @see MixedGeometry_getNumGeometryDefinitions()
 * @see MixedGeometry_removeGeometryDefinitionById()
 * @see MixedGeometry_removeGeometryDefinition()
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
ListOf_t*
MixedGeometry_getListOfGeometryDefinitions(MixedGeometry_t* mg);


/**
 * Get a GeometryDefinition_t from the MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the GeometryDefinition_t
 * to retrieve.
 *
 * @return the nth GeometryDefinition_t in the ListOfGeometryDefinitions within
 * this MixedGeometry.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_getGeometryDefinition(MixedGeometry_t* mg, unsigned int n);


/**
 * Get a GeometryDefinition_t from the MixedGeometry_t based on its identifier.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the GeometryDefinition_t
 * to retrieve.
 *
 * @return the GeometryDefinition_t in the ListOfGeometryDefinitions within
 * this MixedGeometry with the given @p sid or @c NULL if no such
 * GeometryDefinition_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_getGeometryDefinitionById(MixedGeometry_t* mg, const char *sid);


/**
 * Adds a copy of the given GeometryDefinition_t to this MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure to which the GeometryDefinition_t
 * should be added.
 *
 * @param gd the GeometryDefinition_t object to add.
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
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
int
MixedGeometry_addGeometryDefinition(MixedGeometry_t* mg,
                                    const GeometryDefinition_t* gd);


/**
 * Get the number of GeometryDefinition_t objects in this MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure to query.
 *
 * @return the number of GeometryDefinition_t objects in this MixedGeometry_t.
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumGeometryDefinitions(MixedGeometry_t* mg);


/**
 * Creates a new AnalyticGeometry_t object, adds it to this MixedGeometry_t
 * object and returns the AnalyticGeometry_t object created.
 *
 * @param mg the MixedGeometry_t structure to which the AnalyticGeometry_t
 * should be added.
 *
 * @return a new AnalyticGeometry_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t*
MixedGeometry_createAnalyticGeometry(MixedGeometry_t* mg);


/**
 * Creates a new SampledFieldGeometry_t object, adds it to this MixedGeometry_t
 * object and returns the SampledFieldGeometry_t object created.
 *
 * @param mg the MixedGeometry_t structure to which the SampledFieldGeometry_t
 * should be added.
 *
 * @return a new SampledFieldGeometry_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t*
MixedGeometry_createSampledFieldGeometry(MixedGeometry_t* mg);


/**
 * Creates a new CSGeometry_t object, adds it to this MixedGeometry_t object
 * and returns the CSGeometry_t object created.
 *
 * @param mg the MixedGeometry_t structure to which the CSGeometry_t should be
 * added.
 *
 * @return a new CSGeometry_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
CSGeometry_t*
MixedGeometry_createCSGeometry(MixedGeometry_t* mg);


/**
 * Creates a new ParametricGeometry_t object, adds it to this MixedGeometry_t
 * object and returns the ParametricGeometry_t object created.
 *
 * @param mg the MixedGeometry_t structure to which the ParametricGeometry_t
 * should be added.
 *
 * @return a new ParametricGeometry_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
ParametricGeometry_t*
MixedGeometry_createParametricGeometry(MixedGeometry_t* mg);


/**
 * Creates a new MixedGeometry_t object, adds it to this MixedGeometry_t object
 * and returns the MixedGeometry_t object created.
 *
 * @param mg the MixedGeometry_t structure to which the MixedGeometry_t should
 * be added.
 *
 * @return a new MixedGeometry_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
MixedGeometry_t*
MixedGeometry_createMixedGeometry(MixedGeometry_t* mg);


/**
 * Removes the nth GeometryDefinition_t from this MixedGeometry_t and returns a
 * pointer to it.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the GeometryDefinition_t
 * to remove.
 *
 * @return a pointer to the nth GeometryDefinition_t in this MixedGeometry_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_removeGeometryDefinition(MixedGeometry_t* mg, unsigned int n);


/**
 * Removes the GeometryDefinition_t from this MixedGeometry_t based on its
 * identifier and returns a pointer to it.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the GeometryDefinition_t
 * to remove.
 *
 * @return the GeometryDefinition_t in this MixedGeometry_t based on the
 * identifier or NULL if no such GeometryDefinition_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_removeGeometryDefinitionById(MixedGeometry_t* mg,
                                           const char* sid);


/**
 * Returns a ListOf_t * containing OrdinalMapping_t objects from this
 * MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure whose ListOfOrdinalMappings is
 * sought.
 *
 * @return the ListOfOrdinalMappings from this MixedGeometry_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MixedGeometry_addOrdinalMapping()
 * @see MixedGeometry_createOrdinalMapping()
 * @see MixedGeometry_getOrdinalMappingById()
 * @see MixedGeometry_getOrdinalMapping()
 * @see MixedGeometry_getNumOrdinalMappings()
 * @see MixedGeometry_removeOrdinalMappingById()
 * @see MixedGeometry_removeOrdinalMapping()
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
ListOf_t*
MixedGeometry_getListOfOrdinalMappings(MixedGeometry_t* mg);


/**
 * Get an OrdinalMapping_t from the MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the OrdinalMapping_t to
 * retrieve.
 *
 * @return the nth OrdinalMapping_t in the ListOfOrdinalMappings within this
 * MixedGeometry.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_getOrdinalMapping(MixedGeometry_t* mg, unsigned int n);


/**
 * Get an OrdinalMapping_t from the MixedGeometry_t based on the
 * GeometryDefinition to which it refers.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param sid a string representing the "geometryDefinition" attribute of the
 * OrdinalMapping_t object to retrieve.
 *
 * @return the first OrdinalMapping_t in this MixedGeometry_t based on the
 * given geometryDefinition attribute or NULL if no such OrdinalMapping_t
 * exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_getOrdinalMappingByGeometryDefinition(MixedGeometry_t* mg,
                                                    const char *sid);


/**
 * Adds a copy of the given OrdinalMapping_t to this MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure to which the OrdinalMapping_t should
 * be added.
 *
 * @param om the OrdinalMapping_t object to add.
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
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
int
MixedGeometry_addOrdinalMapping(MixedGeometry_t* mg,
                                const OrdinalMapping_t* om);


/**
 * Get the number of OrdinalMapping_t objects in this MixedGeometry_t.
 *
 * @param mg the MixedGeometry_t structure to query.
 *
 * @return the number of OrdinalMapping_t objects in this MixedGeometry_t.
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumOrdinalMappings(MixedGeometry_t* mg);


/**
 * Creates a new OrdinalMapping_t object, adds it to this MixedGeometry_t
 * object and returns the OrdinalMapping_t object created.
 *
 * @param mg the MixedGeometry_t structure to which the OrdinalMapping_t should
 * be added.
 *
 * @return a new OrdinalMapping_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_createOrdinalMapping(MixedGeometry_t* mg);


/**
 * Removes the nth OrdinalMapping_t from this MixedGeometry_t and returns a
 * pointer to it.
 *
 * @param mg the MixedGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the OrdinalMapping_t to
 * remove.
 *
 * @return a pointer to the nth OrdinalMapping_t in this MixedGeometry_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_removeOrdinalMapping(MixedGeometry_t* mg, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * MixedGeometry_t object have been set.
 *
 * @param mg the MixedGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * MixedGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
int
MixedGeometry_hasRequiredAttributes(const MixedGeometry_t * mg);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * MixedGeometry_t object have been set.
 *
 * @param mg the MixedGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * MixedGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the MixedGeometry_t object are:
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
int
MixedGeometry_hasRequiredElements(const MixedGeometry_t * mg);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !MixedGeometry_H__ */


