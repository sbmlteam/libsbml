/**
 * @file AnalyticGeometry.h
 * @brief Definition of the AnalyticGeometry class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class AnalyticGeometry
 * @sbmlbrief{spatial} TODO:Definition of the AnalyticGeometry class.
 */


#ifndef AnalyticGeometry_H__
#define AnalyticGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfAnalyticVolumes.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN AnalyticGeometry : public GeometryDefinition
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfAnalyticVolumes mAnalyticVolumes;

  /** @endcond */

public:

  /**
   * Creates a new AnalyticGeometry using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * AnalyticGeometry.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * AnalyticGeometry.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this AnalyticGeometry.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  AnalyticGeometry(unsigned int level = SpatialExtension::getDefaultLevel(),
                   unsigned int version =
                     SpatialExtension::getDefaultVersion(),
                   unsigned int pkgVersion =
                     SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AnalyticGeometry using the given SpatialPkgNamespaces
   * object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  AnalyticGeometry(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for AnalyticGeometry.
   *
   * @param orig the AnalyticGeometry instance to copy.
   */
  AnalyticGeometry(const AnalyticGeometry& orig);


  /**
   * Assignment operator for AnalyticGeometry.
   *
   * @param rhs the AnalyticGeometry object whose values are to be used as the
   * basis of the assignment.
   */
  AnalyticGeometry& operator=(const AnalyticGeometry& rhs);


  /**
   * Creates and returns a deep copy of this AnalyticGeometry object.
   *
   * @return a (deep) copy of this AnalyticGeometry object.
   */
  virtual AnalyticGeometry* clone() const;


  /**
   * Destructor for AnalyticGeometry.
   */
  virtual ~AnalyticGeometry();


  /**
   * Returns the ListOfAnalyticVolumes from this AnalyticGeometry.
   *
   * @return the ListOfAnalyticVolumes from this AnalyticGeometry.
   */
  const ListOfAnalyticVolumes* getListOfAnalyticVolumes() const;


  /**
   * Returns the ListOfAnalyticVolumes from this AnalyticGeometry.
   *
   * @return the ListOfAnalyticVolumes from this AnalyticGeometry.
   */
  ListOfAnalyticVolumes* getListOfAnalyticVolumes();


  /**
   * Get an AnalyticVolume from the AnalyticGeometry.
   *
   * @param n an unsigned int representing the index of the AnalyticVolume to
   * retrieve.
   *
   * @return the nth AnalyticVolume in the ListOfAnalyticVolumes within this
   * AnalyticGeometry.
   *
   * @see getNumAnalyticVolumes()
   */
  AnalyticVolume* getAnalyticVolume(unsigned int n);


  /**
   * Get an AnalyticVolume from the AnalyticGeometry.
   *
   * @param n an unsigned int representing the index of the AnalyticVolume to
   * retrieve.
   *
   * @return the nth AnalyticVolume in the ListOfAnalyticVolumes within this
   * AnalyticGeometry.
   *
   * @see getNumAnalyticVolumes()
   */
  const AnalyticVolume* getAnalyticVolume(unsigned int n) const;


  /**
   * Get an AnalyticVolume from the AnalyticGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the AnalyticVolume to
   * retrieve.
   *
   * @return the AnalyticVolume in the ListOfAnalyticVolumes within this
   * AnalyticGeometry with the given id or NULL if no such AnalyticVolume
   * exists.
   *
   * @see getAnalyticVolume(unsigned int n)
   * @see getNumAnalyticVolumes()
   */
  AnalyticVolume* getAnalyticVolume(const std::string& sid);


  /**
   * Get an AnalyticVolume from the AnalyticGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the AnalyticVolume to
   * retrieve.
   *
   * @return the AnalyticVolume in the ListOfAnalyticVolumes within this
   * AnalyticGeometry with the given id or NULL if no such AnalyticVolume
   * exists.
   *
   * @see getAnalyticVolume(unsigned int n)
   * @see getNumAnalyticVolumes()
   */
  const AnalyticVolume* getAnalyticVolume(const std::string& sid) const;


  /**
   * Get an AnalyticVolume from the AnalyticGeometry based on the DomainType to
   * which it refers.
   *
   * @param sid a string representing the domainType attribute of the
   * AnalyticVolume object to retrieve.
   *
   * @return the first AnalyticVolume in this AnalyticGeometry based on the
   * given domainType attribute or NULL if no such AnalyticVolume exists.
   */
  const AnalyticVolume* getAnalyticVolumeByDomainType(const std::string& sid)
    const;


  /**
   * Get an AnalyticVolume from the AnalyticGeometry based on the DomainType to
   * which it refers.
   *
   * @param sid a string representing the domainType attribute of the
   * AnalyticVolume object to retrieve.
   *
   * @return the first AnalyticVolume in this AnalyticGeometry based on the
   * given domainType attribute or NULL if no such AnalyticVolume exists.
   */
  AnalyticVolume* getAnalyticVolumeByDomainType(const std::string& sid);


  /**
   * Adds a copy of the given AnalyticVolume to this AnalyticGeometry.
   *
   * @param av the AnalyticVolume object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createAnalyticVolume()
   */
  int addAnalyticVolume(const AnalyticVolume* av);


  /**
   * Get the number of AnalyticVolume objects in this AnalyticGeometry.
   *
   * @return the number of AnalyticVolume objects in this AnalyticGeometry.
   */
  unsigned int getNumAnalyticVolumes() const;


  /**
   * Creates a new AnalyticVolume object, adds it to this AnalyticGeometry
   * object and returns the AnalyticVolume object created.
   *
   * @return a new AnalyticVolume object instance.
   *
   * @see addAnalyticVolume(const AnalyticVolume* av)
   */
  AnalyticVolume* createAnalyticVolume();


  /**
   * Removes the nth AnalyticVolume from this AnalyticGeometry and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the AnalyticVolume to
   * remove.
   *
   * @return a pointer to the nth AnalyticVolume in this AnalyticGeometry.
   *
   * @see getNumAnalyticVolumes
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  AnalyticVolume* removeAnalyticVolume(unsigned int n);


  /**
   * Removes the AnalyticVolume from this AnalyticGeometry based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the AnalyticVolume to
   * remove.
   *
   * @return the AnalyticVolume in this AnalyticGeometry based on the
   * identifier or NULL if no such AnalyticVolume exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  AnalyticVolume* removeAnalyticVolume(const std::string& sid);


  /**
   * Returns the XML element name of this AnalyticGeometry object.
   *
   * For AnalyticGeometry, the XML element name is always @c
   * "analyticGeometry".
   *
   * @return the name of this element, i.e. @c "analyticGeometry".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this AnalyticGeometry object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_SPATIAL_ANALYTICGEOMETRY, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * AnalyticGeometry object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * AnalyticGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the AnalyticGeometry object are:
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * AnalyticGeometry object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * AnalyticGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the AnalyticGeometry object are:
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




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Predicate returning @c true if this AnalyticGeometry's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this AnalyticGeometry's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
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
   * AnalyticGeometry.
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
   * Creates and returns an new "elementName" object in this AnalyticGeometry.
   *
   * @param elementName, the name of the element to create.
   *
   * pointer to the element created.
   */
  virtual SBase* createObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this AnalyticGeometry.
   *
   * @param elementName, the name of the element to get number of.
   *
   * unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this AnalyticGeometry.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int teh index of teh object to retrieve.
   *
   * pointer to the object.
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
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * filter, an ElementFilter that may impose restrictions on the objects to be
   * retrieved.
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
 * Creates a new AnalyticGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * AnalyticGeometry_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * AnalyticGeometry_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this AnalyticGeometry_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_create(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this AnalyticGeometry_t object.
 *
 * @param ag the AnalyticGeometry_t structure.
 *
 * @return a (deep) copy of this AnalyticGeometry_t object.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t*
AnalyticGeometry_clone(const AnalyticGeometry_t* ag);


/**
 * Frees this AnalyticGeometry_t object.
 *
 * @param ag the AnalyticGeometry_t structure.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
void
AnalyticGeometry_free(AnalyticGeometry_t* ag);


/**
 * Returns a ListOf_t* containing AnalyticVolume_t objects from this
 * AnalyticGeometry_t.
 *
 * @param ag the AnalyticGeometry_t structure whose "ListOfAnalyticVolumes" is
 * sought.
 *
 * @return the "ListOfAnalyticVolumes" from this AnalyticGeometry_t as a
 * ListOf_t *.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
ListOf_t*
AnalyticGeometry_getListOfAnalyticVolumes(AnalyticGeometry_t* ag);


/**
 * Get an AnalyticVolume_t from the AnalyticGeometry_t.
 *
 * @param ag the AnalyticGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the AnalyticVolume_t to
 * retrieve.
 *
 * @return the nth AnalyticVolume_t in the ListOfAnalyticVolumes within this
 * AnalyticGeometry.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
const AnalyticVolume_t*
AnalyticGeometry_getAnalyticVolume(AnalyticGeometry_t* ag, unsigned int n);


/**
 * Get an AnalyticVolume_t from the AnalyticGeometry_t based on its identifier.
 *
 * @param ag the AnalyticGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the AnalyticVolume_t to
 * retrieve.
 *
 * @return the AnalyticVolume_t in the ListOfAnalyticVolumes within this
 * AnalyticGeometry with the given id or NULL if no such AnalyticVolume_t
 * exists.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
const AnalyticVolume_t*
AnalyticGeometry_getAnalyticVolumeById(AnalyticGeometry_t* ag,
                                       const char *sid);


/**
 * Get an AnalyticVolume_t from the AnalyticGeometry_t based on the DomainType
 * to which it refers.
 *
 * @param ag the AnalyticGeometry_t structure to search.
 *
 * @param sid a string representing the domainType attribute of the
 * AnalyticVolume_t object to retrieve.
 *
 * @return the first AnalyticVolume_t in this AnalyticGeometry_t based on the
 * given domainType attribute or NULL if no such AnalyticVolume_t exists.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
const AnalyticVolume_t*
AnalyticGeometry_getAnalyticVolumeByDomainType(AnalyticGeometry_t* ag,
                                               const char *sid);


/**
 * Adds a copy of the given AnalyticVolume_t to this AnalyticGeometry_t.
 *
 * @param ag the AnalyticGeometry_t structure to which the AnalyticVolume_t
 * should be added.
 *
 * @param av the AnalyticVolume_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
int
AnalyticGeometry_addAnalyticVolume(AnalyticGeometry_t* ag,
                                   const AnalyticVolume_t* av);


/**
 * Get the number of AnalyticVolume_t objects in this AnalyticGeometry_t.
 *
 * @param ag the AnalyticGeometry_t structure to query.
 *
 * @return the number of AnalyticVolume_t objects in this AnalyticGeometry_t.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
unsigned int
AnalyticGeometry_getNumAnalyticVolumes(AnalyticGeometry_t* ag);


/**
 * Creates a new AnalyticVolume_t object, adds it to this AnalyticGeometry_t
 * object and returns the AnalyticVolume_t object created.
 *
 * @param ag the AnalyticGeometry_t structure to which the AnalyticVolume_t
 * should be added.
 *
 * @return a new AnalyticVolume_t object instance.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_createAnalyticVolume(AnalyticGeometry_t* ag);


/**
 * Removes the nth AnalyticVolume_t from this AnalyticGeometry_t and returns a
 * pointer to it.
 *
 * @param ag the AnalyticGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the AnalyticVolume_t to
 * remove.
 *
 * @return a pointer to the nth AnalyticVolume_t in this AnalyticGeometry_t.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_removeAnalyticVolume(AnalyticGeometry_t* ag, unsigned int n);


/**
 * Removes the AnalyticVolume_t from this AnalyticGeometry_t based on its
 * identifier and returns a pointer to it.
 *
 * @param ag the AnalyticGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the AnalyticVolume_t to
 * remove.
 *
 * @return the AnalyticVolume_t in this AnalyticGeometry_t based on the
 * identifier or NULL if no such AnalyticVolume_t exists.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_removeAnalyticVolumeById(AnalyticGeometry_t* ag,
                                          const char* sid);


/**
 * Predicate returning @c 1 if all the required attributes for this
 * AnalyticGeometry_t object have been set.
 *
 * @param ag the AnalyticGeometry_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this
 * AnalyticGeometry_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the AnalyticGeometry_t object are:
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredAttributes(const AnalyticGeometry_t * ag);


/**
 * Predicate returning @c 1 if all the required elements for this
 * AnalyticGeometry_t object have been set.
 *
 * @param ag the AnalyticGeometry_t structure.
 *
 * @return @c 1 to indicate that all the required elements of this
 * AnalyticGeometry_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required elements for the AnalyticGeometry_t object are:
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredElements(const AnalyticGeometry_t * ag);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !AnalyticGeometry_H__ */


