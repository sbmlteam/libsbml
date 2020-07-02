/**
 * @file Domain.h
 * @brief Definition of the Domain class.
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
 * @class Domain
 * @sbmlbrief{spatial} TODO:Definition of the Domain class.
 */


#ifndef Domain_H__
#define Domain_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfInteriorPoints.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Domain : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDomainType;
  ListOfInteriorPoints mInteriorPoints;

  /** @endcond */

public:

  /**
   * Creates a new Domain using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Domain.
   *
   * @param version an unsigned int, the SBML Version to assign to this Domain.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this Domain.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Domain(unsigned int level = SpatialExtension::getDefaultLevel(),
         unsigned int version = SpatialExtension::getDefaultVersion(),
         unsigned int pkgVersion =
           SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Domain using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Domain(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for Domain.
   *
   * @param orig the Domain instance to copy.
   */
  Domain(const Domain& orig);


  /**
   * Assignment operator for Domain.
   *
   * @param rhs the Domain object whose values are to be used as the basis of
   * the assignment.
   */
  Domain& operator=(const Domain& rhs);


  /**
   * Creates and returns a deep copy of this Domain object.
   *
   * @return a (deep) copy of this Domain object.
   */
  virtual Domain* clone() const;


  /**
   * Destructor for Domain.
   */
  virtual ~Domain();


  /**
   * Returns the value of the "id" attribute of this Domain.
   *
   * @return the value of the "id" attribute of this Domain as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this Domain.
   *
   * @return the value of the "name" attribute of this Domain as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "domainType" attribute of this Domain.
   *
   * @return the value of the "domainType" attribute of this Domain as a
   * string.
   */
  const std::string& getDomainType() const;


  /**
   * Predicate returning @c true if this Domain's "id" attribute is set.
   *
   * @return @c true if this Domain's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this Domain's "name" attribute is set.
   *
   * @return @c true if this Domain's "name" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this Domain's "domainType" attribute is
   * set.
   *
   * @return @c true if this Domain's "domainType" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetDomainType() const;


  /**
   * Sets the value of the "id" attribute of this Domain.
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
   * Sets the value of the "name" attribute of this Domain.
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
   * Sets the value of the "domainType" attribute of this Domain.
   *
   * @param domainType std::string& value of the "domainType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDomainType(const std::string& domainType);


  /**
   * Unsets the value of the "id" attribute of this Domain.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this Domain.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "domainType" attribute of this Domain.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomainType();


  /**
   * Returns the ListOfInteriorPoints from this Domain.
   *
   * @return the ListOfInteriorPoints from this Domain.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getInteriorPoint(unsigned int n)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  const ListOfInteriorPoints* getListOfInteriorPoints() const;


  /**
   * Returns the ListOfInteriorPoints from this Domain.
   *
   * @return the ListOfInteriorPoints from this Domain.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getInteriorPoint(unsigned int n)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  ListOfInteriorPoints* getListOfInteriorPoints();


  /**
   * Get an InteriorPoint from the Domain.
   *
   * @param n an unsigned int representing the index of the InteriorPoint to
   * retrieve.
   *
   * @return the nth InteriorPoint in the ListOfInteriorPoints within this
   * Domain or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  InteriorPoint* getInteriorPoint(unsigned int n);


  /**
   * Get an InteriorPoint from the Domain.
   *
   * @param n an unsigned int representing the index of the InteriorPoint to
   * retrieve.
   *
   * @return the nth InteriorPoint in the ListOfInteriorPoints within this
   * Domain or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  const InteriorPoint* getInteriorPoint(unsigned int n) const;


  /**
   * Adds a copy of the given InteriorPoint to this Domain.
   *
   * @param ip the InteriorPoint object to add.
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
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getInteriorPoint(unsigned int n)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  int addInteriorPoint(const InteriorPoint* ip);


  /**
   * Get the number of InteriorPoint objects in this Domain.
   *
   * @return the number of InteriorPoint objects in this Domain.
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getInteriorPoint(unsigned int n)
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  unsigned int getNumInteriorPoints() const;


  /**
   * Creates a new InteriorPoint object, adds it to this Domain object and
   * returns the InteriorPoint object created.
   *
   * @return a new InteriorPoint object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see getInteriorPoint(const std::string& sid)
   * @see getInteriorPoint(unsigned int n)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   * @see removeInteriorPoint(unsigned int n)
   */
  InteriorPoint* createInteriorPoint();


  /**
   * Removes the nth InteriorPoint from this Domain and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the InteriorPoint to
   * remove.
   *
   * @return a pointer to the nth InteriorPoint in this Domain.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addInteriorPoint(const InteriorPoint* object)
   * @see createInteriorPoint()
   * @see getInteriorPoint(const std::string& sid)
   * @see getInteriorPoint(unsigned int n)
   * @see getNumInteriorPoints()
   * @see removeInteriorPoint(const std::string& sid)
   */
  InteriorPoint* removeInteriorPoint(unsigned int n);


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this Domain object.
   *
   * For Domain, the XML element name is always @c "domain".
   *
   * @return the name of this element, i.e. @c "domain".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Domain object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_DOMAIN, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this Domain
   * object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Domain have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the Domain object are:
   * @li "id"
   * @li "domainType"
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
   * Gets the value of the "attributeName" attribute of this Domain.
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
   * Gets the value of the "attributeName" attribute of this Domain.
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
   * Gets the value of the "attributeName" attribute of this Domain.
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
   * Gets the value of the "attributeName" attribute of this Domain.
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
   * Gets the value of the "attributeName" attribute of this Domain.
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
   * Predicate returning @c true if this Domain's attribute "attributeName" is
   * set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Domain's attribute "attributeName" has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Domain.
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
   * Sets the value of the "attributeName" attribute of this Domain.
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
   * Sets the value of the "attributeName" attribute of this Domain.
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
   * Sets the value of the "attributeName" attribute of this Domain.
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
   * Sets the value of the "attributeName" attribute of this Domain.
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
   * Unsets the value of the "attributeName" attribute of this Domain.
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
   * Creates and returns an new "elementName" object in this Domain.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this Domain.
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
   * Domain.
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
   * Returns the number of "elementName" in this Domain.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this Domain.
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
 * Creates a new Domain_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Domain_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this Domain_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this Domain_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
Domain_t *
Domain_create(unsigned int level,
              unsigned int version,
              unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Domain_t object.
 *
 * @param d the Domain_t structure.
 *
 * @return a (deep) copy of this Domain_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
Domain_t*
Domain_clone(const Domain_t* d);


/**
 * Frees this Domain_t object.
 *
 * @param d the Domain_t structure.
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
void
Domain_free(Domain_t* d);


/**
 * Returns the value of the "id" attribute of this Domain_t.
 *
 * @param d the Domain_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this Domain_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
char *
Domain_getId(const Domain_t * d);


/**
 * Returns the value of the "name" attribute of this Domain_t.
 *
 * @param d the Domain_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this Domain_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
char *
Domain_getName(const Domain_t * d);


/**
 * Returns the value of the "domainType" attribute of this Domain_t.
 *
 * @param d the Domain_t structure whose domainType is sought.
 *
 * @return the value of the "domainType" attribute of this Domain_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
char *
Domain_getDomainType(const Domain_t * d);


/**
 * Predicate returning @c 1 (true) if this Domain_t's "id" attribute is set.
 *
 * @param d the Domain_t structure.
 *
 * @return @c 1 (true) if this Domain_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_isSetId(const Domain_t * d);


/**
 * Predicate returning @c 1 (true) if this Domain_t's "name" attribute is set.
 *
 * @param d the Domain_t structure.
 *
 * @return @c 1 (true) if this Domain_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_isSetName(const Domain_t * d);


/**
 * Predicate returning @c 1 (true) if this Domain_t's "domainType" attribute is
 * set.
 *
 * @param d the Domain_t structure.
 *
 * @return @c 1 (true) if this Domain_t's "domainType" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_isSetDomainType(const Domain_t * d);


/**
 * Sets the value of the "id" attribute of this Domain_t.
 *
 * @param d the Domain_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling Domain_unsetId().
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_setId(Domain_t * d, const char * id);


/**
 * Sets the value of the "name" attribute of this Domain_t.
 *
 * @param d the Domain_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling Domain_unsetName().
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_setName(Domain_t * d, const char * name);


/**
 * Sets the value of the "domainType" attribute of this Domain_t.
 *
 * @param d the Domain_t structure.
 *
 * @param domainType const char * value of the "domainType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_setDomainType(Domain_t * d, const char * domainType);


/**
 * Unsets the value of the "id" attribute of this Domain_t.
 *
 * @param d the Domain_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_unsetId(Domain_t * d);


/**
 * Unsets the value of the "name" attribute of this Domain_t.
 *
 * @param d the Domain_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_unsetName(Domain_t * d);


/**
 * Unsets the value of the "domainType" attribute of this Domain_t.
 *
 * @param d the Domain_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_unsetDomainType(Domain_t * d);


/**
 * Returns a ListOf_t * containing InteriorPoint_t objects from this Domain_t.
 *
 * @param d the Domain_t structure whose ListOfInteriorPoints is sought.
 *
 * @return the ListOfInteriorPoints from this Domain_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see Domain_addInteriorPoint()
 * @see Domain_createInteriorPoint()
 * @see Domain_getInteriorPointById()
 * @see Domain_getInteriorPoint()
 * @see Domain_getNumInteriorPoints()
 * @see Domain_removeInteriorPointById()
 * @see Domain_removeInteriorPoint()
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
ListOf_t*
Domain_getListOfInteriorPoints(Domain_t* d);


/**
 * Get an InteriorPoint_t from the Domain_t.
 *
 * @param d the Domain_t structure to search.
 *
 * @param n an unsigned int representing the index of the InteriorPoint_t to
 * retrieve.
 *
 * @return the nth InteriorPoint_t in the ListOfInteriorPoints within this
 * Domain or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
Domain_getInteriorPoint(Domain_t* d, unsigned int n);


/**
 * Adds a copy of the given InteriorPoint_t to this Domain_t.
 *
 * @param d the Domain_t structure to which the InteriorPoint_t should be
 * added.
 *
 * @param ip the InteriorPoint_t object to add.
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
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_addInteriorPoint(Domain_t* d, const InteriorPoint_t* ip);


/**
 * Get the number of InteriorPoint_t objects in this Domain_t.
 *
 * @param d the Domain_t structure to query.
 *
 * @return the number of InteriorPoint_t objects in this Domain_t.
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
unsigned int
Domain_getNumInteriorPoints(Domain_t* d);


/**
 * Creates a new InteriorPoint_t object, adds it to this Domain_t object and
 * returns the InteriorPoint_t object created.
 *
 * @param d the Domain_t structure to which the InteriorPoint_t should be
 * added.
 *
 * @return a new InteriorPoint_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
Domain_createInteriorPoint(Domain_t* d);


/**
 * Removes the nth InteriorPoint_t from this Domain_t and returns a pointer to
 * it.
 *
 * @param d the Domain_t structure to search.
 *
 * @param n an unsigned int representing the index of the InteriorPoint_t to
 * remove.
 *
 * @return a pointer to the nth InteriorPoint_t in this Domain_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
Domain_removeInteriorPoint(Domain_t* d, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Domain_t object have been set.
 *
 * @param d the Domain_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Domain_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the Domain_t object are:
 * @li "id"
 * @li "domainType"
 *
 * @memberof Domain_t
 */
LIBSBML_EXTERN
int
Domain_hasRequiredAttributes(const Domain_t * d);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Domain_H__ */


