/**
 * @file DistribCauchyDistribution.h
 * @brief Definition of the DistribCauchyDistribution class.
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
 * @class DistribCauchyDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribCauchyDistribution class.
 */


#ifndef DistribCauchyDistribution_H__
#define DistribCauchyDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribContinuousUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribCauchyDistribution : public
  DistribContinuousUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mLocation;
  DistribUncertValue* mScale;

  /** @endcond */

public:

  /**
   * Creates a new DistribCauchyDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribCauchyDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribCauchyDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribCauchyDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribCauchyDistribution(
                            unsigned int level =
                              DistribExtension::getDefaultLevel(),
                            unsigned int version =
                              DistribExtension::getDefaultVersion(),
                            unsigned int pkgVersion =
                              DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribCauchyDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribCauchyDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribCauchyDistribution.
   *
   * @param orig the DistribCauchyDistribution instance to copy.
   */
  DistribCauchyDistribution(const DistribCauchyDistribution& orig);


  /**
   * Assignment operator for DistribCauchyDistribution.
   *
   * @param rhs the DistribCauchyDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribCauchyDistribution& operator=(const DistribCauchyDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribCauchyDistribution object.
   *
   * @return a (deep) copy of this DistribCauchyDistribution object.
   */
  virtual DistribCauchyDistribution* clone() const;


  /**
   * Destructor for DistribCauchyDistribution.
   */
  virtual ~DistribCauchyDistribution();


  /**
   * Returns the value of the "id" attribute of this DistribCauchyDistribution.
   *
   * @return the value of the "id" attribute of this DistribCauchyDistribution
   * as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribCauchyDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribCauchyDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribCauchyDistribution's "id"
   * attribute is set.
   *
   * @return @c true if this DistribCauchyDistribution's "id" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribCauchyDistribution's "name"
   * attribute is set.
   *
   * @return @c true if this DistribCauchyDistribution's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribCauchyDistribution.
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
   * Sets the value of the "name" attribute of this DistribCauchyDistribution.
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
   * Unsets the value of the "id" attribute of this DistribCauchyDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribCauchyDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "location" element of this
   * DistribCauchyDistribution.
   *
   * @return the value of the "location" element of this
   * DistribCauchyDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getLocation() const;


  /**
   * Returns the value of the "location" element of this
   * DistribCauchyDistribution.
   *
   * @return the value of the "location" element of this
   * DistribCauchyDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getLocation();


  /**
   * Returns the value of the "scale" element of this
   * DistribCauchyDistribution.
   *
   * @return the value of the "scale" element of this DistribCauchyDistribution
   * as a DistribUncertValue*.
   */
  const DistribUncertValue* getScale() const;


  /**
   * Returns the value of the "scale" element of this
   * DistribCauchyDistribution.
   *
   * @return the value of the "scale" element of this DistribCauchyDistribution
   * as a DistribUncertValue*.
   */
  DistribUncertValue* getScale();


  /**
   * Predicate returning @c true if this DistribCauchyDistribution's "location"
   * element is set.
   *
   * @return @c true if this DistribCauchyDistribution's "location" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetLocation() const;


  /**
   * Predicate returning @c true if this DistribCauchyDistribution's "scale"
   * element is set.
   *
   * @return @c true if this DistribCauchyDistribution's "scale" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetScale() const;


  /**
   * Sets the value of the "location" element of this
   * DistribCauchyDistribution.
   *
   * @param location DistribUncertValue* value of the "location" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setLocation(const DistribUncertValue* location);


  /**
   * Sets the value of the "scale" element of this DistribCauchyDistribution.
   *
   * @param scale DistribUncertValue* value of the "scale" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setScale(const DistribUncertValue* scale);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribCauchyDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createLocation();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribCauchyDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createScale();


  /**
   * Unsets the value of the "location" element of this
   * DistribCauchyDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLocation();


  /**
   * Unsets the value of the "scale" element of this DistribCauchyDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetScale();


  /**
   * Returns the XML element name of this DistribCauchyDistribution object.
   *
   * For DistribCauchyDistribution, the XML element name is always
   * @c "cauchyDistribution".
   *
   * @return the name of this element, i.e. @c "cauchyDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribCauchyDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_CAUCHYDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribCauchyDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribCauchyDistribution have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribCauchyDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribCauchyDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribCauchyDistribution object are:
   * @li "location"
   * @li "scale"
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * Predicate returning @c true if this DistribCauchyDistribution's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribCauchyDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribCauchyDistribution.
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
   * DistribCauchyDistribution.
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
   * Returns the number of "elementName" in this DistribCauchyDistribution.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribCauchyDistribution.
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
 * Creates a new DistribCauchyDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribCauchyDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribCauchyDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribCauchyDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t *
DistribCauchyDistribution_create(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribCauchyDistribution_t object.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return a (deep) copy of this DistribCauchyDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t*
DistribCauchyDistribution_clone(const DistribCauchyDistribution_t* dcd);


/**
 * Frees this DistribCauchyDistribution_t object.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
void
DistribCauchyDistribution_free(DistribCauchyDistribution_t* dcd);


/**
 * Returns the value of the "id" attribute of this DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribCauchyDistribution_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
char *
DistribCauchyDistribution_getId(const DistribCauchyDistribution_t * dcd);


/**
 * Returns the value of the "name" attribute of this
 * DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this
 * DistribCauchyDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
char *
DistribCauchyDistribution_getName(const DistribCauchyDistribution_t * dcd);


/**
 * Predicate returning @c 1 (true) if this DistribCauchyDistribution_t's "id"
 * attribute is set.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribCauchyDistribution_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_isSetId(const DistribCauchyDistribution_t * dcd);


/**
 * Predicate returning @c 1 (true) if this DistribCauchyDistribution_t's "name"
 * attribute is set.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribCauchyDistribution_t's "name" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_isSetName(const DistribCauchyDistribution_t * dcd);


/**
 * Sets the value of the "id" attribute of this DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribCauchyDistribution_unsetId().
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_setId(DistribCauchyDistribution_t * dcd,
                                const char * id);


/**
 * Sets the value of the "name" attribute of this DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribCauchyDistribution_unsetName().
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_setName(DistribCauchyDistribution_t * dcd,
                                  const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_unsetId(DistribCauchyDistribution_t * dcd);


/**
 * Unsets the value of the "name" attribute of this
 * DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_unsetName(DistribCauchyDistribution_t * dcd);


/**
 * Returns the value of the "location" element of this
 * DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure whose location is
 * sought.
 *
 * @return the value of the "location" element of this
 * DistribCauchyDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCauchyDistribution_getLocation(const DistribCauchyDistribution_t * dcd);


/**
 * Returns the value of the "scale" element of this
 * DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure whose scale is sought.
 *
 * @return the value of the "scale" element of this DistribCauchyDistribution_t
 * as a DistribUncertValue*.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCauchyDistribution_getScale(const DistribCauchyDistribution_t * dcd);


/**
 * Predicate returning @c 1 (true) if this DistribCauchyDistribution_t's
 * "location" element is set.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribCauchyDistribution_t's "location" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_isSetLocation(const DistribCauchyDistribution_t *
  dcd);


/**
 * Predicate returning @c 1 (true) if this DistribCauchyDistribution_t's
 * "scale" element is set.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribCauchyDistribution_t's "scale" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_isSetScale(const DistribCauchyDistribution_t * dcd);


/**
 * Sets the value of the "location" element of this
 * DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @param location DistribUncertValue_t* value of the "location" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_setLocation(DistribCauchyDistribution_t * dcd,
                                      const DistribUncertValue_t* location);


/**
 * Sets the value of the "scale" element of this DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @param scale DistribUncertValue_t* value of the "scale" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_setScale(DistribCauchyDistribution_t * dcd,
                                   const DistribUncertValue_t* scale);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribCauchyDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dcd the DistribCauchyDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCauchyDistribution_createLocation(DistribCauchyDistribution_t* dcd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribCauchyDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dcd the DistribCauchyDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCauchyDistribution_createScale(DistribCauchyDistribution_t* dcd);


/**
 * Unsets the value of the "location" element of this
 * DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_unsetLocation(DistribCauchyDistribution_t * dcd);


/**
 * Unsets the value of the "scale" element of this DistribCauchyDistribution_t.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_unsetScale(DistribCauchyDistribution_t * dcd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCauchyDistribution_t object have been set.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribCauchyDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_hasRequiredAttributes(const
  DistribCauchyDistribution_t * dcd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribCauchyDistribution_t object have been set.
 *
 * @param dcd the DistribCauchyDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribCauchyDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribCauchyDistribution_t object are:
 * @li "location"
 * @li "scale"
 *
 * @memberof DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_hasRequiredElements(const DistribCauchyDistribution_t
  * dcd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribCauchyDistribution_H__ */


