/**
 * @file DistribPoissonDistribution.h
 * @brief Definition of the DistribPoissonDistribution class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class DistribPoissonDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribPoissonDistribution class.
 */


#ifndef DistribPoissonDistribution_H__
#define DistribPoissonDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribDiscreteUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribPoissonDistribution : public
  DistribDiscreteUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mRate;

  /** @endcond */

public:

  /**
   * Creates a new DistribPoissonDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribPoissonDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribPoissonDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribPoissonDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribPoissonDistribution(
                             unsigned int level =
                               DistribExtension::getDefaultLevel(),
                             unsigned int version =
                               DistribExtension::getDefaultVersion(),
                             unsigned int pkgVersion =
                               DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribPoissonDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribPoissonDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribPoissonDistribution.
   *
   * @param orig the DistribPoissonDistribution instance to copy.
   */
  DistribPoissonDistribution(const DistribPoissonDistribution& orig);


  /**
   * Assignment operator for DistribPoissonDistribution.
   *
   * @param rhs the DistribPoissonDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribPoissonDistribution& operator=(const DistribPoissonDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribPoissonDistribution object.
   *
   * @return a (deep) copy of this DistribPoissonDistribution object.
   */
  virtual DistribPoissonDistribution* clone() const;


  /**
   * Destructor for DistribPoissonDistribution.
   */
  virtual ~DistribPoissonDistribution();


  /**
   * Returns the value of the "rate" element of this
   * DistribPoissonDistribution.
   *
   * @return the value of the "rate" element of this DistribPoissonDistribution
   * as a DistribUncertValue*.
   */
  const DistribUncertValue* getRate() const;


  /**
   * Returns the value of the "rate" element of this
   * DistribPoissonDistribution.
   *
   * @return the value of the "rate" element of this DistribPoissonDistribution
   * as a DistribUncertValue*.
   */
  DistribUncertValue* getRate();


  /**
   * Predicate returning @c true if this DistribPoissonDistribution's "rate"
   * element is set.
   *
   * @return @c true if this DistribPoissonDistribution's "rate" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetRate() const;


  /**
   * Sets the value of the "rate" element of this DistribPoissonDistribution.
   *
   * @param rate DistribUncertValue* value of the "rate" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRate(const DistribUncertValue* rate);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribPoissonDistribution object and returns the DistribUncertValue
   * object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createRate();


  /**
   * Unsets the value of the "rate" element of this DistribPoissonDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRate();


  /**
   * Returns the XML element name of this DistribPoissonDistribution object.
   *
   * For DistribPoissonDistribution, the XML element name is always
   * @c "poissonDistribution".
   *
   * @return the name of this element, i.e. @c "poissonDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribPoissonDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_POISSONDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribPoissonDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribPoissonDistribution have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribPoissonDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribPoissonDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribPoissonDistribution object are:
   * @li "rate"
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * Predicate returning @c true if this DistribPoissonDistribution's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribPoissonDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribPoissonDistribution.
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
   * DistribPoissonDistribution.
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
   * Returns the number of "elementName" in this DistribPoissonDistribution.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribPoissonDistribution.
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
 * Creates a new DistribPoissonDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribPoissonDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribPoissonDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribPoissonDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t *
DistribPoissonDistribution_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribPoissonDistribution_t object.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @return a (deep) copy of this DistribPoissonDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t*
DistribPoissonDistribution_clone(const DistribPoissonDistribution_t* dpd);


/**
 * Frees this DistribPoissonDistribution_t object.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
void
DistribPoissonDistribution_free(DistribPoissonDistribution_t* dpd);


/**
 * Returns the value of the "rate" element of this
 * DistribPoissonDistribution_t.
 *
 * @param dpd the DistribPoissonDistribution_t structure whose rate is sought.
 *
 * @return the value of the "rate" element of this DistribPoissonDistribution_t
 * as a DistribUncertValue*.
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribPoissonDistribution_getRate(const DistribPoissonDistribution_t * dpd);


/**
 * Predicate returning @c 1 (true) if this DistribPoissonDistribution_t's
 * "rate" element is set.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribPoissonDistribution_t's "rate" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_isSetRate(const DistribPoissonDistribution_t * dpd);


/**
 * Sets the value of the "rate" element of this DistribPoissonDistribution_t.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @param rate DistribUncertValue_t* value of the "rate" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_setRate(DistribPoissonDistribution_t * dpd,
                                   const DistribUncertValue_t* rate);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribPoissonDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dpd the DistribPoissonDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribPoissonDistribution_createRate(DistribPoissonDistribution_t* dpd);


/**
 * Unsets the value of the "rate" element of this DistribPoissonDistribution_t.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_unsetRate(DistribPoissonDistribution_t * dpd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribPoissonDistribution_t object have been set.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribPoissonDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_hasRequiredAttributes(const
  DistribPoissonDistribution_t * dpd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribPoissonDistribution_t object have been set.
 *
 * @param dpd the DistribPoissonDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribPoissonDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribPoissonDistribution_t object are:
 * @li "rate"
 *
 * @memberof DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribPoissonDistribution_hasRequiredElements(const
  DistribPoissonDistribution_t * dpd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribPoissonDistribution_H__ */


