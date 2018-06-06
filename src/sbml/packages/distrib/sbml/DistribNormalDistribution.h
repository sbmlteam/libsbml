/**
 * @file DistribNormalDistribution.h
 * @brief Definition of the DistribNormalDistribution class.
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
 * @class DistribNormalDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribNormalDistribution class.
 */


#ifndef DistribNormalDistribution_H__
#define DistribNormalDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribContinuousUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribNormalDistribution : public
  DistribContinuousUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mMean;
  DistribUncertValue* mStddev;
  DistribUncertValue* mVariance;

  /** @endcond */

public:

  /**
   * Creates a new DistribNormalDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribNormalDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribNormalDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribNormalDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribNormalDistribution(
                            unsigned int level =
                              DistribExtension::getDefaultLevel(),
                            unsigned int version =
                              DistribExtension::getDefaultVersion(),
                            unsigned int pkgVersion =
                              DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribNormalDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribNormalDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribNormalDistribution.
   *
   * @param orig the DistribNormalDistribution instance to copy.
   */
  DistribNormalDistribution(const DistribNormalDistribution& orig);


  /**
   * Assignment operator for DistribNormalDistribution.
   *
   * @param rhs the DistribNormalDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribNormalDistribution& operator=(const DistribNormalDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribNormalDistribution object.
   *
   * @return a (deep) copy of this DistribNormalDistribution object.
   */
  virtual DistribNormalDistribution* clone() const;


  /**
   * Destructor for DistribNormalDistribution.
   */
  virtual ~DistribNormalDistribution();


  /**
   * Returns the value of the "mean" element of this DistribNormalDistribution.
   *
   * @return the value of the "mean" element of this DistribNormalDistribution
   * as a DistribUncertValue*.
   */
  const DistribUncertValue* getMean() const;


  /**
   * Returns the value of the "mean" element of this DistribNormalDistribution.
   *
   * @return the value of the "mean" element of this DistribNormalDistribution
   * as a DistribUncertValue*.
   */
  DistribUncertValue* getMean();


  /**
   * Returns the value of the "stddev" element of this
   * DistribNormalDistribution.
   *
   * @return the value of the "stddev" element of this
   * DistribNormalDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getStddev() const;


  /**
   * Returns the value of the "stddev" element of this
   * DistribNormalDistribution.
   *
   * @return the value of the "stddev" element of this
   * DistribNormalDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getStddev();


  /**
   * Returns the value of the "variance" element of this
   * DistribNormalDistribution.
   *
   * @return the value of the "variance" element of this
   * DistribNormalDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getVariance() const;


  /**
   * Returns the value of the "variance" element of this
   * DistribNormalDistribution.
   *
   * @return the value of the "variance" element of this
   * DistribNormalDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getVariance();


  /**
   * Predicate returning @c true if this DistribNormalDistribution's "mean"
   * element is set.
   *
   * @return @c true if this DistribNormalDistribution's "mean" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetMean() const;


  /**
   * Predicate returning @c true if this DistribNormalDistribution's "stddev"
   * element is set.
   *
   * @return @c true if this DistribNormalDistribution's "stddev" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetStddev() const;


  /**
   * Predicate returning @c true if this DistribNormalDistribution's "variance"
   * element is set.
   *
   * @return @c true if this DistribNormalDistribution's "variance" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetVariance() const;


  /**
   * Sets the value of the "mean" element of this DistribNormalDistribution.
   *
   * @param mean DistribUncertValue* value of the "mean" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMean(const DistribUncertValue* mean);


  /**
   * Sets the value of the "stddev" element of this DistribNormalDistribution.
   *
   * @param stddev DistribUncertValue* value of the "stddev" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStddev(const DistribUncertValue* stddev);


  /**
   * Sets the value of the "variance" element of this
   * DistribNormalDistribution.
   *
   * @param variance DistribUncertValue* value of the "variance" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVariance(const DistribUncertValue* variance);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribNormalDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createMean();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribNormalDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createStddev();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribNormalDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createVariance();


  /**
   * Unsets the value of the "mean" element of this DistribNormalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMean();


  /**
   * Unsets the value of the "stddev" element of this
   * DistribNormalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStddev();


  /**
   * Unsets the value of the "variance" element of this
   * DistribNormalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariance();


  /**
   * Returns the XML element name of this DistribNormalDistribution object.
   *
   * For DistribNormalDistribution, the XML element name is always
   * @c "normalDistribution".
   *
   * @return the name of this element, i.e. @c "normalDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribNormalDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_NORMALDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribNormalDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribNormalDistribution have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribNormalDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribNormalDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribNormalDistribution object are:
   * @li "mean"
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * Predicate returning @c true if this DistribNormalDistribution's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribNormalDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * DistribNormalDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribNormalDistribution.
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
   * DistribNormalDistribution.
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
   * Returns the number of "elementName" in this DistribNormalDistribution.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribNormalDistribution.
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
 * Creates a new DistribNormalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribNormalDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribNormalDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribNormalDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
DistribNormalDistribution_t *
DistribNormalDistribution_create(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribNormalDistribution_t object.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @return a (deep) copy of this DistribNormalDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
DistribNormalDistribution_t*
DistribNormalDistribution_clone(const DistribNormalDistribution_t* dnd);


/**
 * Frees this DistribNormalDistribution_t object.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
void
DistribNormalDistribution_free(DistribNormalDistribution_t* dnd);


/**
 * Returns the value of the "mean" element of this DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure whose mean is sought.
 *
 * @return the value of the "mean" element of this DistribNormalDistribution_t
 * as a DistribUncertValue*.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNormalDistribution_getMean(const DistribNormalDistribution_t * dnd);


/**
 * Returns the value of the "stddev" element of this
 * DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure whose stddev is sought.
 *
 * @return the value of the "stddev" element of this
 * DistribNormalDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNormalDistribution_getStddev(const DistribNormalDistribution_t * dnd);


/**
 * Returns the value of the "variance" element of this
 * DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure whose variance is
 * sought.
 *
 * @return the value of the "variance" element of this
 * DistribNormalDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNormalDistribution_getVariance(const DistribNormalDistribution_t * dnd);


/**
 * Predicate returning @c 1 (true) if this DistribNormalDistribution_t's "mean"
 * element is set.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribNormalDistribution_t's "mean" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_isSetMean(const DistribNormalDistribution_t * dnd);


/**
 * Predicate returning @c 1 (true) if this DistribNormalDistribution_t's
 * "stddev" element is set.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribNormalDistribution_t's "stddev" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_isSetStddev(const DistribNormalDistribution_t * dnd);


/**
 * Predicate returning @c 1 (true) if this DistribNormalDistribution_t's
 * "variance" element is set.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribNormalDistribution_t's "variance" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_isSetVariance(const DistribNormalDistribution_t *
  dnd);


/**
 * Sets the value of the "mean" element of this DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @param mean DistribUncertValue_t* value of the "mean" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_setMean(DistribNormalDistribution_t * dnd,
                                  const DistribUncertValue_t* mean);


/**
 * Sets the value of the "stddev" element of this DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @param stddev DistribUncertValue_t* value of the "stddev" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_setStddev(DistribNormalDistribution_t * dnd,
                                    const DistribUncertValue_t* stddev);


/**
 * Sets the value of the "variance" element of this
 * DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @param variance DistribUncertValue_t* value of the "variance" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_setVariance(DistribNormalDistribution_t * dnd,
                                      const DistribUncertValue_t* variance);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dnd the DistribNormalDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNormalDistribution_createMean(DistribNormalDistribution_t* dnd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dnd the DistribNormalDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNormalDistribution_createStddev(DistribNormalDistribution_t* dnd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dnd the DistribNormalDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNormalDistribution_createVariance(DistribNormalDistribution_t* dnd);


/**
 * Unsets the value of the "mean" element of this DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_unsetMean(DistribNormalDistribution_t * dnd);


/**
 * Unsets the value of the "stddev" element of this
 * DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_unsetStddev(DistribNormalDistribution_t * dnd);


/**
 * Unsets the value of the "variance" element of this
 * DistribNormalDistribution_t.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_unsetVariance(DistribNormalDistribution_t * dnd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribNormalDistribution_t object have been set.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribNormalDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_hasRequiredAttributes(const
  DistribNormalDistribution_t * dnd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribNormalDistribution_t object have been set.
 *
 * @param dnd the DistribNormalDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribNormalDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribNormalDistribution_t object are:
 * @li "mean"
 *
 * @memberof DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_hasRequiredElements(const DistribNormalDistribution_t
  * dnd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribNormalDistribution_H__ */


