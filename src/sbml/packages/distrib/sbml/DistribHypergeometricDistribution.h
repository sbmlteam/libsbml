/**
 * @file DistribHypergeometricDistribution.h
 * @brief Definition of the DistribHypergeometricDistribution class.
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
 * @class DistribHypergeometricDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribHypergeometricDistribution
 * class.
 */


#ifndef DistribHypergeometricDistribution_H__
#define DistribHypergeometricDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribDiscreteUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribHypergeometricDistribution : public
  DistribDiscreteUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mNumberOfSuccesses;
  DistribUncertValue* mNumberOfTrials;
  DistribUncertValue* mPopulationSize;

  /** @endcond */

public:

  /**
   * Creates a new DistribHypergeometricDistribution using the given SBML
   * Level, Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribHypergeometricDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribHypergeometricDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribHypergeometricDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribHypergeometricDistribution(
                                    unsigned int level =
                                      DistribExtension::getDefaultLevel(),
                                    unsigned int version =
                                      DistribExtension::getDefaultVersion(),
                                    unsigned int pkgVersion =
                                      DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribHypergeometricDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribHypergeometricDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribHypergeometricDistribution.
   *
   * @param orig the DistribHypergeometricDistribution instance to copy.
   */
  DistribHypergeometricDistribution(const DistribHypergeometricDistribution&
    orig);


  /**
   * Assignment operator for DistribHypergeometricDistribution.
   *
   * @param rhs the DistribHypergeometricDistribution object whose values are
   * to be used as the basis of the assignment.
   */
  DistribHypergeometricDistribution& operator=(const
    DistribHypergeometricDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribHypergeometricDistribution
   * object.
   *
   * @return a (deep) copy of this DistribHypergeometricDistribution object.
   */
  virtual DistribHypergeometricDistribution* clone() const;


  /**
   * Destructor for DistribHypergeometricDistribution.
   */
  virtual ~DistribHypergeometricDistribution();


  /**
   * Returns the value of the "numberOfSuccesses" element of this
   * DistribHypergeometricDistribution.
   *
   * @return the value of the "numberOfSuccesses" element of this
   * DistribHypergeometricDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getNumberOfSuccesses() const;


  /**
   * Returns the value of the "numberOfSuccesses" element of this
   * DistribHypergeometricDistribution.
   *
   * @return the value of the "numberOfSuccesses" element of this
   * DistribHypergeometricDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getNumberOfSuccesses();


  /**
   * Returns the value of the "numberOfTrials" element of this
   * DistribHypergeometricDistribution.
   *
   * @return the value of the "numberOfTrials" element of this
   * DistribHypergeometricDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getNumberOfTrials() const;


  /**
   * Returns the value of the "numberOfTrials" element of this
   * DistribHypergeometricDistribution.
   *
   * @return the value of the "numberOfTrials" element of this
   * DistribHypergeometricDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getNumberOfTrials();


  /**
   * Returns the value of the "populationSize" element of this
   * DistribHypergeometricDistribution.
   *
   * @return the value of the "populationSize" element of this
   * DistribHypergeometricDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getPopulationSize() const;


  /**
   * Returns the value of the "populationSize" element of this
   * DistribHypergeometricDistribution.
   *
   * @return the value of the "populationSize" element of this
   * DistribHypergeometricDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getPopulationSize();


  /**
   * Predicate returning @c true if this DistribHypergeometricDistribution's
   * "numberOfSuccesses" element is set.
   *
   * @return @c true if this DistribHypergeometricDistribution's
   * "numberOfSuccesses" element has been set, otherwise @c false is returned.
   */
  bool isSetNumberOfSuccesses() const;


  /**
   * Predicate returning @c true if this DistribHypergeometricDistribution's
   * "numberOfTrials" element is set.
   *
   * @return @c true if this DistribHypergeometricDistribution's
   * "numberOfTrials" element has been set, otherwise @c false is returned.
   */
  bool isSetNumberOfTrials() const;


  /**
   * Predicate returning @c true if this DistribHypergeometricDistribution's
   * "populationSize" element is set.
   *
   * @return @c true if this DistribHypergeometricDistribution's
   * "populationSize" element has been set, otherwise @c false is returned.
   */
  bool isSetPopulationSize() const;


  /**
   * Sets the value of the "numberOfSuccesses" element of this
   * DistribHypergeometricDistribution.
   *
   * @param numberOfSuccesses DistribUncertValue* value of the
   * "numberOfSuccesses" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setNumberOfSuccesses(const DistribUncertValue* numberOfSuccesses);


  /**
   * Sets the value of the "numberOfTrials" element of this
   * DistribHypergeometricDistribution.
   *
   * @param numberOfTrials DistribUncertValue* value of the "numberOfTrials"
   * element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setNumberOfTrials(const DistribUncertValue* numberOfTrials);


  /**
   * Sets the value of the "populationSize" element of this
   * DistribHypergeometricDistribution.
   *
   * @param populationSize DistribUncertValue* value of the "populationSize"
   * element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setPopulationSize(const DistribUncertValue* populationSize);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribHypergeometricDistribution object and returns the
   * DistribUncertValue object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createNumberOfSuccesses();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribHypergeometricDistribution object and returns the
   * DistribUncertValue object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createNumberOfTrials();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribHypergeometricDistribution object and returns the
   * DistribUncertValue object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createPopulationSize();


  /**
   * Unsets the value of the "numberOfSuccesses" element of this
   * DistribHypergeometricDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetNumberOfSuccesses();


  /**
   * Unsets the value of the "numberOfTrials" element of this
   * DistribHypergeometricDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetNumberOfTrials();


  /**
   * Unsets the value of the "populationSize" element of this
   * DistribHypergeometricDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetPopulationSize();


  /**
   * Returns the XML element name of this DistribHypergeometricDistribution
   * object.
   *
   * For DistribHypergeometricDistribution, the XML element name is always
   * @c "hypergeometricDistribution".
   *
   * @return the name of this element, i.e. @c "hypergeometricDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribHypergeometricDistribution
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_HYPERGEOMETRICDISTRIBUTION,
   * SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribHypergeometricDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribHypergeometricDistribution have been set, otherwise @c false is
   * returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribHypergeometricDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribHypergeometricDistribution have been set, otherwise @c false is
   * returned.
   *
   *
   * @note The required elements for the DistribHypergeometricDistribution
   * object are:
   * @li "numberOfSuccesses"
   * @li "numberOfTrials"
   * @li "populationSize"
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * Predicate returning @c true if this DistribHypergeometricDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribHypergeometricDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribHypergeometricDistribution.
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
   * DistribHypergeometricDistribution.
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
   * Returns the number of "elementName" in this
   * DistribHypergeometricDistribution.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this
   * DistribHypergeometricDistribution.
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
 * Creates a new DistribHypergeometricDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribHypergeometricDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribHypergeometricDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribHypergeometricDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t *
DistribHypergeometricDistribution_create(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribHypergeometricDistribution_t
 * object.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @return a (deep) copy of this DistribHypergeometricDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t*
DistribHypergeometricDistribution_clone(const
  DistribHypergeometricDistribution_t* dhd);


/**
 * Frees this DistribHypergeometricDistribution_t object.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
void
DistribHypergeometricDistribution_free(DistribHypergeometricDistribution_t*
  dhd);


/**
 * Returns the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure whose
 * numberOfSuccesses is sought.
 *
 * @return the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribHypergeometricDistribution_getNumberOfSuccesses(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Returns the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure whose
 * numberOfTrials is sought.
 *
 * @return the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribHypergeometricDistribution_getNumberOfTrials(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Returns the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure whose
 * populationSize is sought.
 *
 * @return the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribHypergeometricDistribution_getPopulationSize(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Predicate returning @c 1 (true) if this
 * DistribHypergeometricDistribution_t's "numberOfSuccesses" element is set.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribHypergeometricDistribution_t's
 * "numberOfSuccesses" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_isSetNumberOfSuccesses(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Predicate returning @c 1 (true) if this
 * DistribHypergeometricDistribution_t's "numberOfTrials" element is set.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribHypergeometricDistribution_t's
 * "numberOfTrials" element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_isSetNumberOfTrials(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Predicate returning @c 1 (true) if this
 * DistribHypergeometricDistribution_t's "populationSize" element is set.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribHypergeometricDistribution_t's
 * "populationSize" element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_isSetPopulationSize(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Sets the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @param numberOfSuccesses DistribUncertValue_t* value of the
 * "numberOfSuccesses" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_setNumberOfSuccesses(
                                                       DistribHypergeometricDistribution_t
                                                         * dhd,
                                                       const
                                                         DistribUncertValue_t*
                                                           numberOfSuccesses);


/**
 * Sets the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @param numberOfTrials DistribUncertValue_t* value of the "numberOfTrials"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_setNumberOfTrials(
                                                    DistribHypergeometricDistribution_t
                                                      * dhd,
                                                    const DistribUncertValue_t*
                                                      numberOfTrials);


/**
 * Sets the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @param populationSize DistribUncertValue_t* value of the "populationSize"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_setPopulationSize(
                                                    DistribHypergeometricDistribution_t
                                                      * dhd,
                                                    const DistribUncertValue_t*
                                                      populationSize);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribHypergeometricDistribution_t object and returns the
 * DistribUncertValue_t object created.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribHypergeometricDistribution_createNumberOfSuccesses(DistribHypergeometricDistribution_t*
  dhd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribHypergeometricDistribution_t object and returns the
 * DistribUncertValue_t object created.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribHypergeometricDistribution_createNumberOfTrials(DistribHypergeometricDistribution_t*
  dhd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribHypergeometricDistribution_t object and returns the
 * DistribUncertValue_t object created.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribHypergeometricDistribution_createPopulationSize(DistribHypergeometricDistribution_t*
  dhd);


/**
 * Unsets the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_unsetNumberOfSuccesses(DistribHypergeometricDistribution_t
  * dhd);


/**
 * Unsets the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_unsetNumberOfTrials(DistribHypergeometricDistribution_t
  * dhd);


/**
 * Unsets the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_unsetPopulationSize(DistribHypergeometricDistribution_t
  * dhd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribHypergeometricDistribution_t object have been set.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribHypergeometricDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_hasRequiredAttributes(const
  DistribHypergeometricDistribution_t * dhd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribHypergeometricDistribution_t object have been set.
 *
 * @param dhd the DistribHypergeometricDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribHypergeometricDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribHypergeometricDistribution_t
 * object are:
 * @li "numberOfSuccesses"
 * @li "numberOfTrials"
 * @li "populationSize"
 *
 * @memberof DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_hasRequiredElements(const
  DistribHypergeometricDistribution_t * dhd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribHypergeometricDistribution_H__ */


