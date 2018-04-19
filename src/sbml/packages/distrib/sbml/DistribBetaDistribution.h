/**
 * @file DistribBetaDistribution.h
 * @brief Definition of the DistribBetaDistribution class.
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
 * @class DistribBetaDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribBetaDistribution class.
 */


#ifndef DistribBetaDistribution_H__
#define DistribBetaDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribContinuousUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribBetaDistribution : public
  DistribContinuousUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mAlpha;
  DistribUncertValue* mBeta;

  /** @endcond */

public:

  /**
   * Creates a new DistribBetaDistribution using the given SBML Level, Version
   * and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribBetaDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribBetaDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribBetaDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribBetaDistribution(
                          unsigned int level =
                            DistribExtension::getDefaultLevel(),
                          unsigned int version =
                            DistribExtension::getDefaultVersion(),
                          unsigned int pkgVersion =
                            DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribBetaDistribution using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribBetaDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribBetaDistribution.
   *
   * @param orig the DistribBetaDistribution instance to copy.
   */
  DistribBetaDistribution(const DistribBetaDistribution& orig);


  /**
   * Assignment operator for DistribBetaDistribution.
   *
   * @param rhs the DistribBetaDistribution object whose values are to be used
   * as the basis of the assignment.
   */
  DistribBetaDistribution& operator=(const DistribBetaDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribBetaDistribution object.
   *
   * @return a (deep) copy of this DistribBetaDistribution object.
   */
  virtual DistribBetaDistribution* clone() const;


  /**
   * Destructor for DistribBetaDistribution.
   */
  virtual ~DistribBetaDistribution();


  /**
   * Returns the value of the "id" attribute of this DistribBetaDistribution.
   *
   * @return the value of the "id" attribute of this DistribBetaDistribution as
   * a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribBetaDistribution.
   *
   * @return the value of the "name" attribute of this DistribBetaDistribution
   * as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribBetaDistribution's "id"
   * attribute is set.
   *
   * @return @c true if this DistribBetaDistribution's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribBetaDistribution's "name"
   * attribute is set.
   *
   * @return @c true if this DistribBetaDistribution's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribBetaDistribution.
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
   * Sets the value of the "name" attribute of this DistribBetaDistribution.
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
   * Unsets the value of the "id" attribute of this DistribBetaDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribBetaDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "alpha" element of this DistribBetaDistribution.
   *
   * @return the value of the "alpha" element of this DistribBetaDistribution
   * as a DistribUncertValue*.
   */
  const DistribUncertValue* getAlpha() const;


  /**
   * Returns the value of the "alpha" element of this DistribBetaDistribution.
   *
   * @return the value of the "alpha" element of this DistribBetaDistribution
   * as a DistribUncertValue*.
   */
  DistribUncertValue* getAlpha();


  /**
   * Returns the value of the "beta" element of this DistribBetaDistribution.
   *
   * @return the value of the "beta" element of this DistribBetaDistribution as
   * a DistribUncertValue*.
   */
  const DistribUncertValue* getBeta() const;


  /**
   * Returns the value of the "beta" element of this DistribBetaDistribution.
   *
   * @return the value of the "beta" element of this DistribBetaDistribution as
   * a DistribUncertValue*.
   */
  DistribUncertValue* getBeta();


  /**
   * Predicate returning @c true if this DistribBetaDistribution's "alpha"
   * element is set.
   *
   * @return @c true if this DistribBetaDistribution's "alpha" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetAlpha() const;


  /**
   * Predicate returning @c true if this DistribBetaDistribution's "beta"
   * element is set.
   *
   * @return @c true if this DistribBetaDistribution's "beta" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetBeta() const;


  /**
   * Sets the value of the "alpha" element of this DistribBetaDistribution.
   *
   * @param alpha DistribUncertValue* value of the "alpha" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setAlpha(const DistribUncertValue* alpha);


  /**
   * Sets the value of the "beta" element of this DistribBetaDistribution.
   *
   * @param beta DistribUncertValue* value of the "beta" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setBeta(const DistribUncertValue* beta);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribBetaDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createAlpha();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribBetaDistribution object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createBeta();


  /**
   * Unsets the value of the "alpha" element of this DistribBetaDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetAlpha();


  /**
   * Unsets the value of the "beta" element of this DistribBetaDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBeta();


  /**
   * Returns the XML element name of this DistribBetaDistribution object.
   *
   * For DistribBetaDistribution, the XML element name is always
   * @c "betaDistribution".
   *
   * @return the name of this element, i.e. @c "betaDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribBetaDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_BETADISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribBetaDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribBetaDistribution have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribBetaDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribBetaDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribBetaDistribution object are:
   * @li "alpha"
   * @li "beta"
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * Predicate returning @c true if this DistribBetaDistribution's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribBetaDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * DistribBetaDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribBetaDistribution.
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
   * DistribBetaDistribution.
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
   * Returns the number of "elementName" in this DistribBetaDistribution.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribBetaDistribution.
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
   * Reads the expected attributes into the member data variables
   */
  virtual void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readL3V2V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeL3V1V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeL3V2V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DistribBetaDistribution_t using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBetaDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBetaDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBetaDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
DistribBetaDistribution_t *
DistribBetaDistribution_create(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribBetaDistribution_t object.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return a (deep) copy of this DistribBetaDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
DistribBetaDistribution_t*
DistribBetaDistribution_clone(const DistribBetaDistribution_t* dbd);


/**
 * Frees this DistribBetaDistribution_t object.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
void
DistribBetaDistribution_free(DistribBetaDistribution_t* dbd);


/**
 * Returns the value of the "id" attribute of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribBetaDistribution_t as
 * a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
char *
DistribBetaDistribution_getId(const DistribBetaDistribution_t * dbd);


/**
 * Returns the value of the "name" attribute of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribBetaDistribution_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
char *
DistribBetaDistribution_getName(const DistribBetaDistribution_t * dbd);


/**
 * Predicate returning @c 1 (true) if this DistribBetaDistribution_t's "id"
 * attribute is set.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribBetaDistribution_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_isSetId(const DistribBetaDistribution_t * dbd);


/**
 * Predicate returning @c 1 (true) if this DistribBetaDistribution_t's "name"
 * attribute is set.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribBetaDistribution_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_isSetName(const DistribBetaDistribution_t * dbd);


/**
 * Sets the value of the "id" attribute of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribBetaDistribution_unsetId().
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_setId(DistribBetaDistribution_t * dbd,
                              const char * id);


/**
 * Sets the value of the "name" attribute of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribBetaDistribution_unsetName().
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_setName(DistribBetaDistribution_t * dbd,
                                const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_unsetId(DistribBetaDistribution_t * dbd);


/**
 * Unsets the value of the "name" attribute of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_unsetName(DistribBetaDistribution_t * dbd);


/**
 * Returns the value of the "alpha" element of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure whose alpha is sought.
 *
 * @return the value of the "alpha" element of this DistribBetaDistribution_t
 * as a DistribUncertValue*.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribBetaDistribution_getAlpha(const DistribBetaDistribution_t * dbd);


/**
 * Returns the value of the "beta" element of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure whose beta is sought.
 *
 * @return the value of the "beta" element of this DistribBetaDistribution_t as
 * a DistribUncertValue*.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribBetaDistribution_getBeta(const DistribBetaDistribution_t * dbd);


/**
 * Predicate returning @c 1 (true) if this DistribBetaDistribution_t's "alpha"
 * element is set.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribBetaDistribution_t's "alpha" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_isSetAlpha(const DistribBetaDistribution_t * dbd);


/**
 * Predicate returning @c 1 (true) if this DistribBetaDistribution_t's "beta"
 * element is set.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribBetaDistribution_t's "beta" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_isSetBeta(const DistribBetaDistribution_t * dbd);


/**
 * Sets the value of the "alpha" element of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @param alpha DistribUncertValue_t* value of the "alpha" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_setAlpha(DistribBetaDistribution_t * dbd,
                                 const DistribUncertValue_t* alpha);


/**
 * Sets the value of the "beta" element of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @param beta DistribUncertValue_t* value of the "beta" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_setBeta(DistribBetaDistribution_t * dbd,
                                const DistribUncertValue_t* beta);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribBetaDistribution_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dbd the DistribBetaDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribBetaDistribution_createAlpha(DistribBetaDistribution_t* dbd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribBetaDistribution_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dbd the DistribBetaDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribBetaDistribution_createBeta(DistribBetaDistribution_t* dbd);


/**
 * Unsets the value of the "alpha" element of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_unsetAlpha(DistribBetaDistribution_t * dbd);


/**
 * Unsets the value of the "beta" element of this DistribBetaDistribution_t.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_unsetBeta(DistribBetaDistribution_t * dbd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBetaDistribution_t object have been set.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribBetaDistribution_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_hasRequiredAttributes(const DistribBetaDistribution_t *
  dbd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribBetaDistribution_t object have been set.
 *
 * @param dbd the DistribBetaDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribBetaDistribution_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the DistribBetaDistribution_t object are:
 * @li "alpha"
 * @li "beta"
 *
 * @memberof DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBetaDistribution_hasRequiredElements(const DistribBetaDistribution_t *
  dbd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribBetaDistribution_H__ */


