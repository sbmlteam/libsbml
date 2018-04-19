/**
 * @file DistribGeometricDistribution.h
 * @brief Definition of the DistribGeometricDistribution class.
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
 * @class DistribGeometricDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribGeometricDistribution
 * class.
 */


#ifndef DistribGeometricDistribution_H__
#define DistribGeometricDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribDiscreteUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribGeometricDistribution : public
  DistribDiscreteUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mProbability;

  /** @endcond */

public:

  /**
   * Creates a new DistribGeometricDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribGeometricDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribGeometricDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribGeometricDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribGeometricDistribution(
                               unsigned int level =
                                 DistribExtension::getDefaultLevel(),
                               unsigned int version =
                                 DistribExtension::getDefaultVersion(),
                               unsigned int pkgVersion =
                                 DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribGeometricDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribGeometricDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribGeometricDistribution.
   *
   * @param orig the DistribGeometricDistribution instance to copy.
   */
  DistribGeometricDistribution(const DistribGeometricDistribution& orig);


  /**
   * Assignment operator for DistribGeometricDistribution.
   *
   * @param rhs the DistribGeometricDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribGeometricDistribution& operator=(const DistribGeometricDistribution&
    rhs);


  /**
   * Creates and returns a deep copy of this DistribGeometricDistribution
   * object.
   *
   * @return a (deep) copy of this DistribGeometricDistribution object.
   */
  virtual DistribGeometricDistribution* clone() const;


  /**
   * Destructor for DistribGeometricDistribution.
   */
  virtual ~DistribGeometricDistribution();


  /**
   * Returns the value of the "id" attribute of this
   * DistribGeometricDistribution.
   *
   * @return the value of the "id" attribute of this
   * DistribGeometricDistribution as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribGeometricDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribGeometricDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribGeometricDistribution's "id"
   * attribute is set.
   *
   * @return @c true if this DistribGeometricDistribution's "id" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribGeometricDistribution's "name"
   * attribute is set.
   *
   * @return @c true if this DistribGeometricDistribution's "name" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribGeometricDistribution.
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
   * Sets the value of the "name" attribute of this
   * DistribGeometricDistribution.
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
   * Unsets the value of the "id" attribute of this
   * DistribGeometricDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribGeometricDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "probability" element of this
   * DistribGeometricDistribution.
   *
   * @return the value of the "probability" element of this
   * DistribGeometricDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getProbability() const;


  /**
   * Returns the value of the "probability" element of this
   * DistribGeometricDistribution.
   *
   * @return the value of the "probability" element of this
   * DistribGeometricDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getProbability();


  /**
   * Predicate returning @c true if this DistribGeometricDistribution's
   * "probability" element is set.
   *
   * @return @c true if this DistribGeometricDistribution's "probability"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetProbability() const;


  /**
   * Sets the value of the "probability" element of this
   * DistribGeometricDistribution.
   *
   * @param probability DistribUncertValue* value of the "probability" element
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setProbability(const DistribUncertValue* probability);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribGeometricDistribution object and returns the DistribUncertValue
   * object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createProbability();


  /**
   * Unsets the value of the "probability" element of this
   * DistribGeometricDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetProbability();


  /**
   * Returns the XML element name of this DistribGeometricDistribution object.
   *
   * For DistribGeometricDistribution, the XML element name is always
   * @c "geometricDistribution".
   *
   * @return the name of this element, i.e. @c "geometricDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribGeometricDistribution
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_GEOMETRICLDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribGeometricDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribGeometricDistribution have been set, otherwise @c false is
   * returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribGeometricDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribGeometricDistribution have been set, otherwise @c false is
   * returned.
   *
   *
   * @note The required elements for the DistribGeometricDistribution object
   * are:
   * @li "probability"
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * Predicate returning @c true if this DistribGeometricDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribGeometricDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
   * Returns the number of "elementName" in this DistribGeometricDistribution.
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
   * DistribGeometricDistribution.
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
 * Creates a new DistribGeometricDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribGeometricDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribGeometricDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribGeometricDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
DistribGeometricDistribution_t *
DistribGeometricDistribution_create(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribGeometricDistribution_t
 * object.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @return a (deep) copy of this DistribGeometricDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
DistribGeometricDistribution_t*
DistribGeometricDistribution_clone(const DistribGeometricDistribution_t* dgd);


/**
 * Frees this DistribGeometricDistribution_t object.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
void
DistribGeometricDistribution_free(DistribGeometricDistribution_t* dgd);


/**
 * Returns the value of the "id" attribute of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this
 * DistribGeometricDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
char *
DistribGeometricDistribution_getId(const DistribGeometricDistribution_t * dgd);


/**
 * Returns the value of the "name" attribute of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure whose name is
 * sought.
 *
 * @return the value of the "name" attribute of this
 * DistribGeometricDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
char *
DistribGeometricDistribution_getName(const DistribGeometricDistribution_t *
  dgd);


/**
 * Predicate returning @c 1 (true) if this DistribGeometricDistribution_t's
 * "id" attribute is set.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribGeometricDistribution_t's "id" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_isSetId(const DistribGeometricDistribution_t *
  dgd);


/**
 * Predicate returning @c 1 (true) if this DistribGeometricDistribution_t's
 * "name" attribute is set.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribGeometricDistribution_t's "name"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_isSetName(const DistribGeometricDistribution_t *
  dgd);


/**
 * Sets the value of the "id" attribute of this DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribGeometricDistribution_unsetId().
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_setId(DistribGeometricDistribution_t * dgd,
                                   const char * id);


/**
 * Sets the value of the "name" attribute of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribGeometricDistribution_unsetName().
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_setName(DistribGeometricDistribution_t * dgd,
                                     const char * name);


/**
 * Unsets the value of the "id" attribute of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_unsetId(DistribGeometricDistribution_t * dgd);


/**
 * Unsets the value of the "name" attribute of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_unsetName(DistribGeometricDistribution_t * dgd);


/**
 * Returns the value of the "probability" element of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure whose probability is
 * sought.
 *
 * @return the value of the "probability" element of this
 * DistribGeometricDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribGeometricDistribution_getProbability(const
  DistribGeometricDistribution_t * dgd);


/**
 * Predicate returning @c 1 (true) if this DistribGeometricDistribution_t's
 * "probability" element is set.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribGeometricDistribution_t's "probability"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_isSetProbability(const
  DistribGeometricDistribution_t * dgd);


/**
 * Sets the value of the "probability" element of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @param probability DistribUncertValue_t* value of the "probability" element
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_setProbability(
                                            DistribGeometricDistribution_t *
                                              dgd,
                                            const DistribUncertValue_t*
                                              probability);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribGeometricDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dgd the DistribGeometricDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribGeometricDistribution_createProbability(DistribGeometricDistribution_t*
  dgd);


/**
 * Unsets the value of the "probability" element of this
 * DistribGeometricDistribution_t.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_unsetProbability(DistribGeometricDistribution_t *
  dgd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribGeometricDistribution_t object have been set.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribGeometricDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_hasRequiredAttributes(const
  DistribGeometricDistribution_t * dgd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribGeometricDistribution_t object have been set.
 *
 * @param dgd the DistribGeometricDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribGeometricDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribGeometricDistribution_t object
 * are:
 * @li "probability"
 *
 * @memberof DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribGeometricDistribution_hasRequiredElements(const
  DistribGeometricDistribution_t * dgd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribGeometricDistribution_H__ */


