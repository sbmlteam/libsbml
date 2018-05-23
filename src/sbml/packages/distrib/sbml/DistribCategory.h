/**
 * @file DistribCategory.h
 * @brief Definition of the DistribCategory class.
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
 * @class DistribCategory
 * @sbmlbrief{distrib} TODO:Definition of the DistribCategory class.
 */


#ifndef DistribCategory_H__
#define DistribCategory_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>
#include <sbml/packages/distrib/sbml/DistribBase.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribCategory : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  unsigned int mRank;
  bool mIsSetRank;
  DistribUncertValue* mProbability;
  DistribUncertValue* mValue;

  /** @endcond */

public:

  /**
   * Creates a new DistribCategory using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribCategory.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribCategory.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribCategory.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribCategory(unsigned int level = DistribExtension::getDefaultLevel(),
                  unsigned int version = DistribExtension::getDefaultVersion(),
                  unsigned int pkgVersion =
                    DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribCategory using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribCategory(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribCategory.
   *
   * @param orig the DistribCategory instance to copy.
   */
  DistribCategory(const DistribCategory& orig);


  /**
   * Assignment operator for DistribCategory.
   *
   * @param rhs the DistribCategory object whose values are to be used as the
   * basis of the assignment.
   */
  DistribCategory& operator=(const DistribCategory& rhs);


  /**
   * Creates and returns a deep copy of this DistribCategory object.
   *
   * @return a (deep) copy of this DistribCategory object.
   */
  virtual DistribCategory* clone() const;


  /**
   * Destructor for DistribCategory.
   */
  virtual ~DistribCategory();


  /**
   * Returns the value of the "id" attribute of this DistribCategory.
   *
   * @return the value of the "id" attribute of this DistribCategory as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribCategory.
   *
   * @return the value of the "name" attribute of this DistribCategory as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "rank" attribute of this DistribCategory.
   *
   * @return the value of the "rank" attribute of this DistribCategory as a
   * unsigned integer.
   */
  unsigned int getRank() const;


  /**
   * Predicate returning @c true if this DistribCategory's "id" attribute is
   * set.
   *
   * @return @c true if this DistribCategory's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribCategory's "name" attribute is
   * set.
   *
   * @return @c true if this DistribCategory's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this DistribCategory's "rank" attribute is
   * set.
   *
   * @return @c true if this DistribCategory's "rank" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRank() const;


  /**
   * Sets the value of the "id" attribute of this DistribCategory.
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
   * Sets the value of the "name" attribute of this DistribCategory.
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
   * Sets the value of the "rank" attribute of this DistribCategory.
   *
   * @param rank unsigned int value of the "rank" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRank(unsigned int rank);


  /**
   * Unsets the value of the "id" attribute of this DistribCategory.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribCategory.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "rank" attribute of this DistribCategory.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRank();


  /**
   * Returns the value of the "probability" element of this DistribCategory.
   *
   * @return the value of the "probability" element of this DistribCategory as
   * a DistribUncertValue*.
   */
  const DistribUncertValue* getProbability() const;


  /**
   * Returns the value of the "probability" element of this DistribCategory.
   *
   * @return the value of the "probability" element of this DistribCategory as
   * a DistribUncertValue*.
   */
  DistribUncertValue* getProbability();


  /**
   * Returns the value of the "value" element of this DistribCategory.
   *
   * @return the value of the "value" element of this DistribCategory as a
   * DistribUncertValue.
   */
  const DistribUncertValue* getValue() const;


  /**
   * Returns the value of the "value" element of this DistribCategory.
   *
   * @return the value of the "value" element of this DistribCategory as a
   * DistribUncertValue.
   */
  DistribUncertValue* getValue();


  /**
   * Predicate returning @c true if this DistribCategory's "probability"
   * element is set.
   *
   * @return @c true if this DistribCategory's "probability" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetProbability() const;


  /**
   * Predicate returning @c true if this DistribCategory's "value" element is
   * set.
   *
   * @return @c true if this DistribCategory's "value" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Sets the value of the "probability" element of this DistribCategory.
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
   * Sets the value of the "value" element of this DistribCategory.
   *
   * @param value DistribUncertValue* value of the "value" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setValue(const DistribUncertValue* value);


  /**
   * Creates a new DistribUncertValue object, adds it to this DistribCategory
   * object and returns the DistribUncertValue object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createProbability();


  /**
   * Creates a new DistribUncertValue object, adds it to this DistribCategory
   * object and returns the DistribUncertValue object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createValue();


  /**
   * Unsets the value of the "probability" element of this DistribCategory.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetProbability();


  /**
   * Unsets the value of the "value" element of this DistribCategory.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Returns the XML element name of this DistribCategory object.
   *
   * For DistribCategory, the XML element name is always @c "category".
   *
   * @return the name of this element, i.e. @c "category".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribCategory object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_CATEGORY, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribCategory object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribCategory have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribCategory object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribCategory have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribCategory object are:
   * @li "value"
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
   * Gets the value of the "attributeName" attribute of this DistribCategory.
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
   * Gets the value of the "attributeName" attribute of this DistribCategory.
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
   * Gets the value of the "attributeName" attribute of this DistribCategory.
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
   * Gets the value of the "attributeName" attribute of this DistribCategory.
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
   * Gets the value of the "attributeName" attribute of this DistribCategory.
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
   * Predicate returning @c true if this DistribCategory's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribCategory's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DistribCategory.
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
   * Sets the value of the "attributeName" attribute of this DistribCategory.
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
   * Sets the value of the "attributeName" attribute of this DistribCategory.
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
   * Sets the value of the "attributeName" attribute of this DistribCategory.
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
   * Sets the value of the "attributeName" attribute of this DistribCategory.
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
   * Unsets the value of the "attributeName" attribute of this DistribCategory.
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
   * Creates and returns an new "elementName" object in this DistribCategory.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribCategory.
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
   * DistribCategory.
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
   * Returns the number of "elementName" in this DistribCategory.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribCategory.
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
 * Creates a new DistribCategory_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribCategory_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribCategory_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribCategory_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
DistribCategory_t *
DistribCategory_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribCategory_t object.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return a (deep) copy of this DistribCategory_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategory_clone(const DistribCategory_t* dc);


/**
 * Frees this DistribCategory_t object.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
void
DistribCategory_free(DistribCategory_t* dc);


/**
 * Returns the value of the "id" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribCategory_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
char *
DistribCategory_getId(const DistribCategory_t * dc);


/**
 * Returns the value of the "name" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribCategory_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
char *
DistribCategory_getName(const DistribCategory_t * dc);


/**
 * Returns the value of the "rank" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure whose rank is sought.
 *
 * @return the value of the "rank" attribute of this DistribCategory_t as a
 * unsigned integer.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
unsigned int
DistribCategory_getRank(const DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribCategory_t's "id" attribute
 * is set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) if this DistribCategory_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_isSetId(const DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribCategory_t's "name" attribute
 * is set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) if this DistribCategory_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_isSetName(const DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribCategory_t's "rank" attribute
 * is set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) if this DistribCategory_t's "rank" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_isSetRank(const DistribCategory_t * dc);


/**
 * Sets the value of the "id" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribCategory_unsetId().
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_setId(DistribCategory_t * dc, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribCategory_unsetName().
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_setName(DistribCategory_t * dc, const char * name);


/**
 * Sets the value of the "rank" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @param rank unsigned int value of the "rank" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_setRank(DistribCategory_t * dc, unsigned int rank);


/**
 * Unsets the value of the "id" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_unsetId(DistribCategory_t * dc);


/**
 * Unsets the value of the "name" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_unsetName(DistribCategory_t * dc);


/**
 * Unsets the value of the "rank" attribute of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_unsetRank(DistribCategory_t * dc);


/**
 * Returns the value of the "probability" element of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure whose probability is sought.
 *
 * @return the value of the "probability" element of this DistribCategory_t as
 * a DistribUncertValue*.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCategory_getProbability(const DistribCategory_t * dc);


/**
 * Returns the value of the "value" element of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure whose value is sought.
 *
 * @return the value of the "value" element of this DistribCategory_t as a
 * DistribUncertValue.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCategory_getValue(const DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribCategory_t's "probability"
 * element is set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) if this DistribCategory_t's "probability" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_isSetProbability(const DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribCategory_t's "value" element
 * is set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) if this DistribCategory_t's "value" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_isSetValue(const DistribCategory_t * dc);


/**
 * Sets the value of the "probability" element of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @param probability DistribUncertValue_t* value of the "probability" element
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_setProbability(DistribCategory_t * dc,
                               const DistribUncertValue_t* probability);


/**
 * Sets the value of the "value" element of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @param value DistribUncertValue_t* value of the "value" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_setValue(DistribCategory_t * dc,
                         const DistribUncertValue_t* value);


/**
 * Creates a new DistribUncertValue_t object, adds it to this DistribCategory_t
 * object and returns the DistribUncertValue_t object created.
 *
 * @param dc the DistribCategory_t structure to which the DistribUncertValue_t
 * should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCategory_createProbability(DistribCategory_t* dc);


/**
 * Creates a new DistribUncertValue_t object, adds it to this DistribCategory_t
 * object and returns the DistribUncertValue_t object created.
 *
 * @param dc the DistribCategory_t structure to which the DistribUncertValue_t
 * should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCategory_createValue(DistribCategory_t* dc);


/**
 * Unsets the value of the "probability" element of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_unsetProbability(DistribCategory_t * dc);


/**
 * Unsets the value of the "value" element of this DistribCategory_t.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_unsetValue(DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCategory_t object have been set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribCategory_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_hasRequiredAttributes(const DistribCategory_t * dc);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribCategory_t object have been set.
 *
 * @param dc the DistribCategory_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribCategory_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the DistribCategory_t object are:
 * @li "value"
 *
 * @memberof DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribCategory_hasRequiredElements(const DistribCategory_t * dc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribCategory_H__ */


