/**
 * @file ExternalParameter.h
 * @brief Definition of the ExternalParameter class.
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
 * @class ExternalParameter
 * @sbmlbrief{distrib} TODO:Definition of the ExternalParameter class.
 */


#ifndef ExternalParameter_H__
#define ExternalParameter_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/UncertValue.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class ListOfExternalParameters;

class LIBSBML_EXTERN ExternalParameter : public UncertValue
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDefinitionURL;
  ListOfExternalParameters * mExternalParameters;

  /** @endcond */

public:

  /**
   * Creates a new ExternalParameter using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ExternalParameter.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ExternalParameter.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ExternalParameter.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ExternalParameter(unsigned int level = DistribExtension::getDefaultLevel(),
                    unsigned int version =
                      DistribExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ExternalParameter using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ExternalParameter(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ExternalParameter.
   *
   * @param orig the ExternalParameter instance to copy.
   */
  ExternalParameter(const ExternalParameter& orig);


  /**
   * Assignment operator for ExternalParameter.
   *
   * @param rhs the ExternalParameter object whose values are to be used as the
   * basis of the assignment.
   */
  ExternalParameter& operator=(const ExternalParameter& rhs);


  /**
   * Creates and returns a deep copy of this ExternalParameter object.
   *
   * @return a (deep) copy of this ExternalParameter object.
   */
  virtual ExternalParameter* clone() const;


  /**
   * Destructor for ExternalParameter.
   */
  virtual ~ExternalParameter();


  /**
   * Returns the value of the "definitionURL" attribute of this
   * ExternalParameter.
   *
   * @return the value of the "definitionURL" attribute of this
   * ExternalParameter as a string.
   */
  const std::string& getDefinitionURL() const;


  /**
   * Predicate returning @c true if this ExternalParameter's "definitionURL"
   * attribute is set.
   *
   * @return @c true if this ExternalParameter's "definitionURL" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetDefinitionURL() const;


  /**
   * Sets the value of the "definitionURL" attribute of this ExternalParameter.
   *
   * @param definitionURL std::string& value of the "definitionURL" attribute
   * to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p definitionURL = @c NULL or an empty string
   * is equivalent to calling unsetDefinitionURL().
   */
  int setDefinitionURL(const std::string& definitionURL);


  /**
   * Unsets the value of the "definitionURL" attribute of this
   * ExternalParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefinitionURL();


  /**
   * Returns the ListOfExternalParameters * from this ExternalParameter.
   *
   * @return the ListOfExternalParameters * from this ExternalParameter.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  const ListOfExternalParameters * getListOfExternalParameters() const;


  /**
   * Returns the ListOfExternalParameters * from this ExternalParameter.
   *
   * @return the ListOfExternalParameters * from this ExternalParameter.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  ListOfExternalParameters * getListOfExternalParameters();


  /**
   * Get an ExternalParameter from the ExternalParameter.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to retrieve.
   *
   * @return the nth ExternalParameter in the ListOfExternalParameters * within
   * this ExternalParameter.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  ExternalParameter* getExternalParameter(unsigned int n);


  /**
   * Get an ExternalParameter from the ExternalParameter.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to retrieve.
   *
   * @return the nth ExternalParameter in the ListOfExternalParameters * within
   * this ExternalParameter.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  const ExternalParameter* getExternalParameter(unsigned int n) const;


  /**
   * Adds a copy of the given ExternalParameter to this ExternalParameter.
   *
   * @param ep1 the ExternalParameter object to add.
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
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  int addExternalParameter(const ExternalParameter* ep1);


  /**
   * Get the number of ExternalParameter objects in this ExternalParameter.
   *
   * @return the number of ExternalParameter objects in this ExternalParameter.
   *
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  unsigned int getNumExternalParameters() const;


  /**
   * Creates a new ExternalParameter object, adds it to this ExternalParameter
   * object and returns the ExternalParameter object created.
   *
   * @return a new ExternalParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  ExternalParameter* createExternalParameter();


  /**
   * Removes the nth ExternalParameter from this ExternalParameter and returns
   * a pointer to it.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to remove.
   *
   * @return a pointer to the nth ExternalParameter in this ExternalParameter.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   */
  ExternalParameter* removeExternalParameter(unsigned int n);


  /**
   * Returns the XML element name of this ExternalParameter object.
   *
   * For ExternalParameter, the XML element name is always
   * @c "externalParameter".
   *
   * @return the name of this element, i.e. @c "externalParameter".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ExternalParameter object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_EXTERNALPARAMETER, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * ExternalParameter object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ExternalParameter have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the ExternalParameter object are:
   * @li "definitionURL"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * ExternalParameter object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * ExternalParameter have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the ExternalParameter object are:
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
   * Gets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Gets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Gets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Gets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Gets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Predicate returning @c true if this ExternalParameter's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this ExternalParameter's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Sets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Sets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Sets the value of the "attributeName" attribute of this ExternalParameter.
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
   * Sets the value of the "attributeName" attribute of this ExternalParameter.
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
   * ExternalParameter.
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
   * Creates and returns an new "elementName" object in this ExternalParameter.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this ExternalParameter.
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
   * ExternalParameter.
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
   * Returns the number of "elementName" in this ExternalParameter.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this ExternalParameter.
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
 * Creates a new ExternalParameter_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * ExternalParameter_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * ExternalParameter_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this ExternalParameter_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
ExternalParameter_t *
ExternalParameter_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this ExternalParameter_t object.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @return a (deep) copy of this ExternalParameter_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_clone(const ExternalParameter_t* ep);


/**
 * Frees this ExternalParameter_t object.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
void
ExternalParameter_free(ExternalParameter_t* ep);


/**
 * Returns the value of the "definitionURL" attribute of this
 * ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure whose definitionURL is sought.
 *
 * @return the value of the "definitionURL" attribute of this
 * ExternalParameter_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
char *
ExternalParameter_getDefinitionURL(const ExternalParameter_t * ep);


/**
 * Predicate returning @c 1 (true) if this ExternalParameter_t's
 * "definitionURL" attribute is set.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @return @c 1 (true) if this ExternalParameter_t's "definitionURL" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
int
ExternalParameter_isSetDefinitionURL(const ExternalParameter_t * ep);


/**
 * Sets the value of the "definitionURL" attribute of this ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @param definitionURL const char * value of the "definitionURL" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p definitionURL = @c NULL or an empty string is
 * equivalent to calling ExternalParameter_unsetDefinitionURL().
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
int
ExternalParameter_setDefinitionURL(ExternalParameter_t * ep,
                                   const char * definitionURL);


/**
 * Unsets the value of the "definitionURL" attribute of this
 * ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
int
ExternalParameter_unsetDefinitionURL(ExternalParameter_t * ep);


/**
 * Returns a ListOf_t * containing ExternalParameter_t objects from this
 * ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure whose ListOfExternalParameters *
 * is sought.
 *
 * @return the ListOfExternalParameters * from this ExternalParameter_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see ExternalParameter_addExternalParameter()
 * @see ExternalParameter_createExternalParameter()
 * @see ExternalParameter_getExternalParameterById()
 * @see ExternalParameter_getExternalParameter()
 * @see ExternalParameter_getNumExternalParameters()
 * @see ExternalParameter_removeExternalParameterById()
 * @see ExternalParameter_removeExternalParameter()
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
ListOf_t*
ExternalParameter_getListOfExternalParameters(ExternalParameter_t* ep);


/**
 * Get an ExternalParameter_t from the ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure to search.
 *
 * @param n an unsigned int representing the index of the ExternalParameter_t
 * to retrieve.
 *
 * @return the nth ExternalParameter_t in the ListOfExternalParameters * within
 * this ExternalParameter.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_getExternalParameter(ExternalParameter_t* ep,
                                       unsigned int n);


/**
 * Adds a copy of the given ExternalParameter_t to this ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure to which the ExternalParameter_t
 * should be added.
 *
 * @param ep1 the ExternalParameter_t object to add.
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
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
int
ExternalParameter_addExternalParameter(ExternalParameter_t* ep,
                                       const ExternalParameter_t* ep1);


/**
 * Get the number of ExternalParameter_t objects in this ExternalParameter_t.
 *
 * @param ep the ExternalParameter_t structure to query.
 *
 * @return the number of ExternalParameter_t objects in this
 * ExternalParameter_t.
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
unsigned int
ExternalParameter_getNumExternalParameters(ExternalParameter_t* ep);


/**
 * Creates a new ExternalParameter_t object, adds it to this
 * ExternalParameter_t object and returns the ExternalParameter_t object
 * created.
 *
 * @param ep the ExternalParameter_t structure to which the ExternalParameter_t
 * should be added.
 *
 * @return a new ExternalParameter_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_createExternalParameter(ExternalParameter_t* ep);


/**
 * Removes the nth ExternalParameter_t from this ExternalParameter_t and
 * returns a pointer to it.
 *
 * @param ep the ExternalParameter_t structure to search.
 *
 * @param n an unsigned int representing the index of the ExternalParameter_t
 * to remove.
 *
 * @return a pointer to the nth ExternalParameter_t in this
 * ExternalParameter_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
ExternalParameter_removeExternalParameter(ExternalParameter_t* ep,
                                          unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * ExternalParameter_t object have been set.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * ExternalParameter_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the ExternalParameter_t object are:
 * @li "definitionURL"
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
int
ExternalParameter_hasRequiredAttributes(const ExternalParameter_t * ep);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * ExternalParameter_t object have been set.
 *
 * @param ep the ExternalParameter_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * ExternalParameter_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the ExternalParameter_t object are:
 *
 * @memberof ExternalParameter_t
 */
LIBSBML_EXTERN
int
ExternalParameter_hasRequiredElements(const ExternalParameter_t * ep);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ExternalParameter_H__ */


