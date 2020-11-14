/**
 * @file ListOfGlobalRenderInformation.h
 * @brief Definition of the ListOfGlobalRenderInformation class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 * @class ListOfGlobalRenderInformation
 * @sbmlbrief{render} A list of GlobalRenderInformation objects.
 *
 * The ListOfGlobalRenderInformation is a container for the
 * GlobalRenderInformation elements of a ListOfLayouts object.
 *
 * @copydetails doc_what_is_listof
 *
 * @see GlobalRenderInformation
 */


#ifndef ListOfGlobalRenderInformation_H__
#define ListOfGlobalRenderInformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>
#include <sbml/util/ElementFilter.h>

#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/DefaultValues.h>
#include <sbml/packages/render/sbml/GlobalRenderInformation.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfGlobalRenderInformation : public ListOf
{
protected:

  /** @cond doxygenLibsbmlInternal */

  unsigned int mMajorVersion;
  bool mIsSetMajorVersion;
  unsigned int mMinorVersion;
  bool mIsSetMinorVersion;
  DefaultValues* mDefaultValues;

  /** @endcond */

public:

  /**
   * Creates a new ListOfGlobalRenderInformation using the given SBML Level,
   * Version and &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfGlobalRenderInformation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfGlobalRenderInformation.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfGlobalRenderInformation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGlobalRenderInformation(
                                unsigned int level =
                                  RenderExtension::getDefaultLevel(),
                                unsigned int version =
                                  RenderExtension::getDefaultVersion(),
                                unsigned int pkgVersion =
                                  RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGlobalRenderInformation using the given
   * RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGlobalRenderInformation(RenderPkgNamespaces *renderns);


  /**
   * Copy constructor for ListOfGlobalRenderInformation.
   *
   * @param orig the ListOfGlobalRenderInformation instance to copy.
   */
  ListOfGlobalRenderInformation(const ListOfGlobalRenderInformation& orig);


  /**
   * Assignment operator for ListOfGlobalRenderInformation.
   *
   * @param rhs the ListOfGlobalRenderInformation object whose values are to be
   * used as the basis of the assignment.
   */
  ListOfGlobalRenderInformation& operator=(const ListOfGlobalRenderInformation&
    rhs);


  /**
   * Creates and returns a deep copy of this ListOfGlobalRenderInformation
   * object.
   *
   * @return a (deep) copy of this ListOfGlobalRenderInformation object.
   */
  virtual ListOfGlobalRenderInformation* clone() const;


  /**
   * Destructor for ListOfGlobalRenderInformation.
   */
  virtual ~ListOfGlobalRenderInformation();


  /**
   * Returns the value of the "majorVersion" attribute of this
   * ListOfGlobalRenderInformation.
   *
   * @return the value of the "majorVersion" attribute of this
   * ListOfGlobalRenderInformation as a unsigned integer.
   */
  unsigned int getMajorVersion() const;


  /**
   * Returns the value of the "minorVersion" attribute of this
   * ListOfGlobalRenderInformation.
   *
   * @return the value of the "minorVersion" attribute of this
   * ListOfGlobalRenderInformation as a unsigned integer.
   */
  unsigned int getMinorVersion() const;


  /**
  * Returns the version as a string.
  *
  * @return the version of the GlobalRenderInformation object
  * as a string
  */
  std::string getVersionString() const;

  /**
   * Predicate returning @c true if this ListOfGlobalRenderInformation's
   * "majorVersion" attribute is set.
   *
   * @return @c true if this ListOfGlobalRenderInformation's "majorVersion"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetMajorVersion() const;


  /**
   * Predicate returning @c true if this ListOfGlobalRenderInformation's
   * "minorVersion" attribute is set.
   *
   * @return @c true if this ListOfGlobalRenderInformation's "minorVersion"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetMinorVersion() const;

  /** @cond doxygenLibsbmlInternal */

  /**
  * Predicate returning @c true if this ListOfGlobalRenderInformation's
  * "majorVersion" attribute is set.
  *
  * @return @c true if this ListOfGlobalRenderInformation's "majorVersion"
  * attribute has been set, otherwise @c false is returned.
  */
  bool isSetVersionMajor() const;


  /**
  * Predicate returning @c true if this ListOfGlobalRenderInformation's
  * "minorVersion" attribute is set.
  *
  * @return @c true if this ListOfGlobalRenderInformation's "minorVersion"
  * attribute has been set, otherwise @c false is returned.
  */
  bool isSetVersionMinor() const;

  /** @endcond */

  /**
   * Sets the value of the "majorVersion" attribute of this
   * ListOfGlobalRenderInformation.
   *
   * @param majorVersion unsigned int value of the "majorVersion" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMajorVersion(unsigned int majorVersion);


  /**
   * Sets the value of the "minorVersion" attribute of this
   * ListOfGlobalRenderInformation.
   *
   * @param minorVersion unsigned int value of the "minorVersion" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMinorVersion(unsigned int minorVersion);

  /** @cond doxygenLibsbmlInternal */
  /**
  * Sets the value of the "majorVersion" attribute of this
  * ListOfGlobalRenderInformation.
  *
  * @param majorVersion unsigned int value of the "majorVersion" attribute to
  * be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setVersionMajor(unsigned int majorVersion);


  /**
  * Sets the value of the "minorVersion" attribute of this
  * ListOfGlobalRenderInformation.
  *
  * @param minorVersion unsigned int value of the "minorVersion" attribute to
  * be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setVersionMinor(unsigned int minorVersion);


  /** @endcond */

  /**
  * Sets the version of the render information list.
  * The version consists of a major and a minor version number.
  *
  * @param major major version number
  * @param minor minor version number
  */
  void setVersion(unsigned int major, unsigned int minor);

  /**
   * Unsets the value of the "majorVersion" attribute of this
   * ListOfGlobalRenderInformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMajorVersion();


  /**
   * Unsets the value of the "minorVersion" attribute of this
   * ListOfGlobalRenderInformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMinorVersion();

  /** @cond doxygenLibsbmlInternal */

  /**
  * Unsets the value of the "majorVersion" attribute of this
  * ListOfGlobalRenderInformation.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
  */
  int unsetVersionMajor();


  /**
  * Unsets the value of the "minorVersion" attribute of this
  * ListOfGlobalRenderInformation.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
  */
  int unsetVersionMinor();

  /** @endcond */


  /**
   * Returns the value of the "defaultValues" element of this
   * ListOfGlobalRenderInformation.
   *
   * @return the value of the "defaultValues" element of this
   * ListOfGlobalRenderInformation as a DefaultValues.
   */
  const DefaultValues* getDefaultValues() const;


  /**
   * Returns the value of the "defaultValues" element of this
   * ListOfGlobalRenderInformation.
   *
   * @return the value of the "defaultValues" element of this
   * ListOfGlobalRenderInformation as a DefaultValues.
   */
  DefaultValues* getDefaultValues();


  /**
   * Predicate returning @c true if this ListOfGlobalRenderInformation's
   * "defaultValues" element is set.
   *
   * @return @c true if this ListOfGlobalRenderInformation's "defaultValues"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetDefaultValues() const;


  /**
   * Sets the value of the "defaultValues" element of this
   * ListOfGlobalRenderInformation.
   *
   * @param defaultValues DefaultValues value of the "defaultValues" element
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDefaultValues(const DefaultValues* defaultValues);


  /**
   * Creates a new DefaultValues object, adds it to this
   * ListOfGlobalRenderInformation object and returns the DefaultValues object
   * created.
   *
   * @return a new DefaultValues object instance.
   */
  DefaultValues* createDefaultValues();


  /**
   * Unsets the value of the "defaultValues" element of this
   * ListOfGlobalRenderInformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefaultValues();


  /**
   * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation.
   *
   * @param n an unsigned int representing the index of the
   * GlobalRenderInformation to retrieve.
   *
   * @return the nth GlobalRenderInformation in this
   * ListOfGlobalRenderInformation.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(const std::string& sid)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GlobalRenderInformation* get(unsigned int n);


  /**
   * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation.
   *
   * @param n an unsigned int representing the index of the
   * GlobalRenderInformation to retrieve.
   *
   * @return the nth GlobalRenderInformation in this
   * ListOfGlobalRenderInformation.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(const std::string& sid)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GlobalRenderInformation* get(unsigned int n) const;


  /**
   * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * GlobalRenderInformation to retrieve.
   *
   * @return the GlobalRenderInformation in this ListOfGlobalRenderInformation
   * with the given @p sid or @c NULL if no such GlobalRenderInformation
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(unsigned int n)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GlobalRenderInformation* get(const std::string& sid);


  /**
   * Get a GlobalRenderInformation from the ListOfGlobalRenderInformation based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * GlobalRenderInformation to retrieve.
   *
   * @return the GlobalRenderInformation in this ListOfGlobalRenderInformation
   * with the given @p sid or @c NULL if no such GlobalRenderInformation
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(unsigned int n)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GlobalRenderInformation* get(const std::string& sid) const;


  /**
   * Removes the nth GlobalRenderInformation from this
   * ListOfGlobalRenderInformation and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * GlobalRenderInformation to remove.
   *
   * @return a pointer to the nth GlobalRenderInformation in this
   * ListOfGlobalRenderInformation.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   */
  virtual GlobalRenderInformation* remove(unsigned int n);


  /**
   * Removes the GlobalRenderInformation from this
   * ListOfGlobalRenderInformation based on its identifier and returns a
   * pointer to it.
   *
   * @param sid a string representing the identifier of the
   * GlobalRenderInformation to remove.
   *
   * @return the GlobalRenderInformation in this ListOfGlobalRenderInformation
   * based on the identifier or NULL if no such GlobalRenderInformation exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalRenderInformation()
   * @see remove(unsigned int n)
   */
  virtual GlobalRenderInformation* remove(const std::string& sid);


  /**
   * Adds a copy of the given GlobalRenderInformation to this
   * ListOfGlobalRenderInformation.
   *
   * @param gri the GlobalRenderInformation object to add.
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
   * @see createGlobalRenderInformation()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addGlobalRenderInformation(const GlobalRenderInformation* gri);


  /**
   * Get the number of GlobalRenderInformation objects in this
   * ListOfGlobalRenderInformation.
   *
   * @return the number of GlobalRenderInformation objects in this
   * ListOfGlobalRenderInformation.
   *
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see createGlobalRenderInformation()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumGlobalRenderInformation() const;


  /**
   * Creates a new GlobalRenderInformation object, adds it to this
   * ListOfGlobalRenderInformation object and returns the
   * GlobalRenderInformation object created.
   *
   * @return a new GlobalRenderInformation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalRenderInformation(const GlobalRenderInformation* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalRenderInformation()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  GlobalRenderInformation* createGlobalRenderInformation();


  /**
   * Returns the XML element name of this ListOfGlobalRenderInformation object.
   *
   * For ListOfGlobalRenderInformation, the XML element name is always
   * @c "listOfGlobalRenderInformation".
   *
   * @return the name of this element, i.e. @c "listOfGlobalRenderInformation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfGlobalRenderInformation
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_LIST_OF, SBMLTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   */
  virtual int getTypeCode() const;


  /**
   * Returns the libSBML type code for the SBML objects contained in this
   * ListOfGlobalRenderInformation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfGlobalRenderInformation:
   * @sbmlconstant{SBML_RENDER_GLOBALRENDERINFORMATION, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;


  /**
  * Returns whether the given item can be added to this ListOfGlobalRenderInformation.
  *
  * @param item the SBase element to check.
  *
  * @return @c true if @p item is not @c NULL, and is of type 
  * @sbmlconstant{SBML_RENDER_GLOBALRENDERINFORMATION, SBMLRenderTypeCode_t};
  * @c false otherwise.
  */
  virtual bool isValidTypeForList(SBase * item);


  /**
   * Predicate returning @c true if all the required attributes for this
   * ListOfGlobalRenderInformation object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ListOfGlobalRenderInformation have been set, otherwise @c false is
   * returned.
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
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */




  #ifndef SWIG




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


  /**
  * Creates an XMLNode object from this ListOfLocalRenderInformation object.
  *
  * @return the XMLNode with the XML representation for the
  * ListOfLocalRenderInformation object.
  */
  XMLNode toXML() const;


  /**
  * Parses the xml information in the given node and sets the attributes.
  * This method should never be called by the user. It is only used to read render
  * information from annotations.
  *
  * @param node the XMLNode object reference that describes the ListOfGlobalRenderInformation
  * object to be instantiated.
  */
  void parseXML(const XMLNode& node);

protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new GlobalRenderInformation in this
   * ListOfGlobalRenderInformation
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



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Render package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns the value of the "majorVersion" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose majorVersion is sought.
 *
 * @return the value of the "majorVersion" attribute of this ListOf_t as a
 * unsigned integer.
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
unsigned int
ListOfGlobalRenderInformation_getMajorVersion(const ListOf_t * lo);


/**
 * Returns the value of the "minorVersion" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose minorVersion is sought.
 *
 * @return the value of the "minorVersion" attribute of this ListOf_t as a
 * unsigned integer.
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
unsigned int
ListOfGlobalRenderInformation_getMinorVersion(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "majorVersion" attribute
 * is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "majorVersion" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_isSetMajorVersion(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "minorVersion" attribute
 * is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "minorVersion" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_isSetMinorVersion(const ListOf_t * lo);


/**
 * Sets the value of the "majorVersion" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param majorVersion unsigned int value of the "majorVersion" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_setMajorVersion(ListOf_t * lo,
                                              unsigned int majorVersion);


/**
 * Sets the value of the "minorVersion" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param minorVersion unsigned int value of the "minorVersion" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_setMinorVersion(ListOf_t * lo,
                                              unsigned int minorVersion);


/**
 * Unsets the value of the "majorVersion" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_unsetMajorVersion(ListOf_t * lo);


/**
 * Unsets the value of the "minorVersion" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
ListOfGlobalRenderInformation_unsetMinorVersion(ListOf_t * lo);


/**
 * Get a GlobalRenderInformation_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * GlobalRenderInformation_t to retrieve.
 *
 * @return the nth GlobalRenderInformation_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_getGlobalRenderInformation(ListOf_t* lo,
                                                         unsigned int n);


/**
 * Get a GlobalRenderInformation_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * GlobalRenderInformation_t to retrieve.
 *
 * @return the GlobalRenderInformation_t in this ListOf_t with the given @p sid
 * or @c NULL if no such GlobalRenderInformation_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth GlobalRenderInformation_t from this ListOf_t and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * GlobalRenderInformation_t to remove.
 *
 * @return a pointer to the nth GlobalRenderInformation_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the GlobalRenderInformation_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * GlobalRenderInformation_t to remove.
 *
 * @return the GlobalRenderInformation_t in this ListOf_t based on the
 * identifier or NULL if no such GlobalRenderInformation_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
ListOfGlobalRenderInformation_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfGlobalRenderInformation_H__ */


