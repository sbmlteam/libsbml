/**
 * @file    LocalRenderInformation.h
 * @brief Definition of the LocalRenderInformation class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class LocalRenderInformation
 * @sbmlbrief{render} Rendering information stored in Layouts.
 *
 * LocalRenderInformation is one of the subclasses of
 * RenderInformationBase. A "local rendering information" object contains color
 * definitions, gradient definitions and line endings as defined in
 * RenderInformationBase.  Additionally it has a list of local styles which
 * specifies type, role and id based render information.  Local render
 * information can specify id based render information because it does belong
 * to a certain layout and it can reference ids of object in that layout.
 */

#ifndef LocalRenderInformation_H__
#define LocalRenderInformation_H__

#include <sbml/common/sbmlfwd.h>

#include <sbml/packages/render/sbml/RenderInformationBase.h> 
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/LocalStyle.h> 
#include <sbml/packages/render/sbml/ListOfLocalStyles.h>
#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN LocalRenderInformation : public RenderInformationBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfLocalStyles mLocalStyles;

  /** @endcond */

public:

  /**
   * Creates a new LocalRenderInformation using the given SBML Level, Version
   * and &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * LocalRenderInformation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * LocalRenderInformation.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this LocalRenderInformation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LocalRenderInformation(unsigned int level =
    RenderExtension::getDefaultLevel(),
                         unsigned int version =
                           RenderExtension::getDefaultVersion(),
                         unsigned int pkgVersion =
                           RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new LocalRenderInformation using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LocalRenderInformation(RenderPkgNamespaces *renderns);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a LocalRenderInformation with the given @p id
   * and all lists empty.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id the new id for the LocalRenderInformation.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  LocalRenderInformation(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for LocalRenderInformation.
   *
   * @param orig the LocalRenderInformation instance to copy.
   */
  LocalRenderInformation(const LocalRenderInformation& orig);


  /**
   * Assignment operator for LocalRenderInformation.
   *
   * @param rhs the LocalRenderInformation object whose values are to be used
   * as the basis of the assignment.
   */
  LocalRenderInformation& operator=(const LocalRenderInformation& rhs);


  /**
   * Creates and returns a deep copy of this LocalRenderInformation object.
   *
   * @return a (deep) copy of this LocalRenderInformation object.
   */
  virtual LocalRenderInformation* clone() const;


  /**
   * Destructor for LocalRenderInformation.
   */
  virtual ~LocalRenderInformation();


  /**
   * Returns the ListOfLocalStyles from this LocalRenderInformation.
   *
   * @return the ListOfLocalStyles from this LocalRenderInformation.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getLocalStyle(unsigned int n)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  const ListOfLocalStyles* getListOfLocalStyles() const;


  /**
  * Returns the ListOfLocalStyles from this LocalRenderInformation.
  *
  * @return the ListOfLocalStyles from this LocalRenderInformation.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addLocalStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getLocalStyle(const std::string& sid)
  * @see getLocalStyle(unsigned int n)
  * @see getNumLocalStyles()
  * @see removeLocalStyle(const std::string& sid)
  * @see removeLocalStyle(unsigned int n)
  */
  const ListOfLocalStyles* getListOfStyles() const;


  /**
   * Returns the ListOfLocalStyles from this LocalRenderInformation.
   *
   * @return the ListOfLocalStyles from this LocalRenderInformation.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getLocalStyle(unsigned int n)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  ListOfLocalStyles* getListOfLocalStyles();


  /**
  * Returns the ListOfLocalStyles from this LocalRenderInformation.
  *
  * @return the ListOfLocalStyles from this LocalRenderInformation.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addLocalStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getLocalStyle(const std::string& sid)
  * @see getLocalStyle(unsigned int n)
  * @see getNumLocalStyles()
  * @see removeLocalStyle(const std::string& sid)
  * @see removeLocalStyle(unsigned int n)
  */
  ListOfLocalStyles* getListOfStyles();


  /**
   * Get a LocalStyle from the LocalRenderInformation.
   *
   * @param n an unsigned int representing the index of the LocalStyle to
   * retrieve.
   *
   * @return the nth LocalStyle in the ListOfLocalStyles within this
   * LocalRenderInformation.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  LocalStyle* getLocalStyle(unsigned int n);


  /**
  * Get a LocalStyle from the LocalRenderInformation.
  *
  * @param n an unsigned int representing the index of the LocalStyle to
  * retrieve.
  *
  * @return the nth LocalStyle in the ListOfLocalStyles within this
  * LocalRenderInformation.
  * If the index @p n is invalid, @c NULL is returned.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getStyle(const std::string& sid)
  * @see getNumStyles()
  * @see removeStyle(const std::string& sid)
  * @see removeStyle(unsigned int n)
  */
  LocalStyle* getStyle(unsigned int n);


  /**
   * Get a LocalStyle from the LocalRenderInformation.
   *
   * @param n an unsigned int representing the index of the LocalStyle to
   * retrieve.
   *
   * @return the nth LocalStyle in the ListOfLocalStyles within this
   * LocalRenderInformation.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  const LocalStyle* getLocalStyle(unsigned int n) const;


  /**
  * Get a LocalStyle from the LocalRenderInformation.
  *
  * @param n an unsigned int representing the index of the LocalStyle to
  * retrieve.
  *
  * @return the nth LocalStyle in the ListOfLocalStyles within this
  * LocalRenderInformation.
  * If the index @p n is invalid, @c NULL is returned.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getStyle(const std::string& sid)
  * @see getNumStyles()
  * @see removeStyle(const std::string& sid)
  * @see removeStyle(unsigned int n)
  */
  const LocalStyle* getStyle(unsigned int n) const;


  /**
  * Returns a pointer to the style with the given @p id.
  * If the id is invalid, @c NULL is returned.
  *
  * @param id id of the LocalStyle to be returned.
  *
  * @return pointer to the style with the given @p id or @c NULL
  */
  LocalStyle* getLocalStyle(const std::string& id);

  /**
  * Returns a pointer to the style with the given @p id.
  * If the id is invalid, @c NULL is returned.
  *
  * @param id id of the LocalStyle to be returned.
  *
  * @return pointer to the style with the given @p id or @c NULL
  */
  LocalStyle* getStyle(const std::string& id);

  /**
  * Returns a pointer to the style with the given @p id.
  * If the id is invalid, @c NULL is returned.
  *
  * @param id id of the LocalStyle to be returned.
  *
  * @return const pointer to the style with the given @p id or @c NULL
  */
  const LocalStyle* getLocalStyle(const std::string& id) const;

  /**
  * Returns a pointer to the style with the given @p id.
  * If the id is invalid, @c NULL is returned.
  *
  * @param id id of the LocalStyle to be returned.
  *
  * @return const pointer to the style with the given @p id or @c NULL
  */
  const LocalStyle* getStyle(const std::string& id) const;

  /**
   * Adds a copy of the given LocalStyle to this LocalRenderInformation.
   *
   * @param ls the LocalStyle object to add.
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
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getLocalStyle(unsigned int n)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  int addLocalStyle(const LocalStyle* ls);


  /**
  * Adds a copy of the given LocalStyle to this LocalRenderInformation.
  *
  * @param ls the LocalStyle object to add.
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
  * @see createLocalStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeStyle(const std::string& sid)
  * @see removeStyle(unsigned int n)
  */
  int addStyle(const LocalStyle* ls);


  /**
   * Get the number of LocalStyle objects in this LocalRenderInformation.
   *
   * @return the number of LocalStyle objects in this LocalRenderInformation.
   *
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getLocalStyle(unsigned int n)
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  unsigned int getNumLocalStyles() const;


  /**
  * Get the number of LocalStyle objects in this LocalRenderInformation.
  *
  * @return the number of LocalStyle objects in this LocalRenderInformation.
  *
  *
  * @see addStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see removeStyle(const std::string& sid)
  * @see removeStyle(unsigned int n)
  */
  unsigned int getNumStyles() const;


  /**
   * Creates a new LocalStyle object, adds it to this LocalRenderInformation
   * object and returns the LocalStyle object created.
   *
   * @return a new LocalStyle object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see getLocalStyle(const std::string& sid)
   * @see getLocalStyle(unsigned int n)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   * @see removeLocalStyle(unsigned int n)
   */
  LocalStyle* createLocalStyle();

  /** @cond doxygenLibsbmlInternal */

  /**
  * Creates a new LocalStyle object, adds it to this LocalRenderInformation
  * object, sets the id, and returns the LocalStyle object created.
  *
  * @param id the id to give to the newly-created LocalStyle.
  *
  * @return a new LocalStyle object instance.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const LocalStyle* object)
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeStyle(const std::string& sid)
  * @see removeStyle(unsigned int n)
  */
  LocalStyle* createStyle(const std::string& id);

  /** @endcond */

  /**
   * Removes the nth LocalStyle from this LocalRenderInformation and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the LocalStyle to
   * remove.
   *
   * @return a pointer to the nth LocalStyle in this LocalRenderInformation.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see getLocalStyle(const std::string& sid)
   * @see getLocalStyle(unsigned int n)
   * @see getNumLocalStyles()
   * @see removeLocalStyle(const std::string& sid)
   */
  LocalStyle* removeLocalStyle(unsigned int n);


  /**
  * Removes the nth LocalStyle from this LocalRenderInformation and returns a
  * pointer to it.
  *
  * @param n an unsigned int representing the index of the LocalStyle to
  * remove.
  *
  * @return a pointer to the nth LocalStyle in this LocalRenderInformation.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeStyle(const std::string& sid)
  */
  LocalStyle* removeStyle(unsigned int n);


  /**
  * Removes the LocalStyle from this LocalRenderInformation based on its
  * identifier and returns a pointer to it.
  *
  * @param id a string representing the identifier of the LocalStyle to
  * remove.
  *
  * @return the LocalStyle in this LocalRenderInformation based on the
  * identifier or NULL if no such LocalStyle exists.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addLocalStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getLocalStyle(const std::string& sid)
  * @see getLocalStyle(unsigned int n)
  * @see getNumLocalStyles()
  * @see removeLocalStyle(const std::string& sid)
  */
  LocalStyle* removeLocalStyle(const std::string& id);


  /**
  * Removes the LocalStyle from this LocalRenderInformation based on its
  * identifier and returns a pointer to it.
  *
  * @param id a string representing the identifier of the LocalStyle to
  * remove.
  *
  * @return the LocalStyle in this LocalRenderInformation based on the
  * identifier or NULL if no such LocalStyle exists.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addStyle(const LocalStyle* object)
  * @see createLocalStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeStyle(const std::string& sid)
  */
  LocalStyle* removeStyle(const std::string& id);


  /**
   * Returns the XML element name of this LocalRenderInformation object.
   *
   * For LocalRenderInformation, the XML element name is always
   * @c "renderInformation".
   *
   * @return the name of this element, i.e. @c "renderInformation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this LocalRenderInformation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_LOCALRENDERINFORMATION, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;



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




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this
   * LocalRenderInformation.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this LocalRenderInformation.
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
   * LocalRenderInformation.
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
   * Returns the number of "elementName" in this LocalRenderInformation.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this LocalRenderInformation.
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


  /**
   * Parses the xml information in the given node and sets the attributes.
   * This method should never be called by the user. It is only used to read render 
   * information from annotations.
   *
   * @param node the XMLNode object reference that describes the LocalRenderInformation
   * object to be instantiated.
   */
  void parseXML(const XMLNode& node);


  /**
   * Creates an XMLNode object from this LocalRenderInformation object.
   *
   * @return the XMLNode with the XML representation for the 
   * LocalRenderInformation object.
   *
   */
  XMLNode toXML() const;

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
 * Creates a new LocalRenderInformation_t using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * LocalRenderInformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * LocalRenderInformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * LocalRenderInformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
LocalRenderInformation_t *
LocalRenderInformation_create(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this LocalRenderInformation_t object.
 *
 * @param lri the LocalRenderInformation_t structure.
 *
 * @return a (deep) copy of this LocalRenderInformation_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
LocalRenderInformation_t*
LocalRenderInformation_clone(const LocalRenderInformation_t* lri);


/**
 * Frees this LocalRenderInformation_t object.
 *
 * @param lri the LocalRenderInformation_t structure.
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
void
LocalRenderInformation_free(LocalRenderInformation_t* lri);


/**
 * Returns a ListOf_t * containing LocalStyle_t objects from this
 * LocalRenderInformation_t.
 *
 * @param lri the LocalRenderInformation_t structure whose ListOfLocalStyles is
 * sought.
 *
 * @return the ListOfLocalStyles from this LocalRenderInformation_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see LocalRenderInformation_addLocalStyle()
 * @see LocalRenderInformation_createLocalStyle()
 * @see LocalRenderInformation_getLocalStyleById()
 * @see LocalRenderInformation_getLocalStyle()
 * @see LocalRenderInformation_getNumLocalStyles()
 * @see LocalRenderInformation_removeLocalStyleById()
 * @see LocalRenderInformation_removeLocalStyle()
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
ListOf_t*
LocalRenderInformation_getListOfLocalStyles(LocalRenderInformation_t* lri);


/**
 * Get a LocalStyle_t from the LocalRenderInformation_t.
 *
 * @param lri the LocalRenderInformation_t structure to search.
 *
 * @param n an unsigned int representing the index of the LocalStyle_t to
 * retrieve.
 *
 * @return the nth LocalStyle_t in the ListOfLocalStyles within this
 * LocalRenderInformation.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalRenderInformation_getLocalStyle(LocalRenderInformation_t* lri,
                                     unsigned int n);


/**
 * Adds a copy of the given LocalStyle_t to this LocalRenderInformation_t.
 *
 * @param lri the LocalRenderInformation_t structure to which the LocalStyle_t
 * should be added.
 *
 * @param ls the LocalStyle_t object to add.
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
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
int
LocalRenderInformation_addLocalStyle(LocalRenderInformation_t* lri,
                                     const LocalStyle_t* ls);


/**
 * Get the number of LocalStyle_t objects in this LocalRenderInformation_t.
 *
 * @param lri the LocalRenderInformation_t structure to query.
 *
 * @return the number of LocalStyle_t objects in this LocalRenderInformation_t.
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
unsigned int
LocalRenderInformation_getNumLocalStyles(LocalRenderInformation_t* lri);


/**
 * Creates a new LocalStyle_t object, adds it to this LocalRenderInformation_t
 * object and returns the LocalStyle_t object created.
 *
 * @param lri the LocalRenderInformation_t structure to which the LocalStyle_t
 * should be added.
 *
 * @return a new LocalStyle_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalRenderInformation_createLocalStyle(LocalRenderInformation_t* lri);


/**
 * Removes the nth LocalStyle_t from this LocalRenderInformation_t and returns
 * a pointer to it.
 *
 * @param lri the LocalRenderInformation_t structure to search.
 *
 * @param n an unsigned int representing the index of the LocalStyle_t to
 * remove.
 *
 * @return a pointer to the nth LocalStyle_t in this LocalRenderInformation_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
LocalStyle_t*
LocalRenderInformation_removeLocalStyle(LocalRenderInformation_t* lri,
                                        unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LocalRenderInformation_t object have been set.
 *
 * @param lri the LocalRenderInformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * LocalRenderInformation_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
int
LocalRenderInformation_hasRequiredAttributes(const LocalRenderInformation_t *
  lri);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * LocalRenderInformation_t object have been set.
 *
 * @param lri the LocalRenderInformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * LocalRenderInformation_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the LocalRenderInformation_t object are:
 *
 * @memberof LocalRenderInformation_t
 */
LIBSBML_EXTERN
int
LocalRenderInformation_hasRequiredElements(const LocalRenderInformation_t *
  lri);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !LocalRenderInformation_H__ */


