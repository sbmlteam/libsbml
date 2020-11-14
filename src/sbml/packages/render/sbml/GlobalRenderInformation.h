/**
 * @file    GlobalRenderInformation.h
 * @brief Definition of the GlobalRenderInformation class.
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
 * @class GlobalRenderInformation
 * @sbmlbrief{render} Render information stored in a ListOfLayouts.
 *
 * GlobalRenderInformation is one of the subclasses of RenderInformationBase.
 * A global render information object contains color definitions, gradient
 * definitions and line endings as defined in RenderInformationBase.
 * Additionally it has a list of global styles which specifies type and role
 * based render information.  This class of objects cannot specify id-based
 * render information because it does not belong to a certain layout but it
 * belongs to all layouts.  GlobalRenderInformation can be applied to all
 * layouts.
 */

#ifndef GlobalRenderInformation_H__
#define GlobalRenderInformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/RenderInformationBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ListOfGlobalStyles.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN GlobalRenderInformation : public RenderInformationBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfGlobalStyles mGlobalStyles;

  /** @endcond */

public:

  /**
   * Creates a new GlobalRenderInformation using the given SBML Level, Version
   * and &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GlobalRenderInformation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GlobalRenderInformation.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this GlobalRenderInformation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GlobalRenderInformation(
                          unsigned int level =
                            RenderExtension::getDefaultLevel(),
                          unsigned int version =
                            RenderExtension::getDefaultVersion(),
                          unsigned int pkgVersion =
                            RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GlobalRenderInformation using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GlobalRenderInformation(RenderPkgNamespaces *renderns);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a GlobalRenderInformation with the given @p id
   * and all lists empty.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id the new id for the GlobalRenderInformation.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  GlobalRenderInformation(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for GlobalRenderInformation.
   *
   * @param orig the GlobalRenderInformation instance to copy.
   */
  GlobalRenderInformation(const GlobalRenderInformation& orig);


  /**
   * Assignment operator for GlobalRenderInformation.
   *
   * @param rhs the GlobalRenderInformation object whose values are to be used
   * as the basis of the assignment.
   */
  GlobalRenderInformation& operator=(const GlobalRenderInformation& rhs);


  /**
   * Creates and returns a deep copy of this GlobalRenderInformation object.
   *
   * @return a (deep) copy of this GlobalRenderInformation object.
   */
  virtual GlobalRenderInformation* clone() const;


  /**
   * Destructor for GlobalRenderInformation.
   */
  virtual ~GlobalRenderInformation();


  /**
   * Returns the ListOfGlobalStyles from this GlobalRenderInformation.
   *
   * @return the ListOfGlobalStyles from this GlobalRenderInformation.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getGlobalStyle(unsigned int n)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  const ListOfGlobalStyles* getListOfGlobalStyles() const;


  /**
  * Returns the ListOfGlobalStyles from this GlobalRenderInformation.
  *
  * @return the ListOfGlobalStyles from this GlobalRenderInformation.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  const ListOfGlobalStyles* getListOfStyles() const;


  /**
   * Returns the ListOfGlobalStyles from this GlobalRenderInformation.
   *
   * @return the ListOfGlobalStyles from this GlobalRenderInformation.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getGlobalStyle(unsigned int n)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  ListOfGlobalStyles* getListOfGlobalStyles();


  /**
  * Returns the ListOfGlobalStyles from this GlobalRenderInformation.
  *
  * @return the ListOfGlobalStyles from this GlobalRenderInformation.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  ListOfGlobalStyles* getListOfStyles();
 
  
 /**
   * Get a GlobalStyle from the GlobalRenderInformation.
   *
   * @param n an unsigned int representing the index of the GlobalStyle to
   * retrieve.
   *
   * @return the nth GlobalStyle in the ListOfGlobalStyles within this
   * GlobalRenderInformation or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  GlobalStyle* getGlobalStyle(unsigned int n);


  /**
  * Get a GlobalStyle from the GlobalRenderInformation.
  *
  * @param n an unsigned int representing the index of the GlobalStyle to
  * retrieve.
  *
  * @return the nth GlobalStyle in the ListOfGlobalStyles within this
  * GlobalRenderInformation or @c NULL if no such object exists.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  GlobalStyle* getStyle(unsigned int n);


  /**
   * Get a GlobalStyle from the GlobalRenderInformation.
   *
   * @param n an unsigned int representing the index of the GlobalStyle to
   * retrieve.
   *
   * @return the nth GlobalStyle in the ListOfGlobalStyles within this
   * GlobalRenderInformation or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  const GlobalStyle* getGlobalStyle(unsigned int n) const;


  /**
  * Get a GlobalStyle from the GlobalRenderInformation.
  *
  * @param n an unsigned int representing the index of the GlobalStyle to
  * retrieve.
  *
  * @return the nth GlobalStyle in the ListOfGlobalStyles within this
  * GlobalRenderInformation.
  * If the index @p n is invalid, @c NULL is returned.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  const GlobalStyle* getStyle(unsigned int n) const;


  /**
  * Get a GlobalStyle from the GlobalRenderInformation.
  *
  * @param id a string representing the identifier of the ColorDefinition to
  * remove.
  *
  * @return the GlobalStyle in this GlobalRenderInformation based on the
  * identifier or NULL if no such GlobalStyle exists.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addGlobalStyle(const GlobalStyle* object)
  * @see createGlobalStyle()
  * @see getGlobalStyle(const std::string& sid)
  * @see getNumGlobalStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  GlobalStyle* getGlobalStyle(const std::string& id);


  /**
  * Get a GlobalStyle from the GlobalRenderInformation.
  *
  * @param id a string representing the identifier of the ColorDefinition to
  * remove.
  *
  * @return the GlobalStyle in this GlobalRenderInformation based on the
  * identifier or NULL if no such GlobalStyle exists.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  GlobalStyle* getStyle(const std::string& id);


  /**
  * Get a GlobalStyle from the GlobalRenderInformation.
  *
  * @param id a string representing the identifier of the ColorDefinition to
  * remove.
  *
  * @return the GlobalStyle in this GlobalRenderInformation based on the
  * identifier or NULL if no such GlobalStyle exists.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addGlobalStyle(const GlobalStyle* object)
  * @see createGlobalStyle()
  * @see getGlobalStyle(const std::string& sid)
  * @see getNumGlobalStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  const GlobalStyle* getGlobalStyle(const std::string& id) const;


  /**
  * Get a GlobalStyle from the GlobalRenderInformation.
  *
  * @param id a string representing the identifier of the ColorDefinition to
  * remove.
  *
  * @return the GlobalStyle in this GlobalRenderInformation based on the
  * identifier or NULL if no such GlobalStyle exists.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  const GlobalStyle* getStyle(const std::string& id) const;


  /**
   * Adds a copy of the given GlobalStyle to this GlobalRenderInformation.
   *
   * @param gs the GlobalStyle object to add.
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
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getGlobalStyle(unsigned int n)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  int addGlobalStyle(const GlobalStyle* gs);


  /**
  * Adds a copy of the given GlobalStyle to this GlobalRenderInformation.
  *
  * @param gs the GlobalStyle object to add.
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
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  int addStyle(const GlobalStyle* gs);


  /**
   * Get the number of GlobalStyle objects in this GlobalRenderInformation.
   *
   * @return the number of GlobalStyle objects in this GlobalRenderInformation.
   *
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getGlobalStyle(unsigned int n)
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  unsigned int getNumGlobalStyles() const;


  /**
  * Get the number of GlobalStyle objects in this GlobalRenderInformation.
  *
  * @return the number of GlobalStyle objects in this GlobalRenderInformation.
  *
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  unsigned int getNumStyles() const;


  /**
   * Creates a new GlobalStyle object, adds it to this GlobalRenderInformation
   * object and returns the GlobalStyle object created.
   *
   * @return a new GlobalStyle object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see getGlobalStyle(const std::string& sid)
   * @see getGlobalStyle(unsigned int n)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   * @see removeGlobalStyle(unsigned int n)
   */
  GlobalStyle* createGlobalStyle();


  /**
  * Creates a new GlobalStyle object with the given id, adds it to this GlobalRenderInformation
  * object and returns the GlobalStyle object created.
  *
  * @return a new GlobalStyle object instance.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeGlobalStyle(const std::string& sid)
  * @see removeGlobalStyle(unsigned int n)
  */
  GlobalStyle* createStyle(const std::string& id);


  /**
   * Removes the nth GlobalStyle from this GlobalRenderInformation and returns
   * a pointer to it.
   *
   * @param n an unsigned int representing the index of the GlobalStyle to
   * remove.
   *
   * @return a pointer to the nth GlobalStyle in this GlobalRenderInformation.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see getGlobalStyle(const std::string& sid)
   * @see getGlobalStyle(unsigned int n)
   * @see getNumGlobalStyles()
   * @see removeGlobalStyle(const std::string& sid)
   */
  GlobalStyle* removeGlobalStyle(unsigned int n);


  /**
  * Removes the GlobalStyle with the given id from this GlobalRenderInformation 
  * and returns a pointer to it.
  *
  * @param sid the id of the GlobalStyle to remove.
  *
  * @return a pointer to the nth GlobalStyle in this GlobalRenderInformation.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addGlobalStyle(const GlobalStyle* object)
  * @see createGlobalStyle()
  * @see getGlobalStyle(const std::string& sid)
  * @see getGlobalStyle(unsigned int n)
  * @see getNumGlobalStyles()
  * @see removeGlobalStyle(unsigned int n)
  */
  GlobalStyle* removeGlobalStyle(const std::string& sid);


  /**
  * Removes the nth GlobalStyle from this GlobalRenderInformation and returns
  * a pointer to it.
  *
  * @param n an unsigned int representing the index of the GlobalStyle to
  * remove.
  *
  * @return a pointer to the nth GlobalStyle in this GlobalRenderInformation.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addStyle(const GlobalStyle* object)
  * @see createStyle()
  * @see getStyle(const std::string& sid)
  * @see getStyle(unsigned int n)
  * @see getNumStyles()
  * @see removeGlobalStyle(unsigned int n)
  * @see removeGlobalStyle(const std::string& sid)
  */
  GlobalStyle* removeStyle(unsigned int n);


  /**
   * Returns the XML element name of this GlobalRenderInformation object.
   *
   * For GlobalRenderInformation, the XML element name is always
   * @c "renderInformation".
   *
   * @return the name of this element, i.e. @c "renderInformation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this GlobalRenderInformation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_GLOBALRENDERINFORMATION, SBMLRenderTypeCode_t}.
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
   * GlobalRenderInformation.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this GlobalRenderInformation.
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
   * GlobalRenderInformation.
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
   * Returns the number of "elementName" in this GlobalRenderInformation.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this GlobalRenderInformation.
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
   * @param node the XMLNode object reference that describes the GlobalRenderInformation
   * object to be instantiated.
   */
  void parseXML(const XMLNode& node);


  /**
   * Creates an XMLNode object from this GlobalRenderInformation object.
   *
   * @return the XMLNode with the XML representation for the 
   * GlobalRenderInformation object.
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
 * Creates a new GlobalRenderInformation_t using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GlobalRenderInformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GlobalRenderInformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * GlobalRenderInformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalRenderInformation_t *
GlobalRenderInformation_create(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GlobalRenderInformation_t object.
 *
 * @param gri the GlobalRenderInformation_t structure.
 *
 * @return a (deep) copy of this GlobalRenderInformation_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalRenderInformation_t*
GlobalRenderInformation_clone(const GlobalRenderInformation_t* gri);


/**
 * Frees this GlobalRenderInformation_t object.
 *
 * @param gri the GlobalRenderInformation_t structure.
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
void
GlobalRenderInformation_free(GlobalRenderInformation_t* gri);


/**
 * Returns a ListOf_t * containing GlobalStyle_t objects from this
 * GlobalRenderInformation_t.
 *
 * @param gri the GlobalRenderInformation_t structure whose ListOfGlobalStyles
 * is sought.
 *
 * @return the ListOfGlobalStyles from this GlobalRenderInformation_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see GlobalRenderInformation_addGlobalStyle()
 * @see GlobalRenderInformation_createGlobalStyle()
 * @see GlobalRenderInformation_getGlobalStyleById()
 * @see GlobalRenderInformation_getGlobalStyle()
 * @see GlobalRenderInformation_getNumGlobalStyles()
 * @see GlobalRenderInformation_removeGlobalStyleById()
 * @see GlobalRenderInformation_removeGlobalStyle()
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
ListOf_t*
GlobalRenderInformation_getListOfGlobalStyles(GlobalRenderInformation_t* gri);


/**
 * Get a GlobalStyle_t from the GlobalRenderInformation_t.
 *
 * @param gri the GlobalRenderInformation_t structure to search.
 *
 * @param n an unsigned int representing the index of the GlobalStyle_t to
 * retrieve.
 *
 * @return the nth GlobalStyle_t in the ListOfGlobalStyles within this
 * GlobalRenderInformation or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalRenderInformation_getGlobalStyle(GlobalRenderInformation_t* gri,
                                       unsigned int n);


/**
 * Adds a copy of the given GlobalStyle_t to this GlobalRenderInformation_t.
 *
 * @param gri the GlobalRenderInformation_t structure to which the
 * GlobalStyle_t should be added.
 *
 * @param gs the GlobalStyle_t object to add.
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
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
GlobalRenderInformation_addGlobalStyle(GlobalRenderInformation_t* gri,
                                       const GlobalStyle_t* gs);


/**
 * Get the number of GlobalStyle_t objects in this GlobalRenderInformation_t.
 *
 * @param gri the GlobalRenderInformation_t structure to query.
 *
 * @return the number of GlobalStyle_t objects in this
 * GlobalRenderInformation_t.
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
unsigned int
GlobalRenderInformation_getNumGlobalStyles(GlobalRenderInformation_t* gri);


/**
 * Creates a new GlobalStyle_t object, adds it to this
 * GlobalRenderInformation_t object and returns the GlobalStyle_t object
 * created.
 *
 * @param gri the GlobalRenderInformation_t structure to which the
 * GlobalStyle_t should be added.
 *
 * @return a new GlobalStyle_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalRenderInformation_createGlobalStyle(GlobalRenderInformation_t* gri);


/**
 * Removes the nth GlobalStyle_t from this GlobalRenderInformation_t and
 * returns a pointer to it.
 *
 * @param gri the GlobalRenderInformation_t structure to search.
 *
 * @param n an unsigned int representing the index of the GlobalStyle_t to
 * remove.
 *
 * @return a pointer to the nth GlobalStyle_t in this
 * GlobalRenderInformation_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalRenderInformation_removeGlobalStyle(GlobalRenderInformation_t* gri,
                                          unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GlobalRenderInformation_t object have been set.
 *
 * @param gri the GlobalRenderInformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GlobalRenderInformation_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
GlobalRenderInformation_hasRequiredAttributes(const GlobalRenderInformation_t *
  gri);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * GlobalRenderInformation_t object have been set.
 *
 * @param gri the GlobalRenderInformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * GlobalRenderInformation_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the GlobalRenderInformation_t object are:
 *
 * @memberof GlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
GlobalRenderInformation_hasRequiredElements(const GlobalRenderInformation_t *
  gri);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */

#ifndef LIBSBML_USE_STRICT_INCLUDES
#include <sbml/packages/render/sbml/ListOfGlobalRenderInformation.h>
#endif // LIBSBML_USE_STRICT_INCLUDES

#endif /* !GlobalRenderInformation_H__ */


