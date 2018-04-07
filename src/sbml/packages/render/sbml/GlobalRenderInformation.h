/**
 * @file    GlobalRenderInformation.h
 * @brief   class for representing global render information
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @brief GlobalRenderInformation is the render information stored in the ListOfLayouts. GlobalRenderInformation can be
 *  applied to all layouts.
 *
 * GlobalRenderInformation is one of the subclasses of RenderInformationBase. A global render information object
 * contains color definitions, gradient definitions and line endings as defined in RenderInformationBase.
 * Additionally it has a list of global styles which specifies type and role based render information.
 * Global render information can not specify id based render information because it does not belong to a certain layout
 * but it belongs to all layouts.
 *
 * @class ListOfGlobalRenderInformation
 * @brief container class that stores GlobalRenderInformation objects.
 *
 * The ListOfLayouts in the SBML model contains a ListOfGlobalRenderInformation which holds all GlobalRenderInformation
 * objects.
 */

#ifndef GlobalRenderInformation_H__
#define GlobalRenderInformation_H__

#include <sbml/common/sbmlfwd.h>

#include <sbml/packages/render/sbml/RenderInformationBase.h>
#include <sbml/packages/render/sbml/GlobalStyle.h>
#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus

#include <string>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ListOfGlobalStyles.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN GlobalRenderInformation : public RenderInformationBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  ListOfGlobalStyles mListOfStyles;
  static const std::string ELEMENT_NAME;
  /** @endcond */

public:
  /**
   * Creates a new GlobalRenderInformation object with the given SBML level
   * and SBML version.
   *
   * @param level SBML level of the new object
   * @param level SBML version of the new object
   */
  GlobalRenderInformation (unsigned int level      = RenderExtension::getDefaultLevel(),
    unsigned int version    = RenderExtension::getDefaultVersion(),
    unsigned int pkgVersion = RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GlobalRenderInformation object with the given SBMLNamespaces.
   *
   * @param sbmlns The SBML namespace for the object.
   */
  GlobalRenderInformation (RenderPkgNamespaces* renderns);


  /**
   * Destroy this GlobalRenderInformation object.
   */
  virtual ~GlobalRenderInformation ();


  /**
   * Parses the xml information in the given node and sets the attributes.
   * This method should never be called by the user. It is only used to read render 
   * information from annotations.
   *
   * @param node the XMLNode object reference that describes the GlobalRenderInformation
   * object to be instantiated.
   */
  void parseXML(const XMLNode& node);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a GlobalRenderInformation with the given @p id
   * and all lists empty.
   *
   * @param id the new id for the GlobalRenderInformation.
   *
   * This constructor is deprecated. The new libsbml API only has
   * constructors which take the SBML level and version or one that takes
   * an SBMLNamespaces object.
   */
  GlobalRenderInformation(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Returns the number of styles.
   *
   * @return the number of global styles in the global render information object
   */
  unsigned int getNumStyles() const;

  /**
   * Returns a pointer to the ListOfGlobalStyles object.
   *
   * @return pointer to the list of global styles.
   */
  ListOfGlobalStyles* getListOfStyles();

  /**
   * Returns a const pointer to the ListOfGlobalStyles object.
   *
   * @return const pointer to the list of global styles.
   */
  const ListOfGlobalStyles* getListOfStyles() const;

  /**
   * Returns a pointer to the style with the given index.
   * If the index is invalid, @c NULL is returned.
   * 
   * @param i index of the GlobalStyle to be returned.
   * 
   * @return pointer to the style with the given index or NULL
   */
  GlobalStyle* getStyle(unsigned int i);

  /**
   * Returns a const pointer to the style with the given index.
   * If the index is invalid, @c NULL is returned.
   * 
   * @param i index of the GlobalStyle to be returned.
   * 
   * @return const pointer to the style with the given index or NULL
   */
  const GlobalStyle* getStyle(unsigned int i) const;

  /**
   * Returns a pointer to the style with the given @p id.
   * If the id is invalid, @c NULL is returned.
   * 
   * @param id id of the GlobalStyle to be returned.
   * 
   * @return pointer to the style with the given @p id or @c NULL
   */
  GlobalStyle* getStyle(const std::string& id);

  /**
   * Returns a pointer to the style with the given @p id.
   * If the id is invalid, @c NULL is returned.
   * 
   * @param id id of the GlobalStyle to be returned.
   * 
   * @return const pointer to the style with the given @p id or @c NULL
   */
  const GlobalStyle* getStyle(const std::string& id) const;


  /**
   * Creates a new GlobalStyle object. The object is added to and owned
   * by the GlobalRenderInformation object.
   * 
   * @param id for the new style.
   * 
   * @ return a pointer to the newly created GlobalStyle object.
   */
  GlobalStyle* createStyle(const std::string& id);

  /**
   * Adds a copy of a GlobalStyle to the GlobalRenderInformation object.
   * The style is only added if it is valid, i.e. it has to have an id and
   * a valid group.
   *
   * @param pointer to the global style object to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_OPERATION_FAILED
   *
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this GlobalRenderInformation.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the GlobalRenderInformation</em>.  In addition, the caller should make
   * sure to free the original object if it is no longer being used, or
   * else a memory leak will result.  Please see GlobalRenderInformation::createStyle()
   * for a method that does not lead to these issues.
   *
   * @see createStyle()
   */
  int addStyle(const GlobalStyle* pStyle);

  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth
   *
   * @return a List* of pointers to all children objects.
   */
  virtual List* getAllElements(ElementFilter* filter=NULL);


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */

  /**
   * Returns the XML element name of this object, which for
   * GlobalRenderInformation, is always @c "renderInformation".
   * 
   * @return the name of this element, i.e., @c "renderInformation".
   */
  virtual const std::string& getElementName () const ;


  /**
   * Creates and returns a deep copy of this GlobalRenderInformation object.
   *
   * @return a (deep) copy of this GlobalRenderInformation.
   */
  virtual GlobalRenderInformation* clone () const;

  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or @c SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates an XMLNode object from this GlobalRenderInformation object.
   *
   * @return the XMLNode with the XML representation for the 
   * GlobalRenderInformation object.
   *
   */
  XMLNode toXML() const;

  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets this SBML object to child SBML objects (if any).
   * (Creates a child-parent relationship by the parent)
   *
   * Subclasses must override this function if they define
   * one ore more child elements.
   * Basically, this function needs to be called in
   * constructor, copy constructor, assignment operator.
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  virtual void connectToChild ();
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Enables/Disables the given package with this element and child
   * elements (if any).
   * (This is an internal implementation for enablePakcage function)
   *
   * @note Subclasses in which one or more child elements are defined
   * must override this function.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
    const std::string& pkgPrefix, bool flag);
  /** @endcond */


protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */


};

class DefaultValues;

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */

#endif /* GlobalRenderInformation_H__ */
