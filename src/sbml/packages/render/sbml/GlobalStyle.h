/**
 * @file    GlobalStyle.h
 * @brief Definition of the GlobalStyle class.
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
 * @class GlobalStyle
 * @sbmlbrief{render} A style that can be associated with a layout.
 *
 * Global styles are the style information objects used in
 * GlobalRenderInformation.  Global styles can be associated with layout
 * objects by role and type, but not by identifier; in other respects, global
 * styles and local styles are equivalent.
 *
 * Since GlobalStyle is derived from Styles, it inherits all of the methods
 * and attributes from Style.
 *
 * @see GlobalRenderInformation
 * @see Style
 */

#ifndef GlobalStyle_H__
#define GlobalStyle_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/Style.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN GlobalStyle : public Style
{

public:

  /**
   * Creates a new GlobalStyle using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GlobalStyle.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GlobalStyle.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this GlobalStyle.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GlobalStyle(unsigned int level = RenderExtension::getDefaultLevel(),
              unsigned int version = RenderExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GlobalStyle using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GlobalStyle(RenderPkgNamespaces *renderns);


  /**
   * Creates a new GlobalStyle object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * GlobalStyle object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the GlobalStyle
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  GlobalStyle(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a GlobalStyle with the given @p id
   * and all lists empty.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id the new id for the GlobalStyle.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  GlobalStyle(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for GlobalStyle.
   *
   * @param orig the GlobalStyle instance to copy.
   */
  GlobalStyle(const GlobalStyle& orig);


  /**
   * Assignment operator for GlobalStyle.
   *
   * @param rhs the GlobalStyle object whose values are to be used as the basis
   * of the assignment.
   */
  GlobalStyle& operator=(const GlobalStyle& rhs);


  /**
   * Creates and returns a deep copy of this GlobalStyle object.
   *
   * @return a (deep) copy of this GlobalStyle object.
   */
  virtual GlobalStyle* clone() const;


  /**
   * Destructor for GlobalStyle.
   */
  virtual ~GlobalStyle();


  /**
   * Returns the XML element name of this GlobalStyle object.
   *
   * For GlobalStyle, the XML element name is always @c "style".
   *
   * @return the name of this element, i.e. @c "style".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this GlobalStyle object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_GLOBALSTYLE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;

protected:
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


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new GlobalStyle_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GlobalStyle_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GlobalStyle_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * GlobalStyle_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GlobalStyle_t
 */
LIBSBML_EXTERN
GlobalStyle_t *
GlobalStyle_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GlobalStyle_t object.
 *
 * @param gs the GlobalStyle_t structure.
 *
 * @return a (deep) copy of this GlobalStyle_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GlobalStyle_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalStyle_clone(const GlobalStyle_t* gs);


/**
 * Frees this GlobalStyle_t object.
 *
 * @param gs the GlobalStyle_t structure.
 *
 * @memberof GlobalStyle_t
 */
LIBSBML_EXTERN
void
GlobalStyle_free(GlobalStyle_t* gs);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GlobalStyle_t object have been set.
 *
 * @param gs the GlobalStyle_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GlobalStyle_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GlobalStyle_t
 */
LIBSBML_EXTERN
int
GlobalStyle_hasRequiredAttributes(const GlobalStyle_t * gs);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !GlobalStyle_H__ */


