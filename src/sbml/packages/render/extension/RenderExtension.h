/**
 * @file    RenderExtension.h
 * @brief   Definition of RenderExtension, the core module of the render package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#ifndef RenderExtension_h
#define RenderExtension_h

#include <sbml/common/extern.h>

#ifdef __cplusplus

#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>

#ifndef RENDER_CREATE_NS
#define RENDER_CREATE_NS(variable,sbmlns)\
  EXTENSION_CREATE_NS(RenderPkgNamespaces,variable,sbmlns);
#endif

#include <vector>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN RenderExtension : public SBMLExtension
{
public:

  //---------------------------------------------------------------
  //
  // Required class methods
  //
  //---------------------------------------------------------------

  /**
   * Returns the package name of this extension.
   */
  static const std::string& getPackageName ();

  /**
   * Returns the default SBML Level this extension.
   */
  static unsigned int getDefaultLevel();

  /**
   * Returns the default SBML Version this extension.
   */
  static unsigned int getDefaultVersion();

  /**
   * Returns the default SBML version this extension.
   */
  static unsigned int getDefaultPackageVersion();

  /**
   * Returns URI of supported versions of this package.
   */
  static const std::string&  getXmlnsL3V1V1();
  static const std::string&  getXmlnsL2();

  //
  // Other URI needed in this package (if any)
  //

  //---------------------------------------------------------------


  /**
   * Constructor
   */
  RenderExtension ();


  /**
   * Copy constructor.
   */
  RenderExtension(const RenderExtension&);


  /**
   * Destroy this object.
   */
  virtual ~RenderExtension ();


  /**
   * Assignment operator for GroupsExtension.
   */
  RenderExtension& operator=(const RenderExtension&);


  /**
   * Creates and returns a deep copy of this RenderExtension object.
   * 
   * @return a (deep) copy of this RenderExtension object
   */
  virtual RenderExtension* clone () const;


  /**
   * Returns the name of this package ("fbc")
   *
   * @pram the name of this package ("fbc")
   */
  virtual const std::string& getName() const;


  /**
   * Returns the URI (namespace) of the package corresponding to the combination of 
   * the given sbml level, sbml version, and package version.
   * Empty string will be returned if no corresponding URI exists.
   *
   * @param sbmlLevel the level of SBML
   * @param sbmlVersion the version of SBML
   * @param pkgVersion the version of package
   *
   * @return a string of the package URI
   */
  virtual const std::string& getURI(unsigned int sbmlLevel, unsigned int sbmlVersion, 
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML level with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces<GroupsExtension> object whose alias type is 
   * LayoutPkgNamespace.
   * Null will be returned if the given uri is not defined in the layout package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return an LayoutPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in layout package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code of groups package and returns a string representing 
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the entry in the error table at this index.
   *
   * @param index an unsigned integer representing the index of the error.
   *
   * @return packageErrorTableEntry object in the RenderSBMLErrorTable.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId.
   *
   * @param errorId an unsigned integer representing the errorId of the error.
   *
   * @return unsigned int representing the index in the RenderSBMLErrorTable
   * corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the offset for the errorId range for the "render" package.
   *
   * @return unsigned int representing the offset for errors in the
   * RenderSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */
  /**
   * Initializes layout extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in GroupsExtension.cpp
   *
   *        static SBMLExtensionRegister<GroupsExtension> groupsExtensionRegistry;
   *
   */

  static void init();

  /** @endcond */

  /**
   * Removes the L2 Namespace from a document. 
   *
   * This method should be overridden by all extensions that want to serialize
   * to an L2 annotation.
   */
  virtual void removeL2Namespaces(XMLNamespaces* xmlns)  const;

  
  /**
   * adds all L2 Extension namespaces to the namespace list. 
   * 
   * This method should be overridden by all extensions that want to serialize
   * to an L2 annotation.
   */
  virtual void addL2Namespaces(XMLNamespaces *xmlns) const;

  /**
   * Adds the L2 Namespace to the document and enables the extension.
   *
   * If the extension supports serialization to SBML L2 Annotations, this 
   * method should be overrridden, so it will be activated.
   */
  virtual void enableL2NamespaceForDocument(SBMLDocument* doc)  const;

  /** 
   * Determines whether this extension is being used by the given SBMLDocument
   *
   * The implementation returns true if the list of layouts contains a global render information,
   * or a layout object contains a local render information object. 
   * 
   * @param doc the sbml document to test. 
   * 
   * @return a boolean indicating whether the extension is actually being used
   *         byy the document. 
   */
  virtual bool isInUse(SBMLDocument *doc) const;

};



//
// (NOTE) 
//
// SBMLExtensionNamespaces<RenderExtension> must be instantiated
// in RenderExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<RenderExtension> RenderPkgNamespaces; 

typedef enum
{
    SBML_RENDER_COLORDEFINITION = 1000
  , SBML_RENDER_ELLIPSE = 1001
  , SBML_RENDER_GLOBALRENDERINFORMATION = 1002
  , SBML_RENDER_GLOBALSTYLE = 1003
  , SBML_RENDER_GRADIENTDEFINITION = 1004
  , SBML_RENDER_GRADIENT_STOP = 1005
  , SBML_RENDER_GROUP = 1006
  , SBML_RENDER_IMAGE = 1007
  , SBML_RENDER_LINEENDING = 1008
  , SBML_RENDER_LINEARGRADIENT = 1009
  , SBML_RENDER_LINESEGMENT = 1010
  , SBML_RENDER_LISTOFGLOBALSTYLES = 1011
  , SBML_RENDER_LISTOFLOCALSTYLES = 1012
  , SBML_RENDER_LOCALRENDERINFORMATION = 1013
  , SBML_RENDER_LOCALSTYLE = 1014
  , SBML_RENDER_POLYGON = 1015
  , SBML_RENDER_RADIALGRADIENT = 1016
  , SBML_RENDER_RECTANGLE = 1017
  , SBML_RENDER_RELABSVECTOR = 1018
  , SBML_RENDER_CUBICBEZIER = 1019
  , SBML_RENDER_CURVE = 1020
  , SBML_RENDER_POINT = 1021
  , SBML_RENDER_TEXT = 1022
  , SBML_RENDER_TRANSFORMATION2D = 1023
  , SBML_RENDER_DEFAULTS = 1024
, SBML_RENDER_TRANSFORMATION              =  1025  /*!<Transformation */
, SBML_RENDER_GRAPHICALPRIMITIVE1D        =  1026  /*!<GraphicalPrimitive1D */
, SBML_RENDER_GRAPHICALPRIMITIVE2D        =  1027  /*!<GraphicalPrimitive2D */
, SBML_RENDER_STYLE_BASE                  =  1028  /*!<Style */
, SBML_RENDER_RENDERINFORMATION_BASE      =  1029  /*!<RenderInformationBase */
} SBMLRenderTypeCode_t;


///**
// * @enum StyleType_t
// * @brief Enumeration of values permitted as the value of the "type" attribute
// * on Style objects.
// *
// * @if conly
// * @see Style_getType()
// * @see Style_setType()
// * @elseif java
// * @see Style::getType()
// * @see Style::setType(long)
// * @else
// * @see Style::getType()
// * @see Style::setType()
// * @endif
// */
//typedef enum
//{
//  STYLE_TYPE_COMPARTMENTGLYPH            /*!< The style type is @c "COMPARTMENTGLYPH". */
//, STYLE_TYPE_SPECIESGLYPH                /*!< The style type is @c "SPECIESGLYPH". */
//, STYLE_TYPE_REACTIONGLYPH               /*!< The style type is @c "REACTIONGLYPH". */
//, STYLE_TYPE_SPECIESREFERENCEGLYPH       /*!< The style type is @c "SPECIESREFERENCEGLYPH". */
//, STYLE_TYPE_TEXTGLYPH                   /*!< The style type is @c "TEXTGLYPH". */
//, STYLE_TYPE_GENERALGLYPH                /*!< The style type is @c "GENERALGLYPH". */
//, STYLE_TYPE_GRAPHICALOBJECT             /*!< The style type is @c "GRAPHICALOBJECT". */
//, STYLE_TYPE_ANY                         /*!< The style type is @c "ANY". */
//, STYLE_TYPE_INVALID                     /*!< Invalid StyleType value. */
//} StyleType_t;
//
//
///**
// * Returns the string version of the provided #StyleType_t enumeration.
// *
// * @param st the #StyleType_t enumeration value to convert.
// *
// * @return A string corresponding to the given type:
// * "COMPARTMENTGLYPH",
// * "SPECIESGLYPH",
// * "REACTIONGLYPH",
// * "SPECIESREFERENCEGLYPH",
// * "TEXTGLYPH",
// * "GENERALGLYPH",
// * "GRAPHICALOBJECT",
// * "ANY",
// * or @c NULL if the value is @sbmlconstant{STYLE_TYPE_INVALID, StyleType_t} or
// * another invalid enumeration value.
// *
// * @copydetails doc_returned_unowned_char
// *
// * @if conly
// * @memberof Style_t
// * @endif
// */
//LIBSBML_EXTERN
//const char*
//StyleType_toString(StyleType_t st);
//
//
///**
// * Returns the #StyleType_t enumeration corresponding to the given string or
// * @sbmlconstant{STYLE_TYPE_INVALID, StyleType_t} if there is no such match.
// *
// * @param code the string to convert to a #StyleType_t.
// *
// * @return the corresponding #StyleType_t or @sbmlconstant{STYLE_TYPE_INVALID,
// * StyleType_t} if no match is found.
// *
// * @note The matching is case-sensitive: "COMPARTMENTGLYPH" will return
// * @sbmlconstant{STYLE_TYPE_COMPARTMENTGLYPH, StyleType_t}, but
// * "COMPARTMENTGLYPH" will return @sbmlconstant{STYLE_TYPE_INVALID,
// * StyleType_t}.
// *
// * @if conly
// * @memberof Style_t
// * @endif
// */
//LIBSBML_EXTERN
//StyleType_t
//StyleType_fromString(const char* code);
//
//
///**
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given #StyleType_t is valid.
// *
// * @param st the #StyleType_t enumeration to query.
// *
// * @return @c 1 (true) if the #StyleType_t is
// * @sbmlconstant{STYLE_TYPE_COMPARTMENTGLYPH, StyleType_t},
// * @sbmlconstant{STYLE_TYPE_SPECIESGLYPH, StyleType_t},
// * @sbmlconstant{STYLE_TYPE_REACTIONGLYPH, StyleType_t},
// * @sbmlconstant{STYLE_TYPE_SPECIESREFERENCEGLYPH, StyleType_t},
// * @sbmlconstant{STYLE_TYPE_TEXTGLYPH, StyleType_t},
// * @sbmlconstant{STYLE_TYPE_GENERALGLYPH, StyleType_t},
// * @sbmlconstant{STYLE_TYPE_GRAPHICALOBJECT, StyleType_t}, or
// * @sbmlconstant{STYLE_TYPE_ANY, StyleType_t};
// * @c 0 (false) otherwise (including @sbmlconstant{STYLE_TYPE_INVALID,
// * StyleType_t}).
// *
// * @if conly
// * @memberof Style_t
// * @endif
// */
//LIBSBML_EXTERN
//int
//StyleType_isValid(StyleType_t st);
//
//
///**
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given string is a valid #StyleType_t.
// *
// * @param code the string to query.
// *
// * @return @c 1 (true) if the string is
// * "COMPARTMENTGLYPH",
// * "SPECIESGLYPH",
// * "REACTIONGLYPH",
// * "SPECIESREFERENCEGLYPH",
// * "TEXTGLYPH",
// * "GENERALGLYPH",
// * "GRAPHICALOBJECT", or
// * "ANY";
// * @c 0 (false) otherwise.
// *
// * @note The matching is case-sensitive: "COMPARTMENTGLYPH" will return @c 1
// * (true), but "COMPARTMENTGLYPH" will return @c 0 (false).
// *
// * @if conly
// * @memberof Style_t
// * @endif
// */
//LIBSBML_EXTERN
//int
//StyleType_isValidString(const char* code);
//
//
/**
 * @enum GradientSpreadMethod_t
 * @brief Enumeration of values permitted as the value of the "spreadmethod"
 * attribute on Gradient objects.
 *
 * @if conly
 * @see Gradient_getSpreadmethod()
 * @see Gradient_setSpreadmethod()
 * @elseif java
 * @see Gradient::getSpreadmethod()
 * @see Gradient::setSpreadmethod(long)
 * @else
 * @see Gradient::getSpreadmethod()
 * @see Gradient::setSpreadmethod()
 * @endif
 */
typedef enum
{
  GRADIENT_SPREADMETHOD_PAD            /*!< The gradient spreadmethod is @c "pad". */
, GRADIENT_SPREADMETHOD_REFLECT        /*!< The gradient spreadmethod is @c "reflect". */
, GRADIENT_SPREADMETHOD_REPEAT         /*!< The gradient spreadmethod is @c "repeat". */
, GRADIENT_SPREAD_METHOD_INVALID       /*!< Invalid GradientSpreadMethod value. */
} GradientSpreadMethod_t;


/**
 * Returns the string version of the provided #GradientSpreadMethod_t
 * enumeration.
 *
 * @param gsm the #GradientSpreadMethod_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "pad",
 * "reflect",
 * "repeat",
 * or @c NULL if the value is @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID,
 * GradientSpreadMethod_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Gradient_t
 * @endif
 */
LIBSBML_EXTERN
const char*
GradientSpreadMethod_toString(GradientSpreadMethod_t gsm);


/**
 * Returns the #GradientSpreadMethod_t enumeration corresponding to the given
 * string or @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID,
 * GradientSpreadMethod_t} if there is no such match.
 *
 * @param code the string to convert to a #GradientSpreadMethod_t.
 *
 * @return the corresponding #GradientSpreadMethod_t or
 * @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID, GradientSpreadMethod_t} if no
 * match is found.
 *
 * @note The matching is case-sensitive: "pad" will return
 * @sbmlconstant{GRADIENT_SPREADMETHOD_PAD, GradientSpreadMethod_t}, but "Pad"
 * will return @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID,
 * GradientSpreadMethod_t}.
 *
 * @if conly
 * @memberof Gradient_t
 * @endif
 */
LIBSBML_EXTERN
GradientSpreadMethod_t
GradientSpreadMethod_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #GradientSpreadMethod_t is valid.
 *
 * @param gsm the #GradientSpreadMethod_t enumeration to query.
 *
 * @return @c 1 (true) if the #GradientSpreadMethod_t is
 * @sbmlconstant{GRADIENT_SPREADMETHOD_PAD, GradientSpreadMethod_t},
 * @sbmlconstant{GRADIENT_SPREADMETHOD_REFLECT, GradientSpreadMethod_t}, or
 * @sbmlconstant{GRADIENT_SPREADMETHOD_REPEAT, GradientSpreadMethod_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID, GradientSpreadMethod_t}).
 *
 * @if conly
 * @memberof Gradient_t
 * @endif
 */
LIBSBML_EXTERN
int
GradientSpreadMethod_isValid(GradientSpreadMethod_t gsm);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #GradientSpreadMethod_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "pad",
 * "reflect", or
 * "repeat";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "pad" will return @c 1 (true), but
 * "Pad" will return @c 0 (false).
 *
 * @if conly
 * @memberof Gradient_t
 * @endif
 */
LIBSBML_EXTERN
int
GradientSpreadMethod_isValidString(const char* code);


/**
 * @enum FillRule_t
 * @brief Enumeration of values permitted as the value of the "rule" attribute
 * on Fill objects.
 *
 * @if conly
 * @see Fill_getRule()
 * @see Fill_setRule()
 * @elseif java
 * @see Fill::getRule()
 * @see Fill::setRule(long)
 * @else
 * @see Fill::getRule()
 * @see Fill::setRule()
 * @endif
 */
typedef enum
{
  FILL_RULE_UNSET         /*!< The fill rule is @c "unset". */
, FILL_RULE_NONZERO       /*!< The fill rule is @c "nonzero". */
, FILL_RULE_EVENODD       /*!< The fill rule is @c "evenodd". */
, FILL_RULE_INHERIT       /*!< The fill rule is @c "inherit". */
, FILL_RULE_INVALID       /*!< Invalid FillRule value. */
} FillRule_t;


/**
 * Returns the string version of the provided #FillRule_t enumeration.
 *
 * @param fr the #FillRule_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "unset",
 * "nonzero",
 * "evenodd",
 * "inherit",
 * or @c NULL if the value is @sbmlconstant{FILL_RULE_INVALID, FillRule_t} or
 * another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Fill_t
 * @endif
 */
LIBSBML_EXTERN
const char*
FillRule_toString(FillRule_t fr);


/**
 * Returns the #FillRule_t enumeration corresponding to the given string or
 * @sbmlconstant{FILL_RULE_INVALID, FillRule_t} if there is no such match.
 *
 * @param code the string to convert to a #FillRule_t.
 *
 * @return the corresponding #FillRule_t or @sbmlconstant{FILL_RULE_INVALID,
 * FillRule_t} if no match is found.
 *
 * @note The matching is case-sensitive: "unset" will return
 * @sbmlconstant{FILL_RULE_UNSET, FillRule_t}, but "Unset" will return
 * @sbmlconstant{FILL_RULE_INVALID, FillRule_t}.
 *
 * @if conly
 * @memberof Fill_t
 * @endif
 */
LIBSBML_EXTERN
FillRule_t
FillRule_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FillRule_t is valid.
 *
 * @param fr the #FillRule_t enumeration to query.
 *
 * @return @c 1 (true) if the #FillRule_t is
 * @sbmlconstant{FILL_RULE_UNSET, FillRule_t},
 * @sbmlconstant{FILL_RULE_NONZERO, FillRule_t},
 * @sbmlconstant{FILL_RULE_EVENODD, FillRule_t}, or
 * @sbmlconstant{FILL_RULE_INHERIT, FillRule_t};
 * @c 0 (false) otherwise (including @sbmlconstant{FILL_RULE_INVALID,
 * FillRule_t}).
 *
 * @if conly
 * @memberof Fill_t
 * @endif
 */
LIBSBML_EXTERN
int
FillRule_isValid(FillRule_t fr);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FillRule_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "unset",
 * "nonzero",
 * "evenodd", or
 * "inherit";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "unset" will return @c 1 (true), but
 * "Unset" will return @c 0 (false).
 *
 * @if conly
 * @memberof Fill_t
 * @endif
 */
LIBSBML_EXTERN
int
FillRule_isValidString(const char* code);


/**
 * @enum FontFamily_t
 * @brief Enumeration of values permitted as the value of the "family"
 * attribute on Font objects.
 *
 * @if conly
 * @see Font_getFamily()
 * @see Font_setFamily()
 * @elseif java
 * @see Font::getFamily()
 * @see Font::setFamily(long)
 * @else
 * @see Font::getFamily()
 * @see Font::setFamily()
 * @endif
 */
typedef enum
{
  FONT_FAMILY_SERIF            /*!< The font family is @c "serif". */
, FONT_FAMILY_SANS_SERIF       /*!< The font family is @c "sans-serif". */
, FONT_FAMILY_MONOSPACE        /*!< The font family is @c "monospace". */
, FONT_FAMILY_INVALID          /*!< Invalid FontFamily value. */
} FontFamily_t;


/**
 * Returns the string version of the provided #FontFamily_t enumeration.
 *
 * @param ff the #FontFamily_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "serif",
 * "sans-serif",
 * "monospace",
 * or @c NULL if the value is @sbmlconstant{FONT_FAMILY_INVALID, FontFamily_t}
 * or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
const char*
FontFamily_toString(FontFamily_t ff);


/**
 * Returns the #FontFamily_t enumeration corresponding to the given string or
 * @sbmlconstant{FONT_FAMILY_INVALID, FontFamily_t} if there is no such match.
 *
 * @param code the string to convert to a #FontFamily_t.
 *
 * @return the corresponding #FontFamily_t or
 * @sbmlconstant{FONT_FAMILY_INVALID, FontFamily_t} if no match is found.
 *
 * @note The matching is case-sensitive: "serif" will return
 * @sbmlconstant{FONT_FAMILY_SERIF, FontFamily_t}, but "Serif" will return
 * @sbmlconstant{FONT_FAMILY_INVALID, FontFamily_t}.
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
FontFamily_t
FontFamily_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FontFamily_t is valid.
 *
 * @param ff the #FontFamily_t enumeration to query.
 *
 * @return @c 1 (true) if the #FontFamily_t is
 * @sbmlconstant{FONT_FAMILY_SERIF, FontFamily_t},
 * @sbmlconstant{FONT_FAMILY_SANS_SERIF, FontFamily_t}, or
 * @sbmlconstant{FONT_FAMILY_MONOSPACE, FontFamily_t};
 * @c 0 (false) otherwise (including @sbmlconstant{FONT_FAMILY_INVALID,
 * FontFamily_t}).
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
int
FontFamily_isValid(FontFamily_t ff);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FontFamily_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "serif",
 * "sans-serif", or
 * "monospace";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "serif" will return @c 1 (true), but
 * "Serif" will return @c 0 (false).
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
int
FontFamily_isValidString(const char* code);


/**
 * @enum FontWeight_t
 * @brief Enumeration of values permitted as the value of the "weight"
 * attribute on Font objects.
 *
 * @if conly
 * @see Font_getWeight()
 * @see Font_setWeight()
 * @elseif java
 * @see Font::getWeight()
 * @see Font::setWeight(long)
 * @else
 * @see Font::getWeight()
 * @see Font::setWeight()
 * @endif
 */
typedef enum
{
  FONT_WEIGHT_UNSET         /*!< The font weight is @c "unset". */
, FONT_WEIGHT_NORMAL        /*!< The font weight is @c "normal". */
, FONT_WEIGHT_BOLD          /*!< The font weight is @c "bold". */
, FONT_WEIGHT_INVALID       /*!< Invalid FontWeight value. */
} FontWeight_t;


/**
 * Returns the string version of the provided #FontWeight_t enumeration.
 *
 * @param fw the #FontWeight_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "bold",
 * "normal",
 * or @c NULL if the value is @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t}
 * or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
const char*
FontWeight_toString(FontWeight_t fw);


/**
 * Returns the #FontWeight_t enumeration corresponding to the given string or
 * @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t} if there is no such match.
 *
 * @param code the string to convert to a #FontWeight_t.
 *
 * @return the corresponding #FontWeight_t or
 * @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t} if no match is found.
 *
 * @note The matching is case-sensitive: "bold" will return
 * @sbmlconstant{FONT_WEIGHT_BOLD, FontWeight_t}, but "Bold" will return
 * @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t}.
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
FontWeight_t
FontWeight_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FontWeight_t is valid.
 *
 * @param fw the #FontWeight_t enumeration to query.
 *
 * @return @c 1 (true) if the #FontWeight_t is
 * @sbmlconstant{FONT_WEIGHT_BOLD, FontWeight_t}, or
 * @sbmlconstant{FONT_WEIGHT_NORMAL, FontWeight_t};
 * @c 0 (false) otherwise (including @sbmlconstant{FONT_WEIGHT_INVALID,
 * FontWeight_t}).
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
int
FontWeight_isValid(FontWeight_t fw);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FontWeight_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "bold", or
 * "normal";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "bold" will return @c 1 (true), but
 * "Bold" will return @c 0 (false).
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
int
FontWeight_isValidString(const char* code);


/**
 * @enum FontStyle_t
 * @brief Enumeration of values permitted as the value of the "style" attribute
 * on Font objects.
 *
 * @if conly
 * @see Font_getStyle()
 * @see Font_setStyle()
 * @elseif java
 * @see Font::getStyle()
 * @see Font::setStyle(long)
 * @else
 * @see Font::getStyle()
 * @see Font::setStyle()
 * @endif
 */
typedef enum
{
  FONT_STYLE_UNSET         /*!< The font style is @c "unset". */
, FONT_STYLE_NORMAL        /*!< The font style is @c "normal". */
, FONT_STYLE_ITALIC        /*!< The font style is @c "italic". */
, FONT_STYLE_INVALID       /*!< Invalid FontStyle value. */
} FontStyle_t;


/**
 * Returns the string version of the provided #FontStyle_t enumeration.
 *
 * @param fs the #FontStyle_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "italic",
 * "normal",
 * or @c NULL if the value is @sbmlconstant{FONT_STYLE_INVALID, FontStyle_t} or
 * another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
const char*
FontStyle_toString(FontStyle_t fs);


/**
 * Returns the #FontStyle_t enumeration corresponding to the given string or
 * @sbmlconstant{FONT_STYLE_INVALID, FontStyle_t} if there is no such match.
 *
 * @param code the string to convert to a #FontStyle_t.
 *
 * @return the corresponding #FontStyle_t or @sbmlconstant{FONT_STYLE_INVALID,
 * FontStyle_t} if no match is found.
 *
 * @note The matching is case-sensitive: "italic" will return
 * @sbmlconstant{FONT_STYLE_ITALIC, FontStyle_t}, but "Italic" will return
 * @sbmlconstant{FONT_STYLE_INVALID, FontStyle_t}.
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
FontStyle_t
FontStyle_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FontStyle_t is valid.
 *
 * @param fs the #FontStyle_t enumeration to query.
 *
 * @return @c 1 (true) if the #FontStyle_t is
 * @sbmlconstant{FONT_STYLE_ITALIC, FontStyle_t}, or
 * @sbmlconstant{FONT_STYLE_NORMAL, FontStyle_t};
 * @c 0 (false) otherwise (including @sbmlconstant{FONT_STYLE_INVALID,
 * FontStyle_t}).
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
int
FontStyle_isValid(FontStyle_t fs);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FontStyle_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "italic", or
 * "normal";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "italic" will return @c 1 (true), but
 * "Italic" will return @c 0 (false).
 *
 * @if conly
 * @memberof Font_t
 * @endif
 */
LIBSBML_EXTERN
int
FontStyle_isValidString(const char* code);


/**
 * @enum VTextAnchor_t
 * @brief Enumeration of values permitted as the value of the "vtext-anchor"
 * attribute on various objects.
 *
 * @if conly
 * @see V_getTextanchor()
 * @see V_setTextanchor()
 * @elseif java
 * @see V::getTextanchor()
 * @see V::setTextanchor(long)
 * @else
 * @see V::getTextanchor()
 * @see V::setTextanchor()
 * @endif
 */
typedef enum
{
  V_TEXTANCHOR_UNSET          /*!< The vertical text anchor is @c "unset". */
, V_TEXTANCHOR_TOP            /*!< The vertical text anchor is @c "top". */
, V_TEXTANCHOR_MIDDLE         /*!< The vertical text anchor is @c "middle". */
, V_TEXTANCHOR_BOTTOM         /*!< The vertical text anchor is @c "bottom". */
, V_TEXTANCHOR_BASELINE       /*!< The vertical text anchor is @c "baseline". */
, V_TEXTANCHOR_INVALID        /*!< Invalid VTextAnchor value. */
} VTextAnchor_t;


/**
 * Returns the string version of the provided #VTextAnchor_t enumeration.
 *
 * @param vta the #VTextAnchor_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "top",
 * "middle",
 * "bottom",
 * "baseline",
 * or @c NULL if the value is @sbmlconstant{V_TEXTANCHOR_INVALID,
 * VTextAnchor_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof V_t
 * @endif
 */
LIBSBML_EXTERN
const char*
VTextAnchor_toString(VTextAnchor_t vta);


/**
 * Returns the #VTextAnchor_t enumeration corresponding to the given string or
 * @sbmlconstant{V_TEXTANCHOR_INVALID, VTextAnchor_t} if there is no
 * such match.
 *
 * @param code the string to convert to a #VTextAnchor_t.
 *
 * @return the corresponding #VTextAnchor_t or
 * @sbmlconstant{V_TEXTANCHOR_INVALID, VTextAnchor_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "top" will return
 * @sbmlconstant{V_TEXTANCHOR_TOP, VTextAnchor_t}, but "Top" will return
 * @sbmlconstant{V_TEXTANCHOR_INVALID, VTextAnchor_t}.
 *
 * @if conly
 * @memberof V_t
 * @endif
 */
LIBSBML_EXTERN
VTextAnchor_t
VTextAnchor_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #VTextAnchor_t is valid.
 *
 * @param vta the #VTextAnchor_t enumeration to query.
 *
 * @return @c 1 (true) if the #VTextAnchor_t is
 * @sbmlconstant{V_TEXTANCHOR_TOP, VTextAnchor_t},
 * @sbmlconstant{V_TEXTANCHOR_MIDDLE, VTextAnchor_t},
 * @sbmlconstant{V_TEXTANCHOR_BOTTOM, VTextAnchor_t}, or
 * @sbmlconstant{V_TEXTANCHOR_BASELINE, VTextAnchor_t};
 * @c 0 (false) otherwise (including @sbmlconstant{V_TEXTANCHOR_INVALID,
 * VTextAnchor_t}).
 *
 * @if conly
 * @memberof V_t
 * @endif
 */
LIBSBML_EXTERN
int
VTextAnchor_isValid(VTextAnchor_t vta);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #VTextAnchor_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "top",
 * "middle",
 * "bottom", or
 * "baseline";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "top" will return @c 1 (true), but
 * "Top" will return @c 0 (false).
 *
 * @if conly
 * @memberof V_t
 * @endif
 */
LIBSBML_EXTERN
int
VTextAnchor_isValidString(const char* code);


/**
 * @enum HTextAnchor_t
 * @brief Enumeration of values permitted as the value of the "text-anchor"
 * attribute on various objects.
 *
 * @if conly
 * @see H_getTextanchor()
 * @see H_setTextanchor()
 * @elseif java
 * @see H::getTextanchor()
 * @see H::setTextanchor(long)
 * @else
 * @see H::getTextanchor()
 * @see H::setTextanchor()
 * @endif
 */
typedef enum
{
  H_TEXTANCHOR_UNSET         /*!< The horizontal textanchor is @c "unset". */
, H_TEXTANCHOR_START         /*!< The horizontal textanchor is @c "start". */
, H_TEXTANCHOR_MIDDLE        /*!< The horizontal textanchor is @c "middle". */
, H_TEXTANCHOR_END           /*!< The horizontal textanchor is @c "end". */
, H_TEXTANCHOR_INVALID       /*!< Invalid HTextAnchor value. */
} HTextAnchor_t;


/**
 * Returns the string version of the provided #HTextAnchor_t enumeration.
 *
 * @param hta the #HTextAnchor_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "start",
 * "middle",
 * "end",
 * or @c NULL if the value is @sbmlconstant{H_TEXTANCHOR_INVALID,
 * HTextAnchor_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof H_t
 * @endif
 */
LIBSBML_EXTERN
const char*
HTextAnchor_toString(HTextAnchor_t hta);


/**
 * Returns the #HTextAnchor_t enumeration corresponding to the given string or
 * @sbmlconstant{H_TEXTANCHOR_INVALID, HTextAnchor_t} if there is no such
 * match.
 *
 * @param code the string to convert to a #HTextAnchor_t.
 *
 * @return the corresponding #HTextAnchor_t or
 * @sbmlconstant{H_TEXTANCHOR_INVALID, HTextAnchor_t} if no match is found.
 *
 * @note The matching is case-sensitive: "start" will return
 * @sbmlconstant{H_TEXTANCHOR_START, HTextAnchor_t}, but "Start" will return
 * @sbmlconstant{H_TEXTANCHOR_INVALID, HTextAnchor_t}.
 *
 * @if conly
 * @memberof H_t
 * @endif
 */
LIBSBML_EXTERN
HTextAnchor_t
HTextAnchor_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #HTextAnchor_t is valid.
 *
 * @param hta the #HTextAnchor_t enumeration to query.
 *
 * @return @c 1 (true) if the #HTextAnchor_t is
 * @sbmlconstant{H_TEXTANCHOR_START, HTextAnchor_t},
 * @sbmlconstant{H_TEXTANCHOR_MIDDLE, HTextAnchor_t}, or
 * @sbmlconstant{H_TEXTANCHOR_END, HTextAnchor_t};
 * @c 0 (false) otherwise (including @sbmlconstant{H_TEXTANCHOR_INVALID,
 * HTextAnchor_t}).
 *
 * @if conly
 * @memberof H_t
 * @endif
 */
LIBSBML_EXTERN
int
HTextAnchor_isValid(HTextAnchor_t hta);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #HTextAnchor_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "start",
 * "middle", or
 * "end";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "start" will return @c 1 (true), but
 * "Start" will return @c 0 (false).
 *
 * @if conly
 * @memberof H_t
 * @endif
 */
LIBSBML_EXTERN
int
HTextAnchor_isValidString(const char* code);



LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* RenderExtension_h */
