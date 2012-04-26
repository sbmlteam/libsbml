/**
 * @file    L3ParserSettings.h
 * @brief   Definition of the level 3 infix-to-mathml parser settings.
 * @author  Lucian Smith
 * 
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 *
 * @class L3ParserSettings
 * @brief A helper class for modulating how the function SBML_parseL3Formula behaves.
 *
 * An L3ParserSettings object can be created to pass in to the function
 * SBML_parseL3Formula to change its behavior:  how a given infix string
 * will be translated to ASTNodes.  The behavior can be changed in the
 * following ways:
 * 
 * @li A Model object can be compared to the infix, so that the IDs of function definitions defined
     in the model with the same ID as built-in MathML functions will be translated
     as functions, and not as the built-in MathML function.  Similarly, elements found
     in the model whose IDs may be used in MathML expressions whose IDs may also match 
     pre-defined SBML terms (time, avogadro, infinity, etc.) will be translated as 
     element IDs, and not as the pre-defined SBML term.
 * @li The function 'log' with one argument ('log(x)') can be interpreted three
     different ways:  as the log base 10 (the default), as the natural log (<ln/>),
     or as an error.
 * @li A 'unary minus' (a minus sign that is associated with a single value to
     its right like '-x', as opposed to a minus sign that connects two variables
     like 'x - y') can be interpreted as a separate ASTNode in all cases (the default)
     or can be 'collapsed' if two in a row are encountered, or if the subject of
     the minus sign is a number.
 * @li The translated ASTNodes can be targetted for Level 3 SBML (the default) or 
     for Level 2.  When the target is L2, infix describing a number with a unit
     ('4.3 mL') is interpreted as an error (as L2 MathML cannot have units attached to 
     numbers, unlike L3 MathML), and the string 'avogadro' is translated as 
     the name of an element, and not the specially-defined 'avogadro' csymbol introduced
     in SBML Level 3.
 *
 */

#ifndef L3ParserSettings_h
#define L3ParserSettings_h

#include <sbml/common/libsbml-namespace.h>
#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


/** 
  * The l3p_log_type enum defines three options:
  * @li L3P_PARSE_LOG_AS_LOG10 (0):  parse 'log(x)' as the log base-10 of x, or 'log10(x)'.
  * @li L3P_PARSE_LOG_AS_LN (1): parse 'log(x)' as the natural log of x, or 'ln(x)'.
  * @li L3P_PARSE_LOG_AS_ERROR (2): refuse to parse 'log(x)' at all, and set an error message 
      telling the user to use 'log10(x)', 'ln(x)', or 'log(base, x)' instead.
  */
typedef enum {L3P_PARSE_LOG_AS_LOG10=0,
      L3P_PARSE_LOG_AS_LN=1,
      L3P_PARSE_LOG_AS_ERROR=2
     } l3p_log_type ;

#define L3P_COLLAPSE_UNARY_MINUS true
#define L3P_EXPAND_UNARY_MINUS   false

#define L3P_PARSE_UNITS  true
#define L3P_NO_UNITS false

#define L3P_AVOGADRO_IS_CSYMBOL true
#define L3P_AVOGADRO_IS_NAME    false

#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN

class Model;

class LIBSBML_EXTERN L3ParserSettings
{
private:
  Model* mModel;
  l3p_log_type mParselog;
  bool mCollapseminus;
  bool mParseunits;
  bool mAvoCsymbol;

public:
  /**
   * The default constructor for the L3ParserSettings object.  Sets the Model* to NULL, and other settings to L3P_PARSE_LOG_AS_LOG10, L3P_EXPAND_UNARY_MINUS, L3P_PARSE_UNITS, and L3P_AVOGADRO_IS_CSYMBOL.
   */
  L3ParserSettings();

  /**
   * The explicit constructor for the L3ParserSettings object.  All possible settings are defined explicitly by the caller.
   */
  L3ParserSettings(Model* model, l3p_log_type parselog, bool collapseminus, bool parseunits, bool avocsymbol);

  /**
   * Destroys this L3ParserSettings.
   */
  ~L3ParserSettings();


  /**
   * Sets the model of this L3ParserSettings to the provided pointer.  A copy of the Model is not made, so modifications to the Model itself may affect future parsing.
   *
   * @param model The Model* object to which infix strings are to be compared.
   */
  void setModel(Model* model);

  /*
   * Returns the model this L3ParserSettings object has currently stored to compare infix strings to.
   */
  Model* getModel() const;

  /**
   * Sets the model this L3ParserSettings object has currently stored to NULL, so no comparisons with any model will be performed when parsing.
   */
  void unsetModel();

  /**
   * Sets what to do with infix strings with the function 'log' with one argument, according to the provided type.
   *
   *@param type One of three options to set how 'log' with one argument is interpreted:
   * @li L3P_PARSE_LOG_AS_LOG10 (0)
   * @li L3P_PARSE_LOG_AS_LN (1)
   * @li L3P_PARSE_LOG_AS_ERROR (2)
   */
  void setParseLog(l3p_log_type type);

  /**
   * Returns the current setting indicating what to do with infix strings with the function 'log' with one argument.
   *
   *@return One of three options to set how 'log' with one argument is interpreted:
   * @li L3P_PARSE_LOG_AS_LOG10 (0)
   * @li L3P_PARSE_LOG_AS_LN (1)
   * @li L3P_PARSE_LOG_AS_ERROR (2)
   */
  l3p_log_type getParseLog() const;

  /**
   * Sets whether to collapse unary minuses (L3P_COLLAPSE_UNARY_MINUS, true) or whether to leave them expanded (L3P_EXPAND_UNARY_MINUS, false).  Unary minuses found in infix will always be translated into an ASTNode of type AST_MINUS in the latter case, and subsequent pairs will disappear entirely in the former case, as well as numbers (AST_INTEGER, AST_REAL, AST_REAL_E, and AST_RATIONAL) nodes being given negative values.
   *
   *@param collapseminus A boolean indicating whether to collapse unary minuses (L3P_COLLAPSE_UNARY_MINUS, true) or whether to leave them expanded (L3P_EXPAND_UNARY_MINUS, false).
   */
  void setCollapseMinus(bool collapseminus);

  /**
   * Returns whether the L3ParserSettings object is set to collapse unary minuses (L3P_COLLAPSE_UNARY_MINUS, true) or whether to leave them expanded (L3P_EXPAND_UNARY_MINUS, false).
   *
   *@return A boolean indicating whether unary minuses are set to be collapsed (L3P_COLLAPSE_UNARY_MINUS, true) or whether they are set to be left expanded (L3P_EXPAND_UNARY_MINUS, false).
   */
  bool getCollapseMinus() const;

  /**
   * Sets up this L3ParserSettings object to create ASTNodes that are capable of being used unmodified in SBML Level 2 documents:  this means that infix that associates a unit definition with a number is treated as an error, and that the string 'avogadro' is converted to an ASTNode of type AST_NAME instead of AST_NAME_AVOGADRO.
   */
  void targetL2();

  /**
   * Sets up this L3ParserSettings object to create ASTNodes that are targetted to the capabilities of SBML Level 3 documents:  this means that infix that associates a unit definition with a number is parsed correctly and used to set the 'unit' of the ASTNode, and that the string 'avogadro' is converted to an ASTNode of type AST_NAME_AVOGADRO instead of AST_NAME.
   */
  void targetL3();

  /**
   * Returns a boolean indicating whether this L3ParserSettings object is set up to produce ASTNodes appropriate for inclusion in SBML Level 2 documents:  units on numbers are treated as errors, and 'avogadro' is interpreted as AST_NAME and not AST_NAME_AVOGADRO.  (Both must be true, or this routine will return 'false'.)
   */
  bool getTargetL2() const;

  /**
   * Returns a boolean indicating whether this L3ParserSettings object is set up to produce ASTNodes that take full advantage of properties present in SBML Level 2 documents:  units are properly interpreted, and 'avogadro' is interpreted as AST_NAME_AVOGADRO and not AST_NAME.  (Both must be true, or this routine will return 'false'.)
   */
  bool getTargetL3() const;

  /**
   * Sets whether to parse units associated with number (L3P_PARSE_UNITS, true) or whether to treat units as errors (L3P_NO_UNITS, false).  SBML Level 3 allows the setting of a 'sbml:units' attribute on MathML <cn> elements, which is settable in infix as the unit definition ID string used after a number.  Some examples include:  "4 mL" (for integers), "2.01 Hz" (for reals), "3.1e-6 M" (for real numbers using e-notation), and "(5/8) inches" (for rational numbers).  To produce a valid SBML file, the UnitDefinition must exist in the final model, or the unit must be obtained from the list of pre-defined units in Table 2 of the SBML specification.
   *
   *@param units A boolean indicating whether to parse units (L3P_PARSE_UNITS, true) or whether to treat these strings as errors (L3P_NO_UNITS, false).
   */
  void setParseUnits(bool units);

  /**
   * Returns whether the L3ParserSettings object is set to parse units (L3P_PARSE_UNITS, true) or whether to treat these strings as errors (L3P_NO_UNITS, false).
   *
   *@return A boolean indicating whether units are set to be parsed (L3P_PARSE_UNITS, true) or whether they are set to be treated as errors (L3P_NO_UNITS, false).
   */
  bool getParseUnits() const;

  /**
   * Sets whether to translate the string "avogadro" (in any capitalization) into an ASTNode of type AST_NAME_AVOGADRO (L3P_AVOGADRO_IS_CSYMBOL, true) or whether to translate it into an ASTNode of type AST_NAME (L3P_AVOGADRO_IS_NAME, false).  'Avogadro' is a pre-defined csymbol in SBML Level 3, but was not defined in SBML Level 2, so this should be set to 'false' when parsing infix destined for SBML Level 2 documents.
   *
   *@param units A boolean indicating whether to parse "avogadro" as a csymbol (L3P_AVOGADRO_IS_CSYMBOL, true) or to parse it as a normal AST_NAME (L3P_AVOGADRO_IS_NAME, false).
   */
  void setAvogadroCsymbol(bool l2only);

  /**
   * Returns whether the L3ParserSettings object is set to parse "avogadro" as a csymbol (L3P_AVOGADRO_IS_CSYMBOL, true) or whether to parse it as a normal AST_NAME (L3P_AVOGADRO_IS_NAME, false).
   *
   *@return A boolean indicating whether "avogadro" is to be parsed as a csymbol (L3P_AVOGADRO_IS_CSYMBOL, true) or whether to parse it as a normal AST_NAME (L3P_AVOGADRO_IS_NAME, false).
   */
  bool getAvogadroCsymbol() const;
};


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


LIBSBML_EXTERN
L3ParserSettings_t *
L3ParserSettings_create ();


LIBSBML_EXTERN
void
L3ParserSettings_free (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setModel (L3ParserSettings_t * settings, Model_t * model);


LIBSBML_EXTERN
Model_t *
L3ParserSettings_getModel (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_unsetModel (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setParseLog (L3ParserSettings_t * settings, l3p_log_type type);


LIBSBML_EXTERN
l3p_log_type
L3ParserSettings_getParseLog (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setCollapseMinus (L3ParserSettings_t * settings, int flag);


LIBSBML_EXTERN
int
L3ParserSettings_getCollapseMinus (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setTargetL2 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
int
L3ParserSettings_getTargetL2 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setTargetL3 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
int
L3ParserSettings_getTargetL3 (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setParseUnits (L3ParserSettings_t * settings, int flag);


LIBSBML_EXTERN
int
L3ParserSettings_getParseUnits (L3ParserSettings_t * settings);


LIBSBML_EXTERN
void
L3ParserSettings_setAvogadroCsymbol (L3ParserSettings_t * settings, int flag);


LIBSBML_EXTERN
int
L3ParserSettings_getAvogadroCsymbol (L3ParserSettings_t * settings);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif

#endif /* L3ParserSettings_h */
