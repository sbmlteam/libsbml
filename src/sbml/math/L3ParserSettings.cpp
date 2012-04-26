/**
 * @file    L3ParserSettings.cpp
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
 * ---------------------------------------------------------------------- -->*/

#include <sbml/math/L3ParserSettings.h>
#include <cstddef>
#include <new>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */


L3ParserSettings::L3ParserSettings()
  : mModel (NULL)
  , mParselog(L3P_PARSE_LOG_AS_LOG10)
  , mCollapseminus(L3P_EXPAND_UNARY_MINUS)
  , mParseunits(L3P_PARSE_UNITS)
  , mAvoCsymbol(L3P_AVOGADRO_IS_CSYMBOL)
{
}

L3ParserSettings::L3ParserSettings(Model* model, l3p_log_type parselog, bool collapseminus, bool parseunits, bool avocsymbol)
  : mModel (model)
  , mParselog(parselog)
  , mCollapseminus(collapseminus)
  , mParseunits(parseunits)
  , mAvoCsymbol(avocsymbol)
{
}

L3ParserSettings::~L3ParserSettings()
{
}



void L3ParserSettings::setModel(Model* model)
{
  mModel = model;
}

Model* L3ParserSettings::getModel() const
{
  return mModel;
}

void L3ParserSettings::unsetModel()
{
  mModel = NULL;
}


void L3ParserSettings::setParseLog(l3p_log_type type)
{
  mParselog = type;
}

l3p_log_type L3ParserSettings::getParseLog() const
{
  return mParselog;
}


void L3ParserSettings::setCollapseMinus(bool collapseminus)
{
  mCollapseminus = collapseminus;
}

bool L3ParserSettings::getCollapseMinus() const
{
  return mCollapseminus;
}

void L3ParserSettings::targetL2()
{
  mParseunits = false;
  mAvoCsymbol = false;
}

void L3ParserSettings::targetL3()
{
  mParseunits = true;
  mAvoCsymbol = true;
}

bool L3ParserSettings::getTargetL2() const
{
  return (!mParseunits && !mAvoCsymbol);
}

bool L3ParserSettings::getTargetL3() const
{
  return (mParseunits && mAvoCsymbol);
}

void L3ParserSettings::setParseUnits(bool units)
{
  mParseunits = units;
}

bool L3ParserSettings::getParseUnits() const
{
  return mParseunits;
}


void L3ParserSettings::setAvogadroCsymbol(bool avo)
{
  mAvoCsymbol = avo;
}

bool L3ParserSettings::getAvogadroCsymbol() const
{
  return mAvoCsymbol;
}

/**
 * Creates a new L3ParserSettings_t object and returns a pointer to it
 *
 * @note This functions sets the Model* to NULL, and other settings to 
 * L3P_PARSE_LOG_AS_LOG10, L3P_EXPAND_UNARY_MINUS, L3P_PARSE_UNITS, 
 * and L3P_AVOGADRO_IS_CSYMBOL.
 *
 * @return a pointer to the newly created L3ParserSettings_t structure.
 */
LIBSBML_EXTERN
L3ParserSettings_t *
L3ParserSettings_create ()
{
  return new(nothrow) L3ParserSettings;
}


LIBSBML_EXTERN
void
L3ParserSettings_free (L3ParserSettings_t * settings)
{
  settings = NULL;
}
/**
 * Sets the model associated with this L3ParserSettings_t object
 * to the provided pointer.  
 *
 * @note A copy of the Model is not made, so modifications to the Model itself 
 * may affect future parsing.
 *
 * @param settings the L3ParserSettings_t structure on which to set the Model.
 * @param model The Model* object to which infix strings are to be compared.
 */
LIBSBML_EXTERN
void
L3ParserSettings_setModel (L3ParserSettings_t * settings, Model_t * model)
{
  if (settings == NULL)
    return;

  settings->setModel(model);
}


/**
 * Retrieves the model associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure from which to get the Model.
 *
 * @return the Model_t* object associated with this L3ParserSettings_t object.
 */
LIBSBML_EXTERN
Model_t *
L3ParserSettings_getModel (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return NULL;

  return settings->getModel();
}


/**
 * Unsets the model associated with this L3ParserSettings_t object.
 *
 * @param settings the L3ParserSettings_t structure on which to unset the Model.
 */
LIBSBML_EXTERN
void
L3ParserSettings_unsetModel (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return;

  settings->unsetModel();
}


/**
 * Sets the log parsing option associated with this L3ParserSettings_t object.  
 *
 * This option allows the user to specify how the infix expression 'log(x)'
 * is parsed in a MathML ASTNode. The options are:
 * @li L3P_PARSE_LOG_AS_LOG10 (0)
 * @li L3P_PARSE_LOG_AS_LN (1)
 * @li L3P_PARSE_LOG_AS_ERROR (2)
 *
 * @param settings the L3ParserSettings_t structure on which to set the option.
 * @param type l3p_log_type log parsing option to associate with this 
 * L3ParserSettings_t object.
 */
LIBSBML_EXTERN
void
L3ParserSettings_setParseLog (L3ParserSettings_t * settings, l3p_log_type type)
{
  if (settings == NULL)
    return;

  settings->setParseLog(type);
}


/**
 * Retrieves the log parsing option associated with this L3ParserSettings_t object.  
 *
 * This option allows the user to specify how the infix expression 'log(x)'
 * is parsed in a MathML ASTNode. The options are:
 * @li L3P_PARSE_LOG_AS_LOG10 (0)
 * @li L3P_PARSE_LOG_AS_LN (1)
 * @li L3P_PARSE_LOG_AS_ERROR (2)
 *
 * @param settings the L3ParserSettings_t structure on which to set the Model.
 *
 * @return l3p_log_type log parsing option to associate with this 
 * L3ParserSettings_t object.
 */
LIBSBML_EXTERN
l3p_log_type
L3ParserSettings_getParseLog (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return L3P_PARSE_LOG_AS_LOG10;

  return settings->getParseLog();
}


/**
 * Sets the collapse minus option associated with this L3ParserSettings_t object.  
 *
 * This option allows the user to specify how the infix expression '-4'
 * is parsed in a MathML ASTNode. 
 * 
 * @param settings the L3ParserSettings_t structure on which to set the option.
 * @param flag an integer indicating whether unary minus should be collapsed 
 * (non-zero) or not (zero).
 */
LIBSBML_EXTERN
void
L3ParserSettings_setCollapseMinus (L3ParserSettings_t * settings, int flag)
{
  if (settings == NULL)
    return;

  settings->setCollapseMinus(static_cast<bool>(flag));
}


/**
 * Retrieves the collapse minus option associated with this L3ParserSettings_t object.  
 *
 * This option allows the user to specify how the infix expression '-4'
 * is parsed in a MathML ASTNode. 
 * 
 * @param settings the L3ParserSettings_t structure from which to get the option.
 *
 * @return an integer indicating whether unary minus should be collapsed 
 * (non-zero) or not (zero).
 */
LIBSBML_EXTERN
int
L3ParserSettings_getCollapseMinus (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return 0;

  return (static_cast<int>(settings->getCollapseMinus()));
}


/**
 * Sets the targetL2 option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure on which to set the option.
 */
LIBSBML_EXTERN
void
L3ParserSettings_setTargetL2 (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return;

  settings->targetL2();
}


/**
 * Retrieves the targetL2 option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure from which to get the option.
 *
 * @return an integer indicating whether L2 math is being targeted (non-zero) 
 * or not (zero).
 */
LIBSBML_EXTERN
int
L3ParserSettings_getTargetL2 (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return 0;

  return (static_cast<int>(settings->getTargetL2()));
}


/**
 * Sets the targetL3 option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure on which to set the option.
 */
LIBSBML_EXTERN
void
L3ParserSettings_setTargetL3 (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return;

  settings->targetL3();
}


/**
 * Retrieves the targetL3 option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure from which to get the option.
 *
 * @return an integer indicating whether L3 math is being targeted (non-zero) 
 * or not (zero).
 */
LIBSBML_EXTERN
int
L3ParserSettings_getTargetL3 (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return 0;

  return (static_cast<int>(settings->getTargetL3()));
}


/**
 * Sets the units option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure on which to set the option.
 * @param flag an integer indicating whether numbers should be considered as 
 * a having units (non-zero) or not (zero).
 */
LIBSBML_EXTERN
void
L3ParserSettings_setParseUnits (L3ParserSettings_t * settings, int flag)
{
  if (settings == NULL)
    return;

  settings->setParseUnits(static_cast<bool>(flag));
}


/**
 * Retrieves the units option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure from which to get the option.
 *
 * @return an integer indicating whether numbers should be considered as 
 * a having units (non-zero) or not (zero).
 */
LIBSBML_EXTERN
int
L3ParserSettings_getParseUnits (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return 0;

  return (static_cast<int>(settings->getParseUnits()));
}


/**
 * Sets the avogadro csymbol option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure on which to set the option.
 * @param flag an integer indicating whether avogadro should be considered as 
 * a csymbol (non-zero) or not (zero).
 */
LIBSBML_EXTERN
void
L3ParserSettings_setAvogadroCsymbol (L3ParserSettings_t * settings, int flag)
{
  if (settings == NULL)
    return;

  settings->setAvogadroCsymbol(static_cast<bool>(flag));
}


/**
 * Retrieves the avogadro csymbol option associated with this L3ParserSettings_t object.  
 *
 * @param settings the L3ParserSettings_t structure from which to get the option.
 *
 * @return an integer indicating whether avogadro should be considered as 
 * a csymbol (non-zero) or not (zero).
 */
LIBSBML_EXTERN
int
L3ParserSettings_getAvogadroCsymbol (L3ParserSettings_t * settings)
{
  if (settings == NULL)
    return 0;

  return (static_cast<int>(settings->getAvogadroCsymbol()));
}


