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


L3ParserSettings::L3ParserSettings()
  : mModel (NULL)
  , mParselog(L3P_PARSE_LOG_AS_LOG10)
  , mCollapseminus(L3P_EXPAND_UNARY_MINUS)
  , mParseunits(L3P_PARSE_UNITS)
  , mAvoCsymbol(L3P_AVOGADRO_IS_CSYMBOL)
{
}

L3ParserSettings::L3ParserSettings(Model* model)
  : mModel (model)
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

