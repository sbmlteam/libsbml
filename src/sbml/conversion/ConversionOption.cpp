/**
 * @file    ConversionOption.cpp
 * @brief   Implementation of ConversionOption, the class encapsulating conversion options.
 * @author  Frank Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifdef __cplusplus

#include <sbml/conversion/ConversionOption.h>
#include <sbml/SBase.h>

#include <algorithm>
#include <string>
#include <sstream>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

ConversionOption::ConversionOption(string key, string value, 
    ConversionOptionType_t type, 
    string description) : 
    mKey(key)
  , mValue(value)
  , mType(type)
  , mDescription(description)
{
}

ConversionOption::ConversionOption(std::string key, const char* value, 
  std::string description) : 
    mKey(key)
  , mValue(value)
  , mType(CNV_TYPE_STRING)
  , mDescription(description)
{
}

ConversionOption::ConversionOption(std::string key, bool value, 
  std::string description) : 
    mKey(key)
  , mValue("")
  , mType(CNV_TYPE_STRING)
  , mDescription(description)
{
  setBoolValue(value);
}

ConversionOption::ConversionOption(std::string key, double value, 
  std::string description): 
    mKey(key)
  , mValue("")
  , mType(CNV_TYPE_STRING)
  , mDescription(description)
{
  setDoubleValue(value);
}

ConversionOption::ConversionOption(std::string key, float value, 
  std::string description) : 
    mKey(key)
  , mValue("")
  , mType(CNV_TYPE_STRING)
  , mDescription(description)
{
  setFloatValue(value);
}

ConversionOption::ConversionOption(std::string key, int value, 
  std::string description) : 
    mKey(key)
  , mValue("")
  , mType(CNV_TYPE_STRING)
  , mDescription(description)
{
      setIntValue(value);
}


ConversionOption::ConversionOption
  (const ConversionOption& orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mDescription = orig.mDescription;
    mKey = orig.mKey;
    mType = orig.mType;
    mValue = orig.mValue;
  }
}



ConversionOption& 
ConversionOption::operator=(const ConversionOption& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else
  {
    mDescription = rhs.mDescription;
    mKey = rhs.mKey;
    mType = rhs.mType;
    mValue = rhs.mValue;
  }
   return *this;
}

ConversionOption* 
ConversionOption::clone() const
{
  return new ConversionOption(*this);
}

ConversionOption::~ConversionOption() {}

string 
ConversionOption::getKey() const
{
  return mKey;
}

void 
ConversionOption::setKey(string key)
{
  mKey = key;
}

string 
ConversionOption::getValue() const
{
  return mValue;
}

void 
ConversionOption::setValue(string value)
{
  mValue = value;
}

string 
ConversionOption::getDescription() const
{
  return mDescription;
}

void 
ConversionOption::setDescription(string description)
{
  mDescription = description;
}

ConversionOptionType_t 
ConversionOption::getType() const
{
  return mType;
}

void 
ConversionOption::setType(ConversionOptionType_t type)
{
  mType = type;
}

bool 
ConversionOption::getBoolValue() const
{
  string value = mValue;
#ifdef __BORLANDC__
   std::transform(value.begin(), value.end(), value.begin(),  (int(*)(int))
std::tolower);
#else
   std::transform(value.begin(), value.end(), value.begin(), ::tolower);
#endif
  if (value == "true") return true;
  if (value == "false") return false;

  stringstream str; str << mValue;
  bool result; str >> result;
  return result;
}

void 
ConversionOption::setBoolValue(bool value)
{  
  mValue = (value ? "true" : "false");
  setType(CNV_TYPE_BOOL);
}

double 
ConversionOption::getDoubleValue() const
{
  stringstream str; str << mValue;
  double result; str >> result;
  return result;
}
 
void 
ConversionOption::setDoubleValue(double value)
{
  stringstream str; str << value;
  mValue = str.str();
  setType(CNV_TYPE_DOUBLE);
}

 
float 
ConversionOption::getFloatValue() const
{
  stringstream str; str << mValue;
  float result; str >> result;
  return result;
}

void 
ConversionOption::setFloatValue(float value)
{
  stringstream str; str << value;
  mValue = str.str();
  setType(CNV_TYPE_SINGLE);
}

 
int 
ConversionOption::getIntValue() const
{
  stringstream str; str << mValue;
  int result; str >> result;
  return result;
}
 
void 
ConversionOption::setIntValue(int value)
{
  stringstream str; str << value;
  mValue = str.str();
  setType(CNV_TYPE_INT);
}


/**
 * Creates a new ConversionOption_t with the given key.
 *
 * @param key the key for this option
 */
LIBSBML_EXTERN
ConversionOption_t*
ConversionOption_create(const char* key)
{
  return new ConversionOption(key);
}


/** 
 * Creates and returns a deep copy of the ConversionOption object.
 * 
 * @param co the conversion option to clone
 *
 * @return a (deep) copy of the ConversionOption object.
 */
LIBSBML_EXTERN
ConversionOption_t*
ConversionOption_clone(const ConversionOption_t* co)
{
  if (co == NULL) return NULL;
  return co->clone();
}

/**
 * Creates a new ConversionOption_t with given key and type.
 *
 * @param key the key for this option
 * @param type the type of this option
 */
LIBSBML_EXTERN
ConversionOption_t*
ConversionOption_createWithKeyAndType(const char* key, ConversionOptionType_t type)
{
  return new ConversionOption(key, type);
}

/**
 * Returns the key for the given option.
 * 
 * @param co the conversion option
 *
 * @return the key, as a string.
 */
LIBSBML_EXTERN
const char*
ConversionOption_getKey(const ConversionOption_t* co)
{
  if (co == NULL) return NULL;
  return co->getKey().c_str();
}

/**
 * Returns the description for the given option.
 * 
 * @param co the conversion option
 *
 * @return the description, as a string.
 */

LIBSBML_EXTERN
const char*
ConversionOption_getDescription(const ConversionOption_t* co)
{
  if (co == NULL) return NULL;
  return co->getDescription().c_str();
}

/**
 * Returns the value for the given option.
 * 
 * @param co the conversion option
 *
 * @return the value, as a string.
 */
LIBSBML_EXTERN
const char*
ConversionOption_getValue(const ConversionOption_t* co)
{
  if (co == NULL) return NULL;
  return co->getValue().c_str();
}

/**
 * Returns the value (as boolean) for the given option.
 * 
 * @param co the conversion option
 *
 * @return the value, as a boolean.
 */
LIBSBML_EXTERN
int
ConversionOption_getBoolValue(const ConversionOption_t* co)
{
  if (co == NULL) return 0;
  return (int) co->getBoolValue();
}

/**
 * Returns the value (as integer) for the given option.
 * 
 * @param co the conversion option
 *
 * @return the value, as a integer.
 */LIBSBML_EXTERN
int
ConversionOption_getIntValue(const ConversionOption_t* co)
{
  if (co == NULL) return 0;
  return (int) co->getIntValue();
}

/**
 * Returns the value (as float) for the given option.
 * 
 * @param co the conversion option
 *
 * @return the value, as a float.
 */
LIBSBML_EXTERN
float
ConversionOption_getFloatValue(const ConversionOption_t* co)
{
  if (co == NULL) return std::numeric_limits<float>::quiet_NaN();
  return co->getFloatValue();
}

/**
 * Returns the value (as double) for the given option.
 * 
 * @param co the conversion option
 *
 * @return the value, as a double.
 */
LIBSBML_EXTERN
double
ConversionOption_getDoubleValue(const ConversionOption_t* co)
{
  if (co == NULL) return std::numeric_limits<double>::quiet_NaN();
  return co->getDoubleValue();
}

/**
 * Returns the type for the given option.
 * 
 * @param co the conversion option
 *
 * @return the type
 */
LIBSBML_EXTERN
ConversionOptionType_t
ConversionOption_getType(const ConversionOption_t* co)
{
  if (co == NULL) return CNV_TYPE_STRING;
  return co->getType();
}


/**
 * Sets the key for the option.
 * 
 * @param co the conversion option
 * @param key a string representing the key to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setKey(ConversionOption_t* co, const char* key)
{
  if (co == NULL) return;
  co->setKey(key);
}

/**
 * Sets the description for the option.
 * 
 * @param co the conversion option
 * @param description a string representing the description to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setDescription(ConversionOption_t* co, const char* description)
{
  if (co == NULL) return;
  co->setDescription(description);
    
}

/**
 * Sets the value for the option.
 * 
 * @param co the conversion option
 * @param value a string representing the value to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setValue(ConversionOption_t* co, const char* value)
{
  if (co == NULL) return;
  co->setValue(value);
}

/**
 * Sets the value for the option.
 * 
 * @param co the conversion option
 * @param value a bool representing the value to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setBoolValue(ConversionOption_t* co, int value)
{
  if (co == NULL) return;
  co->setBoolValue(value != 0);
}

/**
 * Sets the value for the option.
 * 
 * @param co the conversion option
 * @param value an integer representing the value to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setIntValue(ConversionOption_t* co, int value)
{
  if (co == NULL) return;
  co->setIntValue(value);
}

/**
 * Sets the value for the option.
 * 
 * @param co the conversion option
 * @param value a float representing the value to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setFloatValue(ConversionOption_t* co, float value)
{
  if (co == NULL) return;
  co->setFloatValue(value);
}

/**
 * Sets the value for the option.
 * 
 * @param co the conversion option
 * @param value a double representing the value to set.
 */
LIBSBML_EXTERN
void
ConversionOption_setDoubleValue(ConversionOption_t* co, double value)
{
  if (co == NULL) return;
  co ->setDoubleValue(value);
}

/**
 * Sets the type for the option.
 * 
 * @param co the conversion option
 * @param type the type for this option.
 */
LIBSBML_EXTERN
void
ConversionOption_setType(ConversionOption_t* co, ConversionOptionType_t type)
{
  if (co == NULL) return;
  co->setType(type);
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
