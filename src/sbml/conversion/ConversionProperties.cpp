/**
* @file    ConversionProperties.cpp
* @brief   Implemenentation of ConversionProperties, the class encapsulating conversion configuration.
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

#include <sbml/conversion/ConversionProperties.h>
#include <sbml/conversion/ConversionOption.h>
#include <sbml/util/util.h>
#include <sbml/SBase.h>

#include <algorithm>
#include <string>
#include <sstream>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN



ConversionProperties::ConversionProperties(SBMLNamespaces* targetNS) : mTargetNamespaces(NULL)
{
  if (targetNS != NULL) mTargetNamespaces = targetNS->clone();
}

ConversionProperties::ConversionProperties(const ConversionProperties& orig)
{
  
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {    
    if (orig.mTargetNamespaces != NULL)
      mTargetNamespaces = orig.mTargetNamespaces->clone();
    else 
      mTargetNamespaces = NULL;

    map<string, ConversionOption*>::const_iterator it;
    for (it = orig.mOptions.begin(); it != orig.mOptions.end(); it++)
    {
      mOptions.insert(pair<string, ConversionOption*>
        ( it->second->getKey(), it->second->clone()));
    }
  }
}

ConversionProperties& 
ConversionProperties::operator=(const ConversionProperties& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  if (&rhs == this)
  {
    return *this;
  }
    // clear 

    if (mTargetNamespaces != NULL)
    {
      delete mTargetNamespaces;
      mTargetNamespaces = NULL;
    }
    
    map<string, ConversionOption*>::iterator it1;
    for (it1 = mOptions.begin(); it1 != mOptions.end(); ++it1)
    {
      if (it1->second != NULL) 
      { 
        delete it1->second;
        it1->second=NULL;
      }
    }
    mOptions.clear();

    // assign

    if (rhs.mTargetNamespaces != NULL)
      mTargetNamespaces = rhs.mTargetNamespaces->clone();
    else 
      mTargetNamespaces = NULL;

    map<string, ConversionOption*>::const_iterator it;
    for (it = rhs.mOptions.begin(); it != rhs.mOptions.end(); ++it)
    {
      mOptions.insert(pair<string, ConversionOption*>
        ( it->second->getKey(), it->second->clone()));
    }

    return *this;
}

ConversionProperties* 
ConversionProperties::clone() const
{
  return new ConversionProperties(*this);
}

ConversionProperties::~ConversionProperties()
{
  if (mTargetNamespaces != NULL)
  {
    delete mTargetNamespaces;
    mTargetNamespaces = NULL;
  }

  map<string, ConversionOption*>::iterator it;
  for (it = mOptions.begin(); it != mOptions.end(); it++)
  {
    if (it->second != NULL) 
    { 
      delete it->second;
      it->second=NULL;
    }
  }

}

SBMLNamespaces * 
ConversionProperties::getTargetNamespaces() const
{
  return mTargetNamespaces;
}

bool 
ConversionProperties::hasTargetNamespaces() const
{
  return mTargetNamespaces != NULL;
}


void 
ConversionProperties::setTargetNamespaces(SBMLNamespaces *targetNS)
{
  if (mTargetNamespaces != NULL) 
  {
      delete mTargetNamespaces;
      mTargetNamespaces = NULL;
  }
  if (targetNS == NULL) return;
  
  mTargetNamespaces = targetNS->clone();
}

std::string 
ConversionProperties::getDescription(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getDescription();

  return "";
}

ConversionOptionType_t 
ConversionProperties::getType(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getType();

  return CNV_TYPE_STRING;
}


ConversionOption* 
ConversionProperties::getOption(std::string key) const
{

  map<string, ConversionOption*>::const_iterator it;
  for (it = mOptions.begin(); it != mOptions.end(); it++)
  {
    if (it->second != NULL && it->second->getKey() == key)
      return it->second;
  }
  return NULL;
}

void 
ConversionProperties::addOption(const ConversionOption &option)
{
  if (&option == NULL) return;
  mOptions.insert(pair<string, ConversionOption*>( option.getKey(), option.clone()));
}

void 
ConversionProperties::addOption(std::string key, std::string value, 
    ConversionOptionType_t type, 
    std::string description)
{
  mOptions.insert(pair<string, ConversionOption*>( key, new ConversionOption(key, value, type, description) ));
}
void 
ConversionProperties::addOption(std::string key, const char* value, 
    std::string description)
{
  mOptions.insert(pair<string, ConversionOption*>( key, new ConversionOption(key, value, description) ));
}
void 
ConversionProperties::addOption(std::string key, bool value, 
    std::string description)
{
  mOptions.insert(pair<string, ConversionOption*>( key, new ConversionOption(key, value, description) ));
}
void 
ConversionProperties::addOption(std::string key, double value, 
    std::string description)
{
  mOptions.insert(pair<string, ConversionOption*>( key, new ConversionOption(key, value, description) ));
}
void 
ConversionProperties::addOption(std::string key, float value, 
    std::string description)
{
  mOptions.insert(pair<string, ConversionOption*>( key, new ConversionOption(key, value, description) ));
}
void 
ConversionProperties::addOption(std::string key, int value, 
    std::string description)
{
  mOptions.insert(pair<string, ConversionOption*>( key, new ConversionOption(key, value, description) ));
}

ConversionOption* 
ConversionProperties::removeOption(std::string key)
{
  ConversionOption* result = getOption(key);
  if (result != NULL)
    mOptions.erase(key);
  return result;
}

bool 
ConversionProperties::hasOption(std::string key) const
{
  return (getOption(key) != NULL);
}

std::string 
ConversionProperties::getValue(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getValue();
  return "";
}

void 
ConversionProperties::setValue(std::string key, std::string value)
{
  ConversionOption *option = getOption(key);
  if (option != NULL) option->setValue(value);
}


bool 
ConversionProperties::getBoolValue(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getBoolValue();
  return false;
}

void 
ConversionProperties::setBoolValue(std::string key, bool value)
{
  ConversionOption *option = getOption(key);
  if (option != NULL) option->setBoolValue(value);
}

double 
ConversionProperties::getDoubleValue(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getDoubleValue();
  return std::numeric_limits<double>::quiet_NaN();
}

void 
ConversionProperties::setDoubleValue(std::string key, double value)
{
  ConversionOption *option = getOption(key);
  if (option != NULL) option->setDoubleValue(value);
}

float 
ConversionProperties::getFloatValue(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getFloatValue();
  return std::numeric_limits<float>::quiet_NaN();
}

void 
ConversionProperties::setFloatValue(std::string key, float value)
{
  ConversionOption *option = getOption(key);
  if (option != NULL) option->setFloatValue(value);

}

int 
ConversionProperties::getIntValue(std::string key) const
{
  ConversionOption *option = getOption(key);
  if (option != NULL) return option->getIntValue();
  return -1;
}

void 
ConversionProperties::setIntValue(std::string key, int value)
{
  ConversionOption *option = getOption(key);
  if (option != NULL) option->setIntValue(value);

}



/** 
 * creates a new conversion properties structure (without namespace)
 */
LIBSBML_EXTERN
ConversionProperties_t*
ConversionProperties_create()
{
  return new ConversionProperties();
}

/** 
 * creates a new conversion properties structure
 * with a specific SBML target namespace.
 * 
 * @param sbmlns the target namespace to convert to
 */
LIBSBML_EXTERN
ConversionProperties_t*
ConversionProperties_createWithSBMLNamespace(SBMLNamespaces_t* sbmlns)
{
  return new ConversionProperties(sbmlns);
}

/** 
 * Creates and returns a deep copy of the given ConversionProperties object.
 * 
 * @param cp the conversion properties to clone
 * 
 * @return a (deep) copy of this ConversionProperties object.
 */
LIBSBML_EXTERN
ConversionProperties_t*
ConversionProperties_clone(const ConversionProperties_t* cp)
{
  return new ConversionProperties();
}

/**
 * Returns the value as boolean for a given option in the properties
 * object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the boolean value of the option with the given key to be freed by the caller, or NULL.
 */
LIBSBML_EXTERN
int
ConversionProperties_getBoolValue(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return 0;
  return cp->getBoolValue(key);
}

/**
 * Returns the value as integer for a given option in the properties
 * object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the integer value of the option with the given key to be freed by the caller, or -1.
 */
LIBSBML_EXTERN
int
ConversionProperties_getIntValue(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return -1;
  return cp->getIntValue(key);
}

/**
 * Returns the description string for a given option in the properties
 * object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the description text of the option with the given key to be freed by the caller.
 */
LIBSBML_EXTERN
char*
ConversionProperties_getDescription(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return NULL;
  return strdup(cp->getDescription(key).c_str());
}

/**
 * Returns the value as double for a given option in the properties
 * object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the double value of the option with the given key to be freed by the caller, or NaN.
 */
LIBSBML_EXTERN
double
ConversionProperties_getDoubleValue(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return std::numeric_limits<double>::quiet_NaN();
  return cp->getDoubleValue(key);
}

/**
 * Returns the value as float for a given option in the properties
 * object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the float value of the option with the given key to be freed by the caller, or NaN.
 */
LIBSBML_EXTERN
float
ConversionProperties_getFloatValue(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return std::numeric_limits<float>::quiet_NaN();
  return cp->getFloatValue(key);
}

/**
 * Returns the value string for a given option in the properties
 * object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the string value of the option with the given key to be freed by the caller, or NULL.
 */
LIBSBML_EXTERN
char*
ConversionProperties_getValue(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return NULL;
  return strdup(cp->getValue(key).c_str());
}

/**
 * Returns the ConversionOption object for a given key.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the option with the given key, or NULL.
 */
LIBSBML_EXTERN
const ConversionOption_t*
ConversionProperties_getOption(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return NULL;
  return cp->getOption(key);
}

/**
 * Returns the type of a given option in the properties object.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return the type of the option with the given key.
 */
LIBSBML_EXTERN
ConversionOptionType_t
ConversionProperties_getType(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return CNV_TYPE_STRING;
  return cp->getType(key);
}

/**
 * Returns the current target SBML namespace of the conversion properties.
 *
 * @param cp the conversion properties
 *
 * @return the SBMLNamepaces object expressing the target namespace, or NULL.
 */ 
LIBSBML_EXTERN
const SBMLNamespaces_t*
ConversionProperties_getTargetNamespaces(const ConversionProperties_t* cp)
{
  if (cp == NULL) return NULL;
  return cp->getTargetNamespaces();
}

/**
 * Checks whether the given properties structure has an option for the given key
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * 
 * @return @c 1 if the option exists, @c 0 otherwise.
 */
LIBSBML_EXTERN
int
ConversionProperties_hasOption(const ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL) return 0;
  return (int)cp->hasOption(key);
}

/**
 * Tests whether the given conversion properties has a target namespace set
 *
 * @param cp the conversion properties
 *
 * @return @c 1 if the target namespace has been set, @c 0
 * otherwise.
 */ 
LIBSBML_EXTERN
int
ConversionProperties_hasTargetNamespaces(const ConversionProperties_t* cp)
{
  if (cp == NULL) return 0;
  return (int)cp->hasTargetNamespaces();
}

/** 
 * Sets the target namespace.
 * 
 * @param cp the conversion properties
 * @param sbmlns the target namespace to use.
 */
LIBSBML_EXTERN
void
ConversionProperties_setTargetNamespaces(ConversionProperties_t* cp, SBMLNamespaces_t* sbmlns)
{
  if (cp == NULL) return;
  cp->setTargetNamespaces(sbmlns);
}

/**
 * Sets the value of the option with given key to the given boolean value.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * @param value the new value for the option.
 */
LIBSBML_EXTERN
void
ConversionProperties_setBoolValue(ConversionProperties_t* cp, const char* key, int value)
{
  if (cp == NULL) return;
  cp->setBoolValue(key, (bool)value);
}

/**
 * Sets the value of the option with given key to the given int value.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * @param value the new value for the option.
 */
LIBSBML_EXTERN
void
ConversionProperties_setIntValue(ConversionProperties_t* cp, const char* key, int value)
{
  if (cp == NULL) return;
  cp->setIntValue(key, value);
}

/**
 * Sets the value of the option with given key to the given double value.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * @param value the new value for the option.
 */
LIBSBML_EXTERN
void
ConversionProperties_setDoubleValue(ConversionProperties_t* cp, const char* key, double value)
{
  if (cp == NULL) return;
  cp->setDoubleValue(key, value);
}

/**
 * Sets the value of the option with given key to the given float value.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * @param value the new value for the option.
 */
LIBSBML_EXTERN
void
ConversionProperties_setFloatValue(ConversionProperties_t* cp, const char* key, float value)
{
  if (cp == NULL) return;
  cp->setFloatValue(key, value);
}

/**
 * Sets the value of the option with given key to the given value.
 * 
 * @param cp the conversion properties
 * @param key the key for the option.
 * @param value the new value for the option.
 */
LIBSBML_EXTERN
void
ConversionProperties_setValue(ConversionProperties_t* cp, const char* key, const char* value)
{
  if (cp == NULL) return;
  cp->setValue(key, value);
}

/**
 * Adds a copy of the given ConversionOption structure to the properties.
 * 
 * @param cp the conversion properties
 * @param option the option to add
 */
LIBSBML_EXTERN
void
ConversionProperties_addOption(ConversionProperties_t* cp, const ConversionOption_t* option)
{
  if (cp == NULL || option == NULL) return;
  cp->addOption(*option);
}

/**
 * Removes the ConversionOption with the given key from the properties.
 * 
 * @param cp the conversion properties
 * @param key the key for the option to remove
 */
LIBSBML_EXTERN 
void 
ConversionProperties_removeOption(ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL || key == NULL) return;
  cp->removeOption(key);
}
  
/**
 * Adds a new ConversionOption structure with the given key to the properties.
 * 
 * @param cp the conversion properties
 * @param key the key for the new option
 */
LIBSBML_EXTERN
void
ConversionProperties_addOptionWithKey(ConversionProperties_t* cp, const char* key)
{
  if (cp == NULL || key == NULL) return;
  cp->addOption(key);
}

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
