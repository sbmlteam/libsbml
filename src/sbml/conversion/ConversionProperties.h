/**
 * @file    ConversionProperties.h
 * @brief   Definition of ConversionProperties, the class encapsulating conversion configuration.
 * @author  Frank Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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

#ifndef ConversionProperties_h
#define ConversionProperties_h



#include <sbml/common/extern.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/ConversionOption.h>


#ifdef __cplusplus

#include <map>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 *  @brief class encapsulating conversion properties
 */
class ConversionProperties
{
public:
  /** 
   * Constructor that initializes the conversion properties
   * with a specific SBML target namespace
   * 
   * @param targetNS the target namespace to convert to
   */
  ConversionProperties(SBMLNamespaces* targetNS=NULL);

  /** 
   * Copy constructor
   */
  ConversionProperties(const ConversionProperties&);
  
  /**
   * Assignment operator for conversion properties.
   */
  ConversionProperties& operator=(const ConversionProperties&);

  /** 
   * clone method
   */
  virtual ConversionProperties* clone() const; 

  /**
   * Destructor
   */
  virtual ~ConversionProperties();

  /**
   * @return the current target namespace
   */ 
  virtual SBMLNamespaces * getTargetNamespaces() const;
  /**
   * @return boolean indicating whether the target namespace has been set.
   */
  virtual bool hasTargetNamespaces() const;
  /** 
   * Sets the target namespace
   * 
   * @param targetNS the target namespace
   */
  virtual void setTargetNamespaces(SBMLNamespaces *targetNS);

  /**
   * @param key the key for the option
   * @return the description of the option with the given key
   */
  virtual std::string getDescription(std::string key) const;
  /**
   * @param key the key for the option
   * @return the type of the option with the given key
   */
  virtual ConversionOptionType_t  getType(std::string key) const;
  /**
   * @param key the key for the option
   * @return the option with the given key
   */
  virtual ConversionOption* getOption(std::string key) const;  
  /**
   * Adds a copy of the given option to the properties 
   * 
   * @param option the option to add
   */
  virtual void addOption(const ConversionOption& option);
  /**
   * Adds a new option with the given parameters
   * 
   * @param key the key for the new option
   * @param value (optional) the value of that option
   * @param type (optional) the type of the option
   * @param description (optional) the description for the option
   */
  virtual void addOption(std::string key, std::string value="", 
    ConversionOptionType_t type=CNV_TYPE_STRING, 
    std::string description="");
  /**
   * Adds a new option with the given parameters
   * 
   * @param key the key for the new option
   * @param value the string value of that option
   * @param description (optional) the description for the option
   */
  virtual void addOption(std::string key, const char* value, 
    std::string description="");
  /**
   * Adds a new option with the given parameters
   * 
   * @param key the key for the new option
   * @param value the boolean value of that option
   * @param description (optional) the description for the option
   */
  virtual void addOption(std::string key, bool value, 
    std::string description="");
  /**
   * Adds a new option with the given parameters
   * 
   * @param key the key for the new option
   * @param value the double value of that option
   * @param description (optional) the description for the option
   */
  virtual void addOption(std::string key, double value, 
    std::string description="");
  /**
   * Adds a new option with the given parameters
   * 
   * @param key the key for the new option
   * @param value the float value of that option
   * @param description (optional) the description for the option
   */
  virtual void addOption(std::string key, float value, 
    std::string description="");
  /**
   * Adds a new option with the given parameters
   * 
   * @param key the key for the new option
   * @param value the integer value of that option
   * @param description (optional) the description for the option
   */
  virtual void addOption(std::string key, int value, 
    std::string description="");
  /**
   * Removes the option with the given key from the properties
   * 
   * @param key the key for the new option to remove
   * @return the removed option
   */
  virtual ConversionOption* removeOption(std::string key);
  /** 
   * Tests whether the properties contain an option with the given key
   * @param key the key of the option to find
   * @return <c>true</c> if the option could be found, <c>false</c> otherwise
   */
  virtual bool hasOption(std::string key) const;  
  
  /**
   * @param key the key for the option
   * @return the string value of the option with the given key
   */
  virtual std::string getValue(std::string key) const;  
  /**
   * Sets the value of the option with given key
   * 
   * @param key the key for the option
   * @param value the new value
   */
  virtual void setValue(std::string key, std::string value);  
  
  /**
   * @param key the key for the option
   * @return the boolean value of the option with the given key
   */
  virtual bool getBoolValue(std::string key) const;
  /**
   * Sets the value of the option with given key
   * 
   * @param key the key for the option
   * @param value the new boolean value
   */
  virtual void setBoolValue(std::string key, bool value);
  
  /**
   * @param key the key for the option
   * @return the double value of the option with the given key
   */
  virtual double getDoubleValue(std::string key) const;
  /**
   * Sets the value of the option with given key
   * 
   * @param key the key for the option
   * @param value the new double value
   */
  virtual void setDoubleValue(std::string key, double value);
  
  /**
   * @param key the key for the option
   * @return the float value of the option with the given key
   */
  virtual float getFloatValue(std::string key) const;
  /**
   * Sets the value of the option with given key
   * 
   * @param key the key for the option
   * @param value the new float value
   */
  virtual void setFloatValue(std::string key, float value);
  
  /**
   * @param key the key for the option
   * @return the int value of the option with the given key
   */
  virtual int getIntValue(std::string key) const;
  /**
   * Sets the value of the option with given key
   * 
   * @param key the key for the option
   * @param value the new integer value
   */
  virtual void setIntValue(std::string key, int value);


protected:
  SBMLNamespaces *mTargetNamespaces;
  std::map<std::string, ConversionOption*> mOptions;
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif /* !ConversionProperties_h */

