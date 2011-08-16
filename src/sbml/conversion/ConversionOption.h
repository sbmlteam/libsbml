/**
 * @file    ConversionOption.h
 * @brief   Definition of ConversionOption, the class encapsulating conversion options.
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

#ifndef ConversionOption_h
#define ConversionOption_h


#include <sbml/common/extern.h>


LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * @enum  ConversionOptionType_t
 * @brief ConversionOptionType_t is the enumeration of possible option types.
 */
typedef enum
{
    CNV_TYPE_BOOL
  , CNV_TYPE_DOUBLE
  , CNV_TYPE_INT
  , CNV_TYPE_SINGLE
  , CNV_TYPE_STRING
} ConversionOptionType_t;

LIBSBML_CPP_NAMESPACE_END

#ifdef __cplusplus
#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 *  @brief class encapsulating conversion options
 */
class LIBSBML_EXTERN ConversionOption
{
public:
  /**
   * @brief creates a new ConversionOption
   * 
   * @param key the key for this option
   * @param value an optional value for this option
   * @param type the type of this option
   * @param description the description for this option
   */
  ConversionOption(std::string key, std::string value="", 
    ConversionOptionType_t type=CNV_TYPE_STRING, 
    std::string description="");
  
  /**
   * Constructor creating a string option
   * 
   * @param key the key for this option
   * @param value the value for this option
   * @param description an optional description
   * 
   */
  ConversionOption(std::string key, const char* value, 
    std::string description="");
  /**
   * Constructor creating a bool option
   * 
   * @param key the key for this option
   * @param value the value for this option
   * @param description an optional description
   * 
   */
  ConversionOption(std::string key, bool value, 
    std::string description="");

  /**
   * Constructor creating a double option
   * 
   * @param key the key for this option
   * @param value the value for this option
   * @param description an optional description
   * 
   */
  ConversionOption(std::string key, double value, 
    std::string description="");

  /**
   * Constructor creating a float option
   * 
   * @param key the key for this option
   * @param value the value for this option
   * @param description an optional description
   * 
   */
  ConversionOption(std::string key, float value, 
    std::string description="");

  /**
   * Constructor creating a integer option
   * 
   * @param key the key for this option
   * @param value the value for this option
   * @param description an optional description
   * 
   */
  ConversionOption(std::string key, int value, 
    std::string description="");

   /**
    * Copy constructor.
    */
   ConversionOption(const ConversionOption& option);

   /**
    * Assignment operator for conversion option.
    */
   ConversionOption& operator=(const ConversionOption& option);

  /**
   * Destroy this object.
   */ 
  virtual ~ConversionOption();

  /** 
   * Clone method
   * 
   * @return returns a clone of this option
   */
  virtual ConversionOption* clone() const;

  /**
   * @return the key for this option
   */
  virtual std::string getKey() const; 
  /**
   * Set the key for this option
   * 
   * @param key the key to set
   *
   */
  virtual void setKey(std::string key);

  /**
   * @return the value of this option as string
   */
  virtual std::string getValue() const;
  /**
   * Set the value for this option
   * 
   * @param value the value to set
   *
   */
  virtual void setValue(std::string value);

  /**
   * @return the description for this option
   */
  virtual std::string getDescription() const;
  /**
   * Set the description for this option
   * 
   * @param description the description to set
   *
   */
  virtual void setDescription(std::string description);

  /**
   * @return the type of this option
   */
  virtual ConversionOptionType_t getType() const;
  /**
   * Set the type of this option
   * 
   * @param type the type to set
   *
   */
  virtual void setType(ConversionOptionType_t type);

  /**
   * Convenience method returning the value of this option as boolean
   * 
   * @return the value of this option as bool
   */   
  virtual bool getBoolValue() const;
  /** 
   * Convenience method setting the value of this option to the given bool. 
   * This will also set the type to boolean.
   * 
   * @param value the value to set
   */
  virtual void setBoolValue(bool value);

  /**
   * Convenience method returning the value of this option as double
   * 
   * @return the value of this option as double
   */   
  virtual double getDoubleValue() const;
  /** 
   * Convenience method setting the value of this option to the given double. 
   * This will also set the type to double.
   * 
   * @param value the value to set
   */
  virtual void setDoubleValue(double value);

  /**
   * Convenience method returning the value of this option as single
   * 
   * @return the value of this option as float
   */   
  virtual float getFloatValue() const;
  /** 
   * Convenience method setting the value of this option to the given single. 
   * This will also set the type to single.
   * 
   * @param value the value to set
   */
  virtual void setFloatValue(float value);

  /**
   * Convenience method returning the value of this option as integer
   * 
   * @return the value of this option as int
   */   
  virtual int getIntValue() const;
  /** 
   * Convenience method setting the value of this option to the given integer. 
   * This will also set the type to integer.
   * 
   * @param value the value to set
   */
  virtual void setIntValue(int value);

protected: 
  std::string mKey;
  std::string mValue;
  std::string mDescription;
  ConversionOptionType_t mType;

};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif /* !ConversionOption */

