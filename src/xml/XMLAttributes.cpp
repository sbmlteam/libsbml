/**
 * @file    XMLAttributes.cpp
 * @brief   XMLAttributes are a list of name/value pairs for XMLElements
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <cerrno>
#include <clocale>
#include <cstdlib>
#include <limits>
#include <sstream>

#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLAttributes.h>
/** @cond doxygen-libsbml-internal */
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/util/util.h>
/** @endcond doxygen-libsbml-internal */

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * @return s with whitespace removed from the beginning and end.
 */
static const string
trim (const string& s)
{
  static const string whitespace(" \t\r\n");

  string::size_type begin = s.find_first_not_of(whitespace);
  string::size_type end   = s.find_last_not_of (whitespace);

  return (begin == string::npos) ? "" : s.substr(begin, end - begin + 1);
}


/*
 * Creates a new empty XMLAttributes set.
 */
XMLAttributes::XMLAttributes () : mLog( 0 )
{
}


/*
 * Destroys this XMLAttributes set.
 */
XMLAttributes::~XMLAttributes ()
{
}

/*
 * Copy constructor; creates a copy of this XMLAttributes set.
 */
XMLAttributes::XMLAttributes(const XMLAttributes& orig)
{
  this->mNames.assign( orig.mNames.begin(), orig.mNames.end() ); 
  this->mValues.assign( orig.mValues.begin(), orig.mValues.end() ); 
  this->mElementName = orig.mElementName;
  this->mLog = orig.mLog;
}


/*
 * Assignment operator for XMLAttributes.
 */
XMLAttributes& 
XMLAttributes::operator=(const XMLAttributes& orig)
{
  this->mNames.assign( orig.mNames.begin(), orig.mNames.end() ); 
  this->mValues.assign( orig.mValues.begin(), orig.mValues.end() ); 
  this->mElementName = orig.mElementName;
  this->mLog = orig.mLog;
  return *this;
}

/*
 * Creates and returns a deep copy of this XMLAttributes set.
 * 
 * @return a (deep) copy of this XMLAttributes set.
 */
XMLAttributes* 
XMLAttributes::clone () const
{
  return new XMLAttributes(*this);
}



/*
 * Adds a name/value pair to this XMLAttributes set.  If name with the same
 * namespace URI already exists in this attribute set, its value will be
 * replaced.
 */
void
XMLAttributes::add (const std::string& name,
		    const std::string& value,
		    const std::string& namespaceURI,
		    const std::string& prefix)
{
  int index = getIndex(name);

  // since in the old version of the method the XMLTriple was initialized
  // with empty strings for the prefix and the uri, I assume that only
  // attributes that are not from the default namespace should have a set
  // prefix and uri.

  if (index == -1 || getURI(index)!=namespaceURI )
  {
    mNames .push_back( XMLTriple(name, namespaceURI, prefix) );
    mValues.push_back( value );
  }
  else
  {
    mValues[index] = value;
  }
}


/*
 * Adds a name/value pair to this XMLAttributes set.  This
 * is really the add function but an attribute with same name wont 
 * be overwritten - this is for annotations
 */
void
XMLAttributes::addResource (const std::string& name, const std::string& value)
{
  mNames .push_back( XMLTriple(name, "", "") );
  mValues.push_back( value );
}


/*
 * Removes a name/value pair from this XMLAttributes set.  
 * This is for annotations
 */
void
XMLAttributes::removeResource (int n)
{
  vector<XMLTriple>::iterator names_iter;
  vector<std::string>::iterator values_iter;

  names_iter = mNames.begin();
  values_iter = mValues.begin();

  for (int p = 0; p < n; p++)
  {
    names_iter++;
    values_iter++;
  }
  mNames.erase(names_iter);
  mValues.erase(values_iter);
}


/*
 * Lookup the index of an attribute by name.
 *
 * @return the index of the given attribute, or -1 if not present.
 */
int
XMLAttributes::getIndex (const std::string& name) const
{
  for (int index = 0; index < getLength(); ++index)
  {
    if (getName(index) == name) return index;
  }
  
  return -1;
}


/*
 * @return the number of attributes in this list.
 */
int
XMLAttributes::getLength () const
{
  return mNames.size();
}


/*
 * @return the name of an attribute in this list (by position).  If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 */
string
XMLAttributes::getName (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNames[index].getName();
}


/*
 * @return the namespace prefix of an attribute in this list (by
 * position).  If index is out of range, an empty string will be
 * returned.  Use getIndex() > 0 to test for attribute existence.
 */
string
XMLAttributes::getPrefix (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNames[index].getPrefix();
}


/*
 * @return the namespace URI of an attribute in this list (by position).
 * If index is out of range, an empty string will be returned.  Use
 * getIndex() > 0 to test for attribute existence.
 */
string
XMLAttributes::getURI (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNames[index].getURI();
}


/*
 * @return the value of an attribute in the list (by position).  If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 */
string
XMLAttributes::getValue (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mValues[index];
}


/*
 * Lookup an attribute's value by name.
 *
 * @return The attribute value as a string.  If an attribute with the
 * given name does not exist, an empty string will be returned.  Use
 * getIndex() > 0 to test for attribute existence.
 */
string
XMLAttributes::getValue (const std::string name) const
{
  return getValue( getIndex(name) );
}


/*
 * @return true if this XMLAttributes set is empty, false otherwise.
 */
bool
XMLAttributes::isEmpty () const
{
  return (getLength() == 0);
}


/*
 * Reads the value for the attribute name into value.  If name was not
 * found or value could not be interpreted as a boolean, value is not modified.
 *
 * According to the W3C XML Schema, valid boolean values are: "true",
 * "false", "1", and "0" (case-insensitive).  For more information, see:
 * http://www.w3.org/TR/xmlschema-2/#boolean
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const std::string&   name
                         , bool&                value
                         , XMLErrorLog*         log
                         , bool                 required ) const
{
  bool assigned = false;
  bool missing  = true;
  int  index    = getIndex(name);

  if ( index != -1 )
  {
    const string& trimmed = trim( getValue(index) );
    if ( !trimmed.empty() )
    {
      missing = false;

      if (trimmed == "0" || trimmed == "false")
      {
        value    = false;
        assigned = true;
      }
      else if (trimmed == "1" || trimmed == "true")
      {
        value    = true;
        assigned = true;
      }
    }
  }

  if ( !log ) log = mLog;

  if ( log && !assigned )
  {
    if ( !missing ) attributeTypeError(name, Boolean, log);
    else if ( required ) attributeRequiredError (name, log);
  }

  return assigned;
}


/*
 * Reads the value for the attribute name into value.  If name was not
 * found or value could not be interpreted as a double, value is not
 * modified.
 *
 * According to the W3C XML Schema, valid doubles are the same as valid
 * doubles for C and the special values "INF", "-INF", and "NaN"
 * (case-sensitive).  For more information, see:
 * http://www.w3.org/TR/xmlschema-2/#double
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const std::string&   name
                         , double&         value
                         , XMLErrorLog*    log
                         , bool            required ) const
{
  bool assigned = false;
  bool missing  = true;
  int  index    = getIndex(name);

  if ( index != -1 )
  {
    const string& trimmed = trim( getValue(index) );
    if ( !trimmed.empty() )
    {
      if (trimmed == "-INF")
      {
        value    = - numeric_limits<double>::infinity();
        assigned = true;
      }
      else if (trimmed == "INF")
      {
        value    = numeric_limits<double>::infinity();
        assigned = true;
      }
      else if (trimmed == "NaN")
      {
        value    = numeric_limits<double>::quiet_NaN();
        assigned = true;
      }
      else
      {
        // Ensure C locale
        char*  ptr    =  setlocale(LC_ALL, NULL);
        string locale = (ptr) ? ptr : "";
        setlocale(LC_ALL, "C");

        errno               = 0;
        char*        endptr = 0;
        const char*  nptr   = trimmed.c_str();
        double       result = strtod(nptr, &endptr);
        unsigned int length = endptr - nptr;

        // Restore previous locale
        setlocale(LC_ALL, locale.empty() ? 0 : locale.c_str());

        if ((length == trimmed.size()) && (errno != ERANGE))
        {
          value    = result;
          assigned = true;
        }
      }
    }
  }

  if ( !log ) log = mLog;

  if ( log && !assigned )
  {
    if ( !missing ) attributeTypeError(name, Double, log);
    else if ( required ) attributeRequiredError (name, log);
  }

  return assigned;
}


/*
 * Reads the value for the attribute name into value.  If name was not
 * found or value could not be interpreted as an long, value is not modified.
 *
 * According to the W3C XML Schema valid integers include zero, *all*
 * positive and *all* negative whole numbers.  For practical purposes, we
 * limit values to what can be stored in a long.  For more information,
 * see: http://www.w3.org/TR/xmlschema-2/#integer
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const std::string&   name
                         , long&           value
                         , XMLErrorLog*    log
                         , bool            required ) const
{
  bool assigned = false;
  bool missing  = true;
  int  index    = getIndex(name);

  if ( index != -1 )
  {
    const string& trimmed = trim( getValue(index) );
    if ( !trimmed.empty() )
    {
      missing = false;

      errno               = 0;
      char*        endptr = 0;
      const char*  nptr   = trimmed.c_str();
      long         result = strtol(nptr, &endptr, 10);
      unsigned int length = endptr - nptr;

      if ((length == trimmed.size()) && (errno != ERANGE))
      {
        value    = result;
        assigned = true;
      }
    }
  }

  if ( !log ) log = mLog;

  if ( log && !assigned )
  {
    if ( !missing ) attributeTypeError(name, Integer, log);
    else if ( required ) attributeRequiredError (name, log);
  }

  return assigned;
}


/*
 * Reads the value for the attribute name into value.  If name was not
 * found or value could not be interpreted as an int, value is not modified.
 *
 * According to the W3C XML Schema valid integers include zero, *all*
 * positive and *all* negative whole numbers.  For practical purposes, we
 * limit values to what can be stored in a int.  For more information,
 * see: http://www.w3.org/TR/xmlschema-2/#integer
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const std::string&  name
                         , int&                value
                         , XMLErrorLog*        log
                         , bool                required ) const
{
  long  temp;
  bool  assigned = readInto(name, temp, log, required);

  if (assigned) value = temp;
  return assigned;
}


/*
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as an unsigned int, value is not
 * modified.
 *
 * According to the W3C XML Schema valid integers include zero, *all*
 * positive and *all* negative whole numbers.  For practical purposes, we
 * limit values to what can be stored in a unsigned int.  For more
 * information, see: http://www.w3.org/TR/xmlschema-2/#integer
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const std::string&  name
                         , unsigned int&       value
                         , XMLErrorLog*        log
                         , bool                required ) const
{
  long  temp;
  bool  assigned = readInto(name, temp, log, required);

  if (assigned && temp >= 0) value = temp;
  else assigned = false;

  return assigned;
}


/*
 * Reads the value for the attribute name into value.  If name was not
 * found, value is not modified.
 *
 * If an XMLErrorLog is passed in and required is true, missing
 * attributes are logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const std::string& name
			 , std::string&       value
                         , XMLErrorLog*       log
                         , bool               required ) const
{
  bool assigned = false;
  int  index    = getIndex(name);


  if ( index != -1 )
  {
    value    = getValue(index);
    assigned = true;
  }

  if ( !log ) log = mLog;

  if ( log && !assigned && required ) attributeRequiredError(name, log);

  return assigned;
}


/** @cond doxygen-libsbml-internal */
/*
 * Writes this XMLAttributes set to stream.
 */
void
XMLAttributes::write (XMLOutputStream& stream) const
{
  for (int n = 0; n < getLength(); ++n)
  {
    if ( getPrefix(n).empty() )
    {
      stream.writeAttribute( getName(n), getValue(n) );
    }
    else
    {
      stream.writeAttribute( mNames[n], getValue(n) );
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */

/*
 * Logs an attribute format error.
 *
 * @param  name  Name of the attribute
 * @param  type  The datatype of the attribute value.
 */
void
XMLAttributes::attributeTypeError (  const std::string& name
				   , DataType           type
				   , XMLErrorLog*       log ) const
{
  ostringstream message;

  if ( !log ) log = mLog;
  if ( !log ) return;

  message << "The ";
  if ( !mElementName.empty() ) message << mElementName << ' ';
  message << name;

  switch ( type )
  {
    case XMLAttributes::Boolean:
      message <<
        " attribute must have a value of either \"true\" or \"false\""
        " (all lowercase).  The numbers \"1\" (true) and \"0\" (false) are"
        " also allowed, but not preferred.  For more information, see:"
        " http://www.w3.org/TR/xmlschema-2/#boolean.";
      break;

    case XMLAttributes::Double:
      message <<
        " attribute must be a double (decimal number).  To represent"
        " infinity use \"INF\", negative infinity use \"-INF\", and"
        " not-a-number use \"NaN\".  For more information, see:"
        " http://www.w3.org/TR/xmlschema-2/#double.";
      break;

    case XMLAttributes::Integer:
      message <<
        " attribute must be an integer (whole number).  For more"
        " information, see: http://www.w3.org/TR/xmlschema-2/#integer.";
      break;
  }

  log->add( XMLError(XMLAttributeTypeMismatch, message.str()) );
}


/*
 * Logs an error indicating a required attribute was missing.
 *
 * @param  name  Name of the attribute
 */
void
XMLAttributes::attributeRequiredError (const std::string&  name,
				       XMLErrorLog*        log) const
{
  ostringstream message;

  if ( !log ) log = mLog;
  if ( !log ) return;

  message << "The ";
  if ( !mElementName.empty() ) message << mElementName << ' ';
  message << "attribute '" << name << "' is required.";

  log->add( XMLError(MissingXMLRequiredAttribute, message.str()) );
}

/** @endcond doxygen-libsbml-internal */


/*
 * Sets the XMLErrorLog this parser will use to log errors.
 */
void
XMLAttributes::setErrorLog (XMLErrorLog* log)
{
  mLog = log;
}


/** @cond doxygen-libsbml-internal */
/*
 * Inserts this XMLAttributes set into stream.
 */
LIBLAX_EXTERN
XMLOutputStream&
operator<< (XMLOutputStream& stream, const XMLAttributes& attributes)
{
  attributes.write(stream);
  return stream;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new empty XMLAttributes_t set.
 */
LIBLAX_EXTERN
XMLAttributes_t *
XMLAttributes_create (void)
{
  return new(nothrow) XMLAttributes;
}


/**
 * Frees the given XMLAttributes_t structure.
 *
 * @param xa the XMLAttributes_t structure to be freed.
 **/
LIBLAX_EXTERN
void
XMLAttributes_free (XMLAttributes_t *xa)
{
  delete static_cast<XMLAttributes*>(xa);
}


/**
 * Creates a deep copy of the given XMLAttributes_t structure.
 * 
 * @param att the XMLAttributes_t structure to be copied
 * 
 * @return a (deep) copy of the given XMLAttributes_t structure.
 */
LIBLAX_EXTERN
XMLAttributes_t *
XMLAttributes_clone (const XMLAttributes_t* att)
{
  return static_cast<XMLAttributes*>( att->clone() );
}


/**
 * Adds a name/value pair to this XMLAttributes_t structure.
 *
 * @param xa the XMLAttributes_t structure 
 * @param name a string, the name of the attribute.
 * @param value a string, the value of the attribute.
 *
 * @note if name already exists in this attribute set, its value will be replaced.
 **/
LIBLAX_EXTERN
void
XMLAttributes_add (XMLAttributes_t *xa,
                   const char *name,
                   const char *value)
{
  xa->add(name, value);
}

/**
   * Adds a name/value pair to this XMLAttributes_t structure with a
   * prefix and URI defining a namespace.
   *
   * @param xa the XMLAttributes_t structure.
   * @param name a string, the name of the attribute.
   * @param value a string, the value of the attribute.
   * @param namespaceURI a string, the namespace URI of the attribute.
   * @param prefix a string, the prefix of the namespace
   *
   * @note if name already exists in this attribute set, its value will be replaced.
 **/
LIBLAX_EXTERN
void
XMLAttributes_addWithNamespace (XMLAttributes_t *xa,
                                const char *name,
                                const char *value,
                                const char* uri,
                                const char* prefix)
{
  xa->add(name, value, uri, prefix);
}



/**
  * Adds a name/value pair to this XMLAttributes_t structure.  
  *
  * This method is similar to the add method but an attribute with same name wont 
  * be overwritten. This facilitates the addition of multiple resource attributes 
  * to a annotations.
  *
  * @param xa the XMLAttributes_t structure.
  * @param name a string, the name of the attribute.
  * @param value a string, the value of the attribute.
 **/
LIBLAX_EXTERN
void
XMLAttributes_addResource (XMLAttributes_t *xa, 
			   const char *name, 
			   const char *value)
{
  xa->addResource(name, value);
}


/**
 * Removes a name/value pair from this XMLAttributes set.  
 *
 * @param xa the XMLAttributes_t structure.
 * @param n an integer the index of the resource to be deleted
 */
LIBLAX_EXTERN
void
XMLAttributes_removeResource (XMLAttributes_t *xa, int n)
{
  xa->removeResource(n);
}
/**
 * Return the index of an attribute by name.
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute for which the index is required.
 *
 * @return the index of the given attribute, or -1 if not present.
 */
LIBLAX_EXTERN
int
XMLAttributes_getIndex (const XMLAttributes_t *xa, const char *name)
{
  return xa->getIndex(name);
}


/**
 * Return the number of attributes in the set.
 *
 * @param xa the XMLAttributes_t structure.
 *
 * @return the number of attributes in this XMLAttributes_t structure.
 **/
LIBLAX_EXTERN
int
XMLAttributes_getLength (const XMLAttributes_t *xa)
{
  return xa->getLength();
}


/**
 * Return the name of an attribute in this XMLAttributes_t structure (by position).
 *
 * @param xa the XMLAttributes_t structure.
 * @param index an integer, the position of the attribute whose name is 
 * required.
 *
 * @return the name of an attribute in this list (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getName (const XMLAttributes_t *xa, int index)
{
  /**
   * I did this because MSVC and gcc handle .c_str() in different ways
   * meaning that with MSVC the actual string goes out of scope before 
   * the char * is returned and thus the char * is garbage once returned
   */
  if (xa->getName(index).empty())
    return NULL;
  else
    return safe_strdup(xa->getName(index).c_str());
}


/**
 * Return the value of an attribute in this XMLAttributes_t structure (by position).
 *
 * @param xa the XMLAttributes_t structure.
 * @param index an integer, the position of the attribute whose value is 
 * required.
 *
 * @return the value of an attribute in the list (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getPrefix (const XMLAttributes_t *xa, int index)
{
  /**
   * I did this because MSVC and gcc handle .c_str() in different ways
   * meaning that with MSVC the actual string goes out of scope before 
   * the char * is returned and thus the char * is garbage once returned
   */
  if (xa->getPrefix(index).empty())
    return NULL;
  else
    return safe_strdup(xa->getPrefix(index).c_str());
}


/**
 * Return the namespace URI of an attribute in this XMLAttributes_t structure (by position).
 *
 * @param xa the XMLAttributes_t structure.
 * @param index an integer, the position of the attribute whose namespace URI is 
 * required.
 *
 * @return the namespace URI of an attribute in this list (by position).
 *
 * @note If index is out of range, an empty string will be returned.  Use
 * getIndex() > 0 to test for attribute existence.
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getURI (const XMLAttributes_t *xa, int index)
{
  /**
   * I did this because MSVC and gcc handle .c_str() in different ways
   * meaning that with MSVC the actual string goes out of scope before 
   * the char * is returned and thus the char * is garbage once returned
   */
  if (xa->getURI(index).empty())
    return NULL;
  else
    return safe_strdup(xa->getURI(index).c_str());
}


/**
 * Return the value of an attribute in this XMLAttributes_t structure (by position).
 *
 * @param xa the XMLAttributes_t structure.
 * @param index an integer, the position of the attribute whose value is 
 * required.
 *
 * @return the value of an attribute in the list (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getValue (const XMLAttributes_t *xa, int index)
{
  /**
   * I did this because MSVC and gcc handle .c_str() in different ways
   * meaning that with MSVC the actual string goes out of scope before 
   * the char * is returned and thus the char * is garbage once returned
   */
  if (xa->getValue(index).empty())
    return NULL;
  else
    return safe_strdup(xa->getValue(index).c_str());
}


/**
 * Return an attribute's value by name.
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute whose value is required.
 *
 * @return The attribute value as a string.  
 *
 * @note If an attribute with the
 * given name does not exist, an empty string will be returned.  Use
 * getIndex() > 0 to test for attribute existence.
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getValueByName (const XMLAttributes_t *xa, const char *name)
{
  /**
   * I did this because MSVC and gcc handle .c_str() in different ways
   * meaning that with MSVC the actual string goes out of scope before 
   * the char * is returned and thus the char * is garbage once returned
   */
  if (xa->getValue(name).empty())
    return NULL;
  else
    return safe_strdup(xa->getValue(name).c_str());
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLAttributes_t structure is empty.
 *
 * @param xa the XMLAttributes_t structure.
 * 
 * @return @c non-zero (true) if this XMLAttributes_t structure is empty, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_isEmpty (const XMLAttributes_t *xa)
{
  return static_cast<int>( xa->isEmpty() );
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as a boolean, value is not
 * modified.
 *
 * According to the W3C XML Schema, valid boolean values are: "true",
 * "false", "1", and "0" (case-insensitive).  For more information, see:
 * http://www.w3.org/TR/xmlschema-2/#boolean
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute.
 * @param value a boolean, the value of the attribute.
 * @param log an XMLErrorLog_t, the error log.
 * @param required a boolean, indicating whether the attribute is required.
 *
 * @returns @c non-zero (true) if the attribute was read into value, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoBoolean (XMLAttributes_t *xa,
			       const char *name,
			       int *value,
			       XMLErrorLog_t *log,
			       int required)
{
  return static_cast<int>( xa->readInto(name, *(value), log, required) );
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as a double, value is not
 * modified.
 *
   * According to the W3C XML Schema, valid doubles are the same as valid
   * doubles for C and the special values "INF", "-INF", and "NaN"
   * (case-sensitive).  For more information, see:
   * http://www.w3.org/TR/xmlschema-2/#double
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute.
 * @param value a boolean, the value of the attribute.
 * @param log an XMLErrorLog_t, the error log.
 * @param required a boolean, indicating whether the attribute is required.
 *
 * @returns @c non-zero (true) if the attribute was read into value, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoDouble (XMLAttributes_t *xa,
			      const char *name,
			      double *value,
			      XMLErrorLog_t *log,
			      int required)
{
  return static_cast<int>( xa->readInto(name, *(value), log, required) );
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as a long, value is not
 * modified.
 *
   * According to the W3C XML Schema valid integers include zero, *all*
   * positive and *all* negative whole numbers.  For practical purposes, we
   * limit values to what can be stored in a long.  For more information,
   * see: http://www.w3.org/TR/xmlschema-2/#integer
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute.
 * @param value a boolean, the value of the attribute.
 * @param log an XMLErrorLog_t, the error log.
 * @param required a boolean, indicating whether the attribute is required.
 *
 * @returns @c non-zero (true) if the attribute was read into value, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoLong (XMLAttributes_t *xa,
			    const char *name,
			    long *value,
			    XMLErrorLog_t *log,
			    int required)
{
  return static_cast<int>( xa->readInto(name, *(value), log, required) );
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as an integer, value is not
 * modified.
 *
   * According to the W3C XML Schema valid integers include zero, *all*
   * positive and *all* negative whole numbers.  For practical purposes, we
   * limit values to what can be stored in a int.  For more information,
   * see: http://www.w3.org/TR/xmlschema-2/#integer
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute.
 * @param value a boolean, the value of the attribute.
 * @param log an XMLErrorLog_t, the error log.
 * @param required a boolean, indicating whether the attribute is required.
 *
 * @returns @c non-zero (true) if the attribute was read into value, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoInt (XMLAttributes_t *xa,
			   const char *name,
			   int *value,
			   XMLErrorLog_t *log,
			   int required)
{
  return static_cast<int>( xa->readInto(name, *(value), log, required) );
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as an unsigned int, value is not
 * modified.
 *
   * According to the W3C XML Schema valid integers include zero, *all*
   * positive and *all* negative whole numbers.  For practical purposes, we
   * limit values to what can be stored in a unsigned int.  For more
   * information, see: http://www.w3.org/TR/xmlschema-2/#integer
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute.
 * @param value a boolean, the value of the attribute.
 * @param log an XMLErrorLog_t, the error log.
 * @param required a boolean, indicating whether the attribute is required.
 *
 * @returns @c non-zero (true) if the attribute was read into value, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoUnsignedInt (XMLAttributes_t *xa,
				   const char *name,
				   unsigned int *value,
				   XMLErrorLog_t *log,
				   int required)
{
  return static_cast<int>( xa->readInto(name, *(value), log, required) );
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as a string, value is not
 * modified.
 *
 * If an XMLErrorLog is passed in datatype format errors are logged.  If
 * required is true, missing attributes are also logged.
 *
 *
 * @param xa the XMLAttributes_t structure.
 * @param name a string, the name of the attribute.
 * @param value a boolean, the value of the attribute.
 * @param log an XMLErrorLog_t, the error log.
 * @param required a boolean, indicating whether the attribute is required.
 *
 * @returns @c non-zero (true) if the attribute was read into value, 
 * @c zero (false) otherwise.
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoString (XMLAttributes_t *xa,
			      const char *name,
			      char *value,
			      XMLErrorLog_t *log,
			      int required)
{
  //  return static_cast<int>( xa->readInto(name, static_cast<string&>(value), log, required) );
  // for now
   return 0;
}




/** @endcond doxygen-c-only */
