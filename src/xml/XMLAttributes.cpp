/**
 * \file    XMLAttributes.cpp
 * \brief   XMLAttributes are a list of name/value pairs for XMLElements
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <cerrno>
#include <clocale>
#include <cstdlib>
#include <limits>
#include <sstream>

#include "XMLErrorLog.h"
#include "XMLOutputStream.h"
#include "XMLAttributes.h"


using namespace std;


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


/**
 * Creates a new empty XMLAttributes set.
 */
XMLAttributes::XMLAttributes ()
{
}


/**
 * Destroys this XMLAttributes set.
 */
XMLAttributes::~XMLAttributes ()
{
}



/**
 * Adds a name/value pair to this XMLAttributes set.  If name with the same namespace URI already
 * exists in this attribute set, its value will be replaced.
 */
void
XMLAttributes::add (const string& name, const string& value, const string& namespaceURI, const string& prefix)
{
  int index = getIndex(name);

  // since in the old version of the method the XMLTriple was initialized with empty strings
  // for the prefix and the uri, I assume that only attributes that are not from the default namespace 
  // should have a set prefix and uri. 
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


/**
 * Adds a name/value pair to this XMLAttributes set.  This
 * is really the add function but an attribute with same name wont 
 * be overwritten - this is for annotations
 */
void
XMLAttributes::addResource (const string& name, const string& value)
{
  mNames .push_back( XMLTriple(name, "", "") );
  mValues.push_back( value );
}


/**
 * Lookup the index of an attribute by name.
 *
 * @return the index of the given attribute, or -1 if not present.
 */
int
XMLAttributes::getIndex (const string name) const
{
  for (int index = 0; index < getLength(); ++index)
  {
    if (getName(index) == name) return index;
  }
  
  return -1;
}


/**
 * @return the number of attributes in this list.
 */
int
XMLAttributes::getLength () const
{
  return mNames.size();
}


/**
 * @return the name of an attribute in this list (by position).  If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 */
string
XMLAttributes::getName (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNames[index].getName();
}


/**
 * @return the namespace prefix of an attribute in this list (by
 * position).  If index is out of range, an empty string will be
 * returned.  Use getIndex() > 0 to test for attribute existence.
 */
string
XMLAttributes::getPrefix (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNames[index].getPrefix();
}


/**
 * @return the namespace URI of an attribute in this list (by position).
 * If index is out of range, an empty string will be returned.  Use
 * getIndex() > 0 to test for attribute existence.
 */
string
XMLAttributes::getURI (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mNames[index].getURI();
}


/**
 * @return the value of an attribute in the list (by position).  If index
 * is out of range, an empty string will be returned.  Use getIndex() > 0
 * to test for attribute existence.
 */
string
XMLAttributes::getValue (int index) const
{
  return (index < 0 || index >= getLength()) ? "" : mValues[index];
}


/**
 * Lookup an attribute's value by name.
 *
 * @return The attribute value as a string.  If an attribute with the
 * given name does not exist, an empty string will be returned.  Use
 * getIndex() > 0 to test for attribute existence.
 */
string
XMLAttributes::getValue (const string name) const
{
  return getValue( getIndex(name) );
}


/**
 * @return true if this XMLAttributes set is empty, false otherwise.
 */
bool
XMLAttributes::isEmpty () const
{
  return (getLength() == 0);
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as a boolean, value is not modified.
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
XMLAttributes::readInto (  const string&   name
                         , bool&           value
                         , XMLErrorLog*    log
                         , bool            required ) const
{
  bool assigned = false;
  bool missing  = true;
  int  index    = getIndex(name);

  if (index != -1)
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

  if (log && !assigned)
  {
         if (!missing) log->attributeTypeError(name, XMLErrorLog::Boolean);
    else if (required) log->attributeRequired (name);
  }

  return assigned;
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
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const string&   name
                         , double&         value
                         , XMLErrorLog*    log
                         , bool            required ) const
{
  bool assigned = false;
  bool missing  = true;
  int  index    = getIndex(name);

  if (index != -1)
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

  if (log && !assigned)
  {
         if (!missing) log->attributeTypeError(name, XMLErrorLog::Double);
    else if (required) log->attributeRequired (name);
  }

  return assigned;
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as an long, value is not modified.
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
XMLAttributes::readInto (  const string&   name
                         , long&           value
                         , XMLErrorLog*    log
                         , bool            required ) const
{
  bool assigned = false;
  bool missing  = true;
  int  index    = getIndex(name);

  if (index != -1)
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

  if (log && !assigned)
  {
         if (!missing) log->attributeTypeError(name, XMLErrorLog::Integer);
    else if (required) log->attributeRequired (name);
  }

  return assigned;
}


/**
 * Reads the value for the attribute name into value.  If name was not
 * found or value could be interpreted as an int, value is not modified.
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


/**
 * Reads the value for the attribute name into value.  If name was not
 * found, value is not modified.
 *
 * If an XMLErrorLog is passed in and required is true, missing
 * attributes are logged.
 *
 * @returns true if the attribute was read into value, false otherwise.
 */
bool
XMLAttributes::readInto (  const string&   name
                         , string&         value
                         , XMLErrorLog*    log
                         , bool            required ) const
{
  bool assigned = false;
  int  index    = getIndex(name);


  if (index != -1)
  {
    value    = getValue(index);
    assigned = true;
  }

  if (log && !assigned && required) log->attributeRequired(name);

  return assigned;
}


/**
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


/**
 * Inserts this XMLAttributes set into stream.
 */
LIBLAX_EXTERN
XMLOutputStream&
operator<< (XMLOutputStream& stream, const XMLAttributes& attributes)
{
  attributes.write(stream);
  return stream;
}


/**
 * 
 **/
LIBLAX_EXTERN
XMLAttributes_t *
XMLAttributes_create (void)
{
  return new(nothrow) XMLAttributes;
}


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLAttributes_free (XMLAttributes_t *xa)
{
  delete static_cast<XMLAttributes*>(xa);
}


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLAttributes_add (XMLAttributes_t *xa, const char *name, const char *value, const char* uri, const char* prefix)
{
  xa->add(name, value, uri, prefix);
}


/**
 * 
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
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_getIndex (const XMLAttributes_t *xa, const char *name)
{
  return xa->getIndex(name);
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_getLength (const XMLAttributes_t *xa)
{
  return xa->getLength();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getName (const XMLAttributes_t *xa, int index)
{
  return xa->getName(index).empty() ? NULL : xa->getName(index).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getPrefix (const XMLAttributes_t *xa, int index)
{
  return xa->getPrefix(index).empty() ? NULL : xa->getPrefix(index).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getURI (const XMLAttributes_t *xa, int index)
{
  return xa->getURI(index).empty() ? NULL : xa->getURI(index).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getValue (const XMLAttributes_t *xa, int index)
{
  return xa->getValue(index).empty() ? NULL : xa->getValue(index).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getValueByName (const XMLAttributes_t *xa, const char *name)
{
  return xa->getValue(name).empty() ? NULL : xa->getValue(name).c_str();
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_isEmpty (const XMLAttributes_t *xa)
{
  return static_cast<int>( xa->isEmpty() );
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoBoolean (XMLAttributes_t *xa,
			       const char *name,
			       int &value,
			       XMLErrorLog_t *log,
			       int required)
{
  return static_cast<int>( xa->readInto(name, value, log, required) );
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoDouble (XMLAttributes_t *xa,
			      const char *name,
			      double &value,
			      XMLErrorLog_t *log,
			      int required)
{
  return static_cast<int>( xa->readInto(name, value, log, required) );
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoLong (XMLAttributes_t *xa,
			    const char *name,
			    long &value,
			    XMLErrorLog_t *log,
			    int required)
{
  return static_cast<int>( xa->readInto(name, value, log, required) );
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoInt (XMLAttributes_t *xa,
			   const char *name,
			   int &value,
			   XMLErrorLog_t *log,
			   int required)
{
  return static_cast<int>( xa->readInto(name, value, log, required) );
}


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoUnsignedInt (XMLAttributes_t *xa,
				   const char *name,
				   unsigned int &value,
				   XMLErrorLog_t *log,
				   int required)
{
  return static_cast<int>( xa->readInto(name, value, log, required) );
}


/**
 * 
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
}

