/**
 * \file    XMLAttributes.h
 * \brief   XMLAttributes are a list of name/value pairs for XML elements
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


#ifndef XMLAttributes_h
#define XMLAttributes_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>
#include <vector>

#include <sbml/xml/XMLTriple.h>


class XMLErrorLog;
class XMLOutputStream;


class LIBLAX_EXTERN XMLAttributes
{
public:

  /**
   * Creates a new empty XMLAttributes set.
   */
  XMLAttributes ();

  /**
   * Destroys this XMLAttributes set.
   */
  virtual ~XMLAttributes ();


  /**
   * Adds a name/value pair to this XMLAttributes set.  If name already
   * exists in this attribute set, its value will be replaced.
   */
  void add (const std::string& name, const std::string& value,const std::string& namespaceURI="", const std::string& prefix="");

  /**
  * Adds a name/value pair to this XMLAttributes set.  This
  * is really the add function but an attribute with same name wont 
  * be overwritten - this is for annotations
  */
  void addResource (const std::string& name, const std::string& value);

/**
   * Lookup the index of an attribute by name.
   *
   * @return the index of the given attribute, or -1 if not present.
   */
  int getIndex (const std::string name) const;

  /**
   * @return the number of attributes in this list.
   */
  int getLength () const;

  /**
   * @return the name of an attribute in this list (by position).  If index
   * is out of range, an empty string will be returned.  Use getIndex() > 0
   * to test for attribute existence.
   */
  std::string getName (int index) const;

  /**
   * @return the namespace prefix of an attribute in this list (by
   * position).  If index is out of range, an empty string will be
   * returned.  Use getIndex() > 0 to test for attribute existence.
   */
  std::string getPrefix (int index) const;

  /**
   * @return the namespace URI of an attribute in this list (by position).
   * If index is out of range, an empty string will be returned.  Use
   * getIndex() > 0 to test for attribute existence.
   */
  std::string getURI (int index) const;

  /**
   * @return the value of an attribute in the list (by position).  If index
   * is out of range, an empty string will be returned.  Use getIndex() > 0
   * to test for attribute existence.
   */
  std::string getValue (int index) const;

  /**
   * Lookup an attribute's value by name.
   *
   * @return The attribute value as a string.  If an attribute with the
   * given name does not exist, an empty string will be returned.  Use
   * getIndex() > 0 to test for attribute existence.
   */
  std::string getValue (const std::string name) const;

  /**
   * @return true if this XMLAttributes set is empty, false otherwise.
   */
  bool isEmpty () const;

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
   * @returns true if the attribute was read into value, false otherwise.
   */
  bool readInto (  const std::string&  name
                 , bool&               value
                 , XMLErrorLog*        log      = 0
                 , bool                required = false ) const;

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
  bool readInto (  const std::string&  name
                 , double&             value
                 , XMLErrorLog*        log      = 0
                 , bool                required = false ) const;

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
  bool readInto (  const std::string&  name
                 , long&               value
                 , XMLErrorLog*        log      = 0
                 , bool                required = false ) const;

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
  bool readInto (  const std::string&  name
                 , int&                value
                 , XMLErrorLog*        log      = 0
                 , bool                required = false ) const;

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
  bool readInto (  const std::string&  name
                 , unsigned int&       value
                 , XMLErrorLog*        log      = 0
                 , bool                required = false ) const;

  /**
   * Reads the value for the attribute name into value.  If name was not
   * found, value is not modified.
   *
   * If an XMLErrorLog is passed in and required is true, missing
   * attributes are logged.
   *
   * @returns true if the attribute was read into value, false otherwise.
   */
  bool readInto (  const std::string&  name
                 , std::string&        value
                 , XMLErrorLog*        log      = 0
                 , bool                required = false ) const;

  /**
   * Writes this XMLAttributes set to stream.
   */
  void write (XMLOutputStream& stream) const;


#ifndef SWIG

  /**
   * Inserts this XMLAttributes set into stream.
   */
  LIBLAX_EXTERN
  friend XMLOutputStream&
  operator<< (XMLOutputStream& stream, const XMLAttributes& attributes);

#endif  /* !SWIG */

protected:

  std::vector<XMLTriple>    mNames;
  std::vector<std::string>  mValues;
};

#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS


/**
 * 
 **/
LIBLAX_EXTERN
XMLAttributes_t *
XMLAttributes_create (void);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLAttributes_free (XMLAttributes_t *xa);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLAttributes_add (XMLAttributes_t *xa, const char *name, const char *value);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLAttributes_addWithNamespace (XMLAttributes_t *xa, const char *name, const char *value, const char* uri, const char* prefix);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLAttributes_addResource (XMLAttributes_t *xa, 
			   const char *name, 
			   const char *value);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_getIndex (const XMLAttributes_t *xa, const char *name);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_getLength (const XMLAttributes_t *xa);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getName (const XMLAttributes_t *xa, int index);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getPrefix (const XMLAttributes_t *xa, int index);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getURI (const XMLAttributes_t *xa, int index);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getValue (const XMLAttributes_t *xa, int index);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLAttributes_getValueByName (const XMLAttributes_t *xa, const char *name);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_isEmpty (const XMLAttributes_t *xa);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoBoolean (XMLAttributes_t *xa,
			       const char *name,
			       int value,
			       XMLErrorLog_t *log,
			       int required);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoDouble (XMLAttributes_t *xa,
			      const char *name,
			      double value,
			      XMLErrorLog_t *log,
			      int required);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoLong (XMLAttributes_t *xa,
			    const char *name,
			    long value,
			    XMLErrorLog_t *log,
			    int required);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoInt (XMLAttributes_t *xa,
			   const char *name,
			   int value,
			   XMLErrorLog_t *log,
			   int required);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoUnsignedInt (XMLAttributes_t *xa,
				   const char *name,
				   unsigned int value,
				   XMLErrorLog_t *log,
				   int required);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLAttributes_readIntoString (XMLAttributes_t *xa,
			      const char *name,
			      char *value,
			      XMLErrorLog_t *log,
			      int required);





END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLAttributes_h */
