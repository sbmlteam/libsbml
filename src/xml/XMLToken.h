/**
 * @file    XMLToken.h
 * @brief   A unit of XML syntax, either an XML element or text.
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 * @class XMLToken.
 * @brief Implementation of %XMLToken construct.
 */


#ifndef XMLToken_h
#define XMLToken_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLTriple.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <string>



class XMLOutputStream;


class LIBLAX_EXTERN XMLToken
{
public:

  /**
   * Creates a new empty XMLToken.
   */
  XMLToken ();

  /**
   * Creates a start element XMLToken with the given set of attributes and
   * namespace declarations.
   */
  XMLToken (  const XMLTriple&      triple
            , const XMLAttributes&  attributes
            , const XMLNamespaces&  namespaces
            , const unsigned int    line   = 0
            , const unsigned int    column = 0 );

  /**
   * Creates a start element XMLToken with the given set of attributes.
   */
  XMLToken (  const XMLTriple&      triple
            , const XMLAttributes&  attributes
            , const unsigned int    line   = 0
            , const unsigned int    column = 0 );


  /**
   * Creates an end element XMLToken.
   */
  XMLToken (  const XMLTriple&    triple
            , const unsigned int  line   = 0
            , const unsigned int  column = 0 );

  /**
   * Creates a text XMLToken.
   */
  XMLToken (  const std::string&  chars
            , const unsigned int  line   = 0
            , const unsigned int  column = 0 );

  /**
   * Destroys this XMLToken.
   */
  virtual ~XMLToken ();


  /**
   * Appends characters to this XML text content.
   */
  void append (const std::string& chars);

  /**
   * @return the XMLAttributes of this XML element.
   */
  const XMLAttributes& getAttributes () const;

  /**
   * @return the characters of this XML text.
   */
  const std::string& getCharacters () const;

  /**
   * @return the column at which this XMLToken occurred.
   */
  unsigned int getColumn () const;

  /**
   * @return the line at which this XMLToken occurred.
   */
  unsigned int getLine () const;

  /**
   * @return the XML namespace declarations for this XML element.
   */
  const XMLNamespaces& getNamespaces () const;

  /**
   * @return the (unqualified) name of this XML element.
   */
  const std::string& getName () const;

  /**
   * @return the namespace prefix of this XML element.  If no prefix
   * exists, an empty string will be return.
   */
  const std::string& getPrefix () const;

  /**
   * @return the namespace URI of this XML element.
   */
  const std::string& getURI () const;


  /**
   * @return true if this XMLToken is an XML element.
   */
  bool isElement () const;

  /**
   * @return true if this XMLToken is an XML end element, false
   * otherwise.
   */
  bool isEnd () const;

  /**
   * @return true if this XMLToken is an XML end element for the given XML
   * start element, false otherwise.
   */
  bool isEndFor (const XMLToken& element) const;

  /**
   * @return true if this XMLToken is an end of file (input) marker, false
   * otherwise.
   */
  bool isEOF () const;

  /**
   * @return true if this XMLToken is an XML start element, false
   * otherwise.
   */
  bool isStart () const;

  /**
   * @return true if this XMLToken is text, false otherwise.
   */
  bool isText () const;

  /**
   * Declares this XML start element is also an end element.
   */
  void setEnd ();

  /**
   * Declares this XMLToken is an end-of-file (input) marker.
   */
  void setEOF ();

  /**
   * Writes this XMLToken to stream.
   */
  void write (XMLOutputStream& stream) const;


  /**
   * Prints a string representation of the underlying token stream, for
   * debugging purposes.
   */
  std::string toString ();


#ifndef SWIG

  /**
   * Inserts this XMLToken into stream.
   */
  LIBLAX_EXTERN
  friend
  XMLOutputStream& operator<< (XMLOutputStream& stream, const XMLToken& token);

#endif  /* !SWIG */


protected:

  XMLTriple     mTriple;
  XMLAttributes mAttributes;
  XMLNamespaces mNamespaces;

  std::string mChars;

  bool mIsStart;
  bool mIsEnd;
  bool mIsText;

  unsigned int mLine;
  unsigned int mColumn;
};

#endif  /* __cplusplus */

#ifndef SWIG

BEGIN_C_DECLS


/**
 * 
 **/
LIBLAX_EXTERN
XMLToken_t *
XMLToken_create (void);


/**
 * 
 **/
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithTriple (const XMLTriple_t *triple);


/**
 * 
 **/
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithTripleAttr (const XMLTriple_t *triple,
			       const XMLAttributes_t *attr);


/**
 * 
 **/
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithTripleAttrNS (const XMLTriple_t *triple,
				 const XMLAttributes_t *attr,
				 const XMLNamespaces_t *ns);


/**
 * 
 **/
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithText (const char *text);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLToken_free (XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLToken_append (XMLToken_t *token, const char *text);


/**
 * 
 **/
LIBLAX_EXTERN
const XMLAttributes_t *
XMLToken_getAttributes (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLToken_getCharacters (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
unsigned int
XMLToken_getColumn (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
unsigned int
XMLToken_getLine (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
const XMLNamespaces_t *
XMLToken_getNamespaces (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLToken_getName (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLToken_getPrefix (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLToken_getURI (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLToken_isElement (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLToken_isEnd (const XMLToken_t *token); 


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLToken_isEndFor (const XMLToken_t *token, const XMLToken_t *element);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLToken_isEOF (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLToken_isStart (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLToken_isText (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLToken_setEnd (XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLToken_setEOF (XMLToken_t *token);



END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLToken_h */
