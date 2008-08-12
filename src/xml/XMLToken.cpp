/**
 * @file    XMLToken.cpp
 * @brief   A unit of XML syntax, either an XML element or text.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
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

#include <sstream>

/** @cond doxygen-libsbml-internal */
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/util/util.h>
/** @endcond doxygen-libsbml-internal */
#include <sbml/xml/XMLToken.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new empty XMLToken.
 */
XMLToken::XMLToken () :
   mIsStart   ( false )
 , mIsEnd     ( false )
 , mIsText    ( false )
 , mLine      ( 0     )
 , mColumn    ( 0     )
{
}


/*
 * Creates a start element XMLToken with the given set of attributes and
 * namespace declarations.
 */
XMLToken::XMLToken (  const XMLTriple&      triple
                    , const XMLAttributes&  attributes
                    , const XMLNamespaces&  namespaces
                    , const unsigned int    line
                    , const unsigned int    column ) :
   mTriple    ( triple     )
 , mAttributes( attributes )
 , mNamespaces( namespaces )
 , mIsStart   ( true       )
 , mIsEnd     ( false      )
 , mIsText    ( false      )
 , mLine      ( line       )
 , mColumn    ( column     )
{
}


/*
 * Creates a start element XMLToken with the given set of attributes.
 */
XMLToken::XMLToken (  const XMLTriple&      triple
                    , const XMLAttributes&  attributes
                    , const unsigned int    line
                    , const unsigned int    column ) :
   mTriple    ( triple     )
 , mAttributes( attributes )
 , mIsStart   ( true       )
 , mIsEnd     ( false      )
 , mIsText    ( false      )
 , mLine      ( line       )
 , mColumn    ( column     )
{
}


/*
 * Creates an end element XMLToken.
 */
XMLToken::XMLToken (  const XMLTriple&    triple
                    , const unsigned int  line
                    , const unsigned int  column ) :
   mTriple    ( triple )
 , mIsStart   ( false  )
 , mIsEnd     ( true   )
 , mIsText    ( false  )
 , mLine      ( line   )
 , mColumn    ( column )

{
}


/*
 * Creates a text XMLToken.
 */
XMLToken::XMLToken (  const std::string&  chars
                    , const unsigned int  line
                    , const unsigned int  column ) :
   mChars     ( chars  )
 , mIsStart   ( false  )
 , mIsEnd     ( false  )
 , mIsText    ( true   )
 , mLine      ( line   )
 , mColumn    ( column )
{
}


/*
 * Destroys this XMLToken.
 */
XMLToken::~XMLToken ()
{
}


/*
 * Copy constructor; creates a copy of this XMLToken.
 */
XMLToken::XMLToken(const XMLToken& orig)
{
  if (orig.mTriple.isEmpty())
    mTriple = XMLTriple();
  else
    mTriple = XMLTriple(orig.getName(), orig.getURI(), orig.getPrefix());
  
  if (orig.mAttributes.isEmpty())
    mAttributes = XMLAttributes();
  else
    mAttributes = XMLAttributes(orig.getAttributes());
  
  if (orig.mNamespaces.isEmpty())
    mNamespaces = XMLNamespaces();
  else
    mNamespaces = XMLNamespaces(orig.getNamespaces());

  mChars = orig.mChars;

  mIsStart = orig.mIsStart;
  mIsEnd = orig.mIsEnd;
  mIsText = orig.mIsText;

  mLine = orig.mLine;
  mColumn = orig.mColumn;
}


/*
 * Assignment operator for XMLToken.
 */
XMLToken& 
XMLToken::operator=(const XMLToken& orig)
{
  if (orig.mTriple.isEmpty())
    mTriple = XMLTriple();
  else
    mTriple = XMLTriple(orig.getName(), orig.getURI(), orig.getPrefix());
  
  if (orig.mAttributes.isEmpty())
    mAttributes = XMLAttributes();
  else
    mAttributes = XMLAttributes(orig.getAttributes());
  
  if (orig.mNamespaces.isEmpty())
    mNamespaces = XMLNamespaces();
  else
    mNamespaces = XMLNamespaces(orig.getNamespaces());

  mChars = orig.mChars;

  mIsStart = orig.mIsStart;
  mIsEnd = orig.mIsEnd;
  mIsText = orig.mIsText;

  mLine = orig.mLine;
  mColumn = orig.mColumn;

  return *this;
}

/*
 * Creates and returns a deep copy of this XMLToken.
 * 
 * @return a (deep) copy of this XMLToken set.
 */
XMLToken* 
XMLToken::clone () const
{
  return new XMLToken(*this);
}


/*
 * Appends characters to this XML text content.
 */
void
XMLToken::append (const std::string& chars)
{
  mChars.append(chars);
}


/*
 * @return the XMLAttributes of this XML element.
 */
const XMLAttributes&
XMLToken::getAttributes () const
{
  return mAttributes;
}


/*
 * @return the characters of this XML text.
 */
const string&
XMLToken::getCharacters () const
{
  return mChars;
} 


/*
 * @return the column at which this XMLToken occurred.
 */
unsigned int
XMLToken::getColumn () const
{
  return mColumn;
}


/*
 * @return the line at which this XMLToken occurred.
 */
unsigned int
XMLToken::getLine () const
{
  return mLine;
}



/*
 * @return the XML namespace declarations for this XML element.
 */
const XMLNamespaces&
XMLToken::getNamespaces () const
{
  return mNamespaces;
}


/**
 * Sets an XMLnamespaces to this XML element.
 *
 * @note This function replaces the existing XMLNamespaces with the new one.
 */
void 
XMLToken::setNamespaces(const XMLNamespaces& namespaces)
{
  mNamespaces = namespaces;
}


/**
 * Appends an XML namespace prefix and URI pair to this XMLToken.
 * If there is an XML namespace with the given prefix in this XMLToken, 
 * then the existing XML namespace will be overwritten by the new one.
 */
void 
XMLToken::addNamespace (const std::string& uri, const std::string& prefix)
{
   mNamespaces.add(uri, prefix);
}


/**
 * Removes an XML Namespace stored in the given position of the XMLNamespaces
 * of this XMLToken.
 *
 * @param index an integer, position of the removed namespace.
 */
void 
XMLToken::removeNamespace (int index)
{
  mNamespaces.remove(index);
}


/**
 * Removes an XML Namespace with the given prefix.
 *
 * @param prefix a string, prefix of the required namespace.
 */
void 
XMLToken::removeNamespace (const std::string& prefix)
{
  mNamespaces.remove(prefix);
}


/**
 * Clears (deletes) all XML namespace declarations in the XMLNamespaces of
 * this XMLToken.
 */
void 
XMLToken::clearNamespaces ()
{
  mNamespaces.clear();
}


/**
 * Look up the index of an XML namespace declaration by URI.
 *
 * @param uri a string, uri of the required namespace.
 *
 * @return the index of the given declaration, or -1 if not present.
 */
int 
XMLToken::getNamespaceIndex (const std::string& uri) const
{
  return mNamespaces.getIndex(uri);
}


/**
 * Look up the index of an XML namespace declaration by prefix.
 *
 * @param prefix a string, prefix of the required namespace.
 *
 * @return the index of the given declaration, or -1 if not present.
 */
int 
XMLToken::getNamespaceIndexByPrefix (const std::string& prefix) const
{
  return mNamespaces.getIndexByPrefix(prefix);
}


/**
 * Returns the number of XML namespaces stored in the XMLNamespaces 
 * of this XMLToken.
 *
 * @return the number of namespaces in this list.
 */
int 
XMLToken::getNamespacesLength () const
{
  return mNamespaces.getLength();
}


/**
 * Look up the prefix of an XML namespace declaration by position.
 *
 * Callers should use getNamespacesLength() to find out how many 
 * namespaces are stored in the XMLNamespaces.
 * 
 * @return the prefix of an XML namespace declaration in the XMLNamespaces 
 * (by position).  
 */
std::string 
XMLToken::getNamespacePrefix (int index) const
{
  return mNamespaces.getPrefix(index);
}


/**
 * Look up the prefix of an XML namespace declaration by its URI.
 *
 * @return the prefix of an XML namespace declaration given its URI.  
 */
std::string 
XMLToken::getNamespacePrefix (const std::string& uri) const
{
  return mNamespaces.getPrefix(uri);
}


/**
 * Look up the URI of an XML namespace declaration by its position.
 *
 * @return the URI of an XML namespace declaration in the XMLNamespaces
 * (by position).  
 */
std::string 
XMLToken::getNamespaceURI (int index) const
{
  return mNamespaces.getURI(index);
}


/**
 * Look up the URI of an XML namespace declaration by its prefix.
 *
 * @return the URI of an XML namespace declaration given its prefix.  
 */
std::string 
XMLToken::getNamespaceURI (const std::string& prefix) const
{
  return mNamespaces.getURI(prefix);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * the XMLNamespaces of this XMLToken is empty.
 * 
 * @return @c true if the XMLNamespaces of this XMLToken is empty, 
 * @c false otherwise.
 */
bool 
XMLToken::isNamespacesEmpty () const
{
  return mNamespaces.isEmpty();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * an XML Namespace with the given URI is contained in the XMLNamespaces of
 * this XMLToken.
 *
 * @return @c true if an XML Namespace with the given URI is contained in the
 * XMLNamespaces of this XMLToken,  @c false otherwise.
 */
bool 
XMLToken::hasNamespaceURI(const std::string& uri) const
{
  return mNamespaces.hasURI(uri);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * an XML Namespace with the given prefix is contained in the XMLNamespaces of
 * this XMLToken.
 *
 * @param prefix a string, the prefix for the namespace
 * 
 * @return @c true if an XML Namespace with the given URI is contained in the
 * XMLNamespaces of this XMLToken, @c false otherwise.
 */
bool 
XMLToken::hasNamespacePrefix(const std::string& prefix) const
{
  return mNamespaces.hasPrefix(prefix);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * an XML Namespace with the given uri/prefix pair is contained in the 
 * XMLNamespaces ofthis XMLToken.
 *
 * @param uri a string, the uri for the namespace
 * @param prefix a string, the prefix for the namespace
 * 
 * @return @c true if an XML Namespace with the given uri/prefix pair is 
 * contained in the XMLNamespaces of this XMLToken,  @c false otherwise.
 */
bool 
XMLToken::hasNamespaceNS(const std::string& uri, const std::string& prefix) const
{
  return mNamespaces.hasNS(uri,prefix);
}



/*
 * @return the (unqualified) name of this XML element.
 */
const string&
XMLToken::getName () const
{
  return mTriple.getName();
}


/*
 * @return the namespace prefix of this XML element.  If no prefix
 * exists, an empty string will be return.
 */
const string&
XMLToken::getPrefix () const
{
  return mTriple.getPrefix();
}


/*
 * @return the namespace URI of this XML element.
 */
const string&
XMLToken::getURI () const
{
  return mTriple.getURI();
}


/*
 * @return true if this XMLToken is an XML element.
 */
bool
XMLToken::isElement () const
{
  return mIsStart || mIsEnd;
}

 
/*
 * @return true if this XMLToken is an XML end element, false
 * otherwise.
 */
bool
XMLToken::isEnd () const
{
  return mIsEnd;
}


/*
 * @return true if this XMLToken is an XML end element for the given XML
 * start element, false otherwise.
 */
bool
XMLToken::isEndFor (const XMLToken& element) const
{
  return
    isEnd()                        &&
    element.isStart()              &&
    element.getName() == getName() &&
    element.getURI () == getURI ();
}


/*
 * @return true if this XMLToken is an end of file (input) marker, false
 * otherwise.
 */
bool
XMLToken::isEOF () const
{
  return (mIsStart == false && mIsEnd == false && mIsText == false);
}


/*
 * @return true if this XMLToken is an XML start element, false
 * otherwise.
 */
bool
XMLToken::isStart () const
{
  return mIsStart;
}


/*
 * @return true if this XMLToken is text, false otherwise.
 */
bool
XMLToken::isText () const
{
  return mIsText;
}


/*
 * Declares this XML start element is also an end element.
 */
void
XMLToken::setEnd ()
{
  mIsEnd = true;
}


/*
 * Declares this XML start/end element is no longer an end element.
 */
void
XMLToken::unsetEnd ()
{
  mIsEnd = false;
}


/*
 * Declares this XMLToken is an end-of-file (input) marker.
 */
void
XMLToken::setEOF ()
{
  mIsStart = false;
  mIsEnd   = false;
  mIsText  = false;
}


/** @cond doxygen-libsbml-internal */
/*
 * Writes this XMLToken to stream.
 */
void
XMLToken::write (XMLOutputStream& stream) const
{
  if ( isEOF () ) return;

  if ( isText() )
  {
    stream << getCharacters();
    return;
  }

  if ( isStart() ) stream.startElement( mTriple );
  if ( isStart() ) stream << mNamespaces << mAttributes;
  if ( isEnd()   ) stream.endElement( mTriple );
}
/** @endcond doxygen-libsbml-internal */


/*
 * Prints a string representation of the underlying token stream, for
 * debugging purposes.
 */
string
XMLToken::toString ()
{
  ostringstream stream;

  if ( isText() )
  {
    stream << getCharacters();
  }
  else
  {
    stream << '<';
    if ( !isStart() && isEnd() ) stream << '/';

    stream << getName();

    if (  isStart() && isEnd() ) stream << '/';
    stream << '>';
  }

  return stream.str();
}


/** @cond doxygen-libsbml-internal */
/*
 * Inserts this XMLToken into stream.
 */
LIBLAX_EXTERN
XMLOutputStream&
operator<< (XMLOutputStream& stream, const XMLToken& token)
{
  token.write(stream);
  return stream;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new empty XMLToken_t structure and returns a pointer to it.
 *
 * @return pointer to new XMLToken_t structure.
 */
LIBLAX_EXTERN
XMLToken_t *
XMLToken_create (void)
{
  return new(nothrow) XMLToken;
}


/**
 * Creates a new end element XMLToken_t structure with XMLTriple_t structure set
 * and returns a pointer to it.
 *
 * @param triple XMLTriple_t structure to be set.
 *
 * @return pointer to new XMLToken_t structure.
 */
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithTriple (const XMLTriple_t *triple)
{
  return new(nothrow) XMLToken(*triple);
}


/**
 * Creates a new start element XMLToken_t structure with XMLTriple_t and XMLAttributes_t
 * structures set and returns a pointer to it.
 *
 * @param triple XMLTriple_t structure to be set.
 * @param attr XMLAttributes_t structure to be set.
 *
 * @return pointer to new XMLToken_t structure.
 */
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithTripleAttr (const XMLTriple_t *triple,
			       const XMLAttributes_t *attr)
{
  return new(nothrow) XMLToken(*triple, *attr);
}


/**
 * Creates a new start element XMLToken_t structure with XMLTriple_t, XMLAttributes_t
 * and XMLNamespaces_t structures set and returns a pointer to it.
 *
 * @param triple XMLTriple_t structure to be set.
 * @param attr XMLAttributes_t structure to be set.
 * @param ns XMLNamespaces_t structure to be set.
 *
 * @return pointer to new XMLToken_t structure.
 */
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithTripleAttrNS (const XMLTriple_t *triple,
				 const XMLAttributes_t *attr,
				 const XMLNamespaces_t *ns)
{
  return new(nothrow) XMLToken(*triple, *attr, *ns);
}


/**
 * Creates a text XMLToken_t structure.
 *
 * @param text a string, the text to be added to the XMLToken_t structure
 *
 * @return pointer to new XMLToken_t structure.
 */
LIBLAX_EXTERN
XMLToken_t *
XMLToken_createWithText (const char *text)
{
  return (text != NULL) ? new(nothrow) XMLToken(text) : new(nothrow) XMLToken;
}

/**
 * Destroys this XMLToken_t structure.
 *
 * @param token XMLToken_t structure to be freed.
 **/
LIBLAX_EXTERN
void
XMLToken_free (XMLToken_t *token)
{
  delete static_cast<XMLToken*>( token );
}


/**
 * Creates a deep copy of the given XMLToken_t structure
 * 
 * @param t the XMLToken_t structure to be copied
 * 
 * @return a (deep) copy of the given XMLToken_t structure.
 */
LIBLAX_EXTERN
XMLToken_t *
XMLToken_clone (const XMLToken_t* t)
{
  return static_cast<XMLToken*>( t->clone() );
}


/**
 * Appends characters to this XML text content.
 *
 * @param token XMLToken_t structure to be appended to.
 * @param text string, characters to append
 **/
LIBLAX_EXTERN
void
XMLToken_append (XMLToken_t *token, const char *text)
{
  if (text != NULL)
  {
    token->append(text);
  }
}


/**
 * Returns the attributes of this element.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the XMLAttributes_t of this XML element.
 */
LIBLAX_EXTERN
const XMLAttributes_t *
XMLToken_getAttributes (const XMLToken_t *token)
{
  return &(token->getAttributes());
}


/**
 * Returns the text of this element.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the characters of this XML text.
 */
LIBLAX_EXTERN
const char *
XMLToken_getCharacters (const XMLToken_t *token)
{
  return token->getCharacters().empty() ? NULL : token->getCharacters().c_str();
}


/**
 * Returns the column at which this XMLToken_t structure occurred.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the column at which this XMLToken_t structure occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLToken_getColumn (const XMLToken_t *token)
{
  return token->getColumn();
}    


/**
 * Returns the line at which this XMLToken_t structure occurred.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the line at which this XMLToken_t structure occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLToken_getLine (const XMLToken_t *token)
{
  return token->getLine();
}    



/**
 * Returns the XML namespace declarations for this XML element.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the XML namespace declarations for this XML element.
 */
LIBLAX_EXTERN
const XMLNamespaces_t *
XMLToken_getNamespaces (const XMLToken_t *token)
{
  return &(token->getNamespaces());
}


/**
 * Sets an XMLnamespaces to this XML element.
 *
 * @param token XMLToken_t structure to be queried.
 * @param namespaces XMLNamespaces to be set to this XMLToken.
 *
 * @note This function replaces the existing XMLNamespaces with the new one.
 */
LIBLAX_EXTERN
void 
XMLToken_setNamespaces(XMLToken_t *token, const XMLNamespaces_t* namespaces)
{
  if(!namespaces)
  {
    return;
  }

  token->setNamespaces(*namespaces);
}


/**
 * Appends an XML namespace prefix and URI pair to this XMLToken.
 * If there is an XML namespace with the given prefix in this XMLToken, 
 * then the existing XML namespace will be overwritten by the new one.
 *
 * @param token XMLToken_t structure to be queried.
 * @param uri a string, the uri for the namespace
 * @param prefix a string, the prefix for the namespace
 */
LIBLAX_EXTERN
void 
XMLToken_addNamespace (XMLToken_t *token, const char* uri, const char* prefix)
{
  token->addNamespace(uri, prefix);
}


/**
 * Removes an XML Namespace stored in the given position of the XMLNamespaces
 * of this XMLNode.
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, position of the removed namespace.
 */
LIBLAX_EXTERN
void 
XMLToken_removeNamespace (XMLToken_t *token, int index)
{
  token->removeNamespace(index);
}


/**
 * Removes an XML Namespace with the given prefix.
 *
 * @param token XMLToken_t structure to be queried.
 * @param prefix a string, prefix of the required namespace.
 */
LIBLAX_EXTERN
void 
XMLToken_removeNamespaceByPrefix (XMLToken_t *token, const char* prefix)
{
  token->removeNamespace(prefix);
}


/**
 * Clears (deletes) all XML namespace declarations in the XMLNamespaces 
 * of this XMLNode.
 *
 * @param token XMLToken_t structure to be queried.
 */
LIBLAX_EXTERN
void 
XMLToken_clearNamespaces (XMLToken_t *token)
{
  token->clearNamespaces();
}


/**
 * Look up the index of an XML namespace declaration by URI.
 *
 * @param token XMLToken_t structure to be queried.
 * @param uri a string, uri of the required namespace.
 *
 * @return the index of the given declaration, or -1 if not present.
 */
LIBLAX_EXTERN
int 
XMLToken_getNamespaceIndex (const XMLToken_t *token, const char* uri)
{
  return token->getNamespaceIndex(uri);
}


/**
 * Look up the index of an XML namespace declaration by prefix.
 *
 * @param token XMLToken_t structure to be queried.
 * @param prefix a string, prefix of the required namespace.
 *
 * @return the index of the given declaration, or -1 if not present.
 */
LIBLAX_EXTERN
int 
XMLToken_getNamespaceIndexByPrefix (const XMLToken_t *token, const char* prefix)
{
  return token->getNamespaceIndexByPrefix(prefix);
}


/**
 * Returns the number of XML namespaces stored in the XMLNamespaces 
 * of this XMLNode.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the number of namespaces in this list.
 */
LIBLAX_EXTERN
int 
XMLToken_getNamespacesLength (const XMLToken_t *token)
{
  return token->getNamespacesLength();
}


/**
 * Look up the prefix of an XML namespace declaration by position.
 *
 * Callers should use getNamespacesLength() to find out how many 
 * namespaces are stored in the XMLNamespaces.
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, position of the removed namespace.
 * 
 * @return the prefix of an XML namespace declaration in the XMLNamespaces 
 * (by position).  
 *
 * @note returned char* should be freed with safe_free() by the caller.
 */
LIBLAX_EXTERN
char* 
XMLToken_getNamespacePrefix (const XMLToken_t *token, int index)
{
  const std::string str = token->getNamespacePrefix(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Look up the prefix of an XML namespace declaration by its URI.
 *
 * @param token XMLToken_t structure to be queried.
 * @param uri a string, uri of the required namespace.
 *
 * @return the prefix of an XML namespace declaration given its URI.  
 *
 * @note returned char* should be freed with safe_free() by the caller.
 */
LIBLAX_EXTERN
char* 
XMLToken_getNamespacePrefixByURI (const XMLToken_t *token, const char* uri)
{
  const std::string str = token->getNamespacePrefix(uri);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Look up the URI of an XML namespace declaration by its position.
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, position of the removed namespace.
 *
 * @return the URI of an XML namespace declaration in the XMLNamespaces
 * (by position).  
 *
 * @note returned char* should be freed with safe_free() by the caller.
 */
LIBLAX_EXTERN
char* 
XMLToken_getNamespaceURI (const XMLToken_t *token, int index)
{
  const std::string str = token->getNamespaceURI(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Look up the URI of an XML namespace declaration by its prefix.
 *
 * @param token XMLToken_t structure to be queried.
 * @param prefix a string, prefix of the required namespace.
 *
 * @return the URI of an XML namespace declaration given its prefix.  
 *
 * @note returned char* should be freed with safe_free() by the caller.
 */
LIBLAX_EXTERN
char* 
XMLToken_getNamespaceURIByPrefix (const XMLToken_t *token, const char* prefix)
{
  const std::string str = token->getNamespaceURI(prefix);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * the XMLNamespaces of this XMLToken is empty.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c non-zero (true) if the XMLNamespaces of this XMLToken is empty, 
 * @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isNamespacesEmpty (const XMLToken_t *token)
{
  return token->isNamespacesEmpty();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * an XML Namespace with the given URI is contained in the XMLNamespaces of
 * this XMLToken.
 * 
 * @param token XMLToken_t structure to be queried.
 * @param uri a string, the uri for the namespace
 *
 * @return @c no-zero (true) if an XML Namespace with the given URI is 
 * contained in the XMLNamespaces of this XMLToken,  @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_hasNamespaceURI(const XMLToken_t *token, const char* uri)
{
  return token->hasNamespaceURI(uri);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * an XML Namespace with the given prefix is contained in the XMLNamespaces of
 * this XMLToken.
 *
 * @param token XMLToken_t structure to be queried.
 * @param prefix a string, the prefix for the namespace
 * 
 * @return @c no-zero (true) if an XML Namespace with the given URI is 
 * contained in the XMLNamespaces of this XMLToken, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_hasNamespacePrefix(const XMLToken_t *token, const char* prefix)
{
  return token->hasNamespacePrefix(prefix);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * an XML Namespace with the given uri/prefix pair is contained in the 
 * XMLNamespaces ofthis XMLToken.
 *
 * @param token XMLToken_t structure to be queried.
 * @param uri a string, the uri for the namespace
 * @param prefix a string, the prefix for the namespace
 * 
 * @return @c non-zero (true) if an XML Namespace with the given uri/prefix pair is 
 * contained in the XMLNamespaces of this XMLToken,  @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_hasNamespaceNS(const XMLToken_t *token, const char* uri, const char* prefix)
{
  return token->hasNamespaceNS(uri, prefix);
}



/**
 * Returns the (unqualified) name of this XML element.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the (unqualified) name of this XML element.
 */
LIBLAX_EXTERN
const char *
XMLToken_getName (const XMLToken_t *token)
{
  return token->getName().empty() ? NULL : token->getName().c_str();
}


/**
 * Returns the namespace prefix of this XML element.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the namespace prefix of this XML element.  
 *
 * @note If no prefix
 * exists, an empty string will be return.
 */
LIBLAX_EXTERN
const char *
XMLToken_getPrefix (const XMLToken_t *token)
{
  return token->getPrefix().empty() ? NULL : token->getPrefix().c_str();
}


/**
 * Returns the namespace URI of this XML element.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the namespace URI of this XML element.
 */
LIBLAX_EXTERN
const char *
XMLToken_getURI (const XMLToken_t *token)
{
  return token->getURI().empty() ? NULL : token->getURI().c_str();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLToken_t structure is an XML element.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLToken_t structure is an XML element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isElement (const XMLToken_t *token)
{
  return static_cast<int>( token->isElement() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLToken_t structure is an XML end element.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLToken_t structure is an XML end element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isEnd (const XMLToken_t *token) 
{
  return static_cast<int>( token->isEnd() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLToken_t structure is an XML end element for the given start element.
 * 
 * @param token XMLToken_t structure to be queried.
 * @param element XMLToken_t structure, element for which query is made.
 *
 * @return @c non-zero (true) if this XMLToken_t structure is an XML end element for the given
 * XMLToken_t structure start element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isEndFor (const XMLToken_t *token, const XMLToken_t *element)
{
  return static_cast<int>( token->isEndFor(*element) );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLToken_t structure is an end of file marker.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLToken_t structure is an end of file (input) marker, @c zero (false)
 * otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isEOF (const XMLToken_t *token)
{
  return static_cast<int>( token->isEOF() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLToken_t structure is an XML start element.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c true if this XMLToken_t structure is an XML start element, @c false otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isStart (const XMLToken_t *token)
{
  return static_cast<int>( token->isStart() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLToken_t structure is an XML text element.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLToken_t structure is an XML text element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isText (const XMLToken_t *token)
{
  return static_cast<int>( token->isText() );
}


/**
 * Declares this XML start element is also an end element.
 *
 * @param token XMLToken_t structure to be set.
 *
 */
LIBLAX_EXTERN
void
XMLToken_setEnd (XMLToken_t *token)
{
  token->setEnd();
}


/**
 * Declares this XMLToken_t structure is an end-of-file (input) marker.
 *
 * @param token XMLToken_t structure to be set.
 *
 */
LIBLAX_EXTERN
void
XMLToken_setEOF (XMLToken_t *token)
{
  token->setEOF();
}


/*
 * Declares this XML start/end element is no longer an end element.
 *
 * @param token XMLToken_t structure to be set.
 *
 */
LIBLAX_EXTERN
void
XMLToken_unsetEnd (XMLToken_t *token)
{
  token->unsetEnd();
}

/** @endcond doxygen-c-only */
