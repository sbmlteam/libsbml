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
 * Copyright 2005-2009 California Institute of Technology.
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
  if(&orig!=this)
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
 * @return the XMLAttributes of this XML element.
 */
const XMLAttributes&
XMLToken::getAttributes () const
{
  return mAttributes;
}


/*
 * Sets an XMLAttributes to this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param attributes XMLAttributes to be set to this XMLToken.
 *
 * @note This function replaces the existing XMLAttributes with the new one.
 */
void 
XMLToken::setAttributes(const XMLAttributes& attributes)
{
  if (mIsStart)
  {
    mAttributes = attributes;
  }
}


/*
 * Adds an attribute to the attribute set in this XMLToken optionally 
 * with a prefix and URI defining a namespace.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param name a string, the local name of the attribute.
 * @param value a string, the value of the attribute.
 * @param namespaceURI a string, the namespace URI of the attribute.
 * @param prefix a string, the prefix of the namespace
 *
 * @note if local name with the same namespace URI already exists in the
 * attribute set, its value and prefix will be replaced.
 *
 * @docnote The native C++ implementation of this method defines a
 * default argument value.  In the documentation generated for different
 * libSBML language bindings, you may or may not see corresponding
 * arguments in the method declarations.  For example, in Java, a default
 * argument is handled by declaring two separate methods, with one of
 * them having the argument and the other one lacking the argument.
 * However, the libSBML documentation will be @em identical for both
 * methods.  Consequently, if you are reading this and do not see an
 * argument even though one is described, please look for descriptions of
 * other variants of this method near where this one appears in the
 * documentation.
 */
void 
XMLToken::addAttr (  const std::string& name
	           , const std::string& value
    	           , const std::string& namespaceURI
	           , const std::string& prefix      )
{
  if (mIsStart) mAttributes.add(name, value, namespaceURI, prefix);
}


/*
 * Adds an attribute with the given XMLTriple/value pair to the attribute set
 * in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @note if local name with the same namespace URI already exists in the 
 * attribute set, its value and prefix will be replaced.
 *
 * @param triple an XMLTriple, the XML triple of the attribute.
 * @param value a string, the value of the attribute.
 */
void 
XMLToken::addAttr ( const XMLTriple& triple, const std::string& value)
{
  if (mIsStart) mAttributes.add(triple, value);
}


/*
 * Removes an attribute with the given index from the attribute set in
 * this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param n an integer the index of the resource to be deleted
 */
void 
XMLToken::removeAttr (int n)
{
  if (mIsStart) mAttributes.remove(n);
}


/*
 * Removes an attribute with the given local name and namespace URI from 
 * the attribute set in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param name   a string, the local name of the attribute.
 * @param uri    a string, the namespace URI of the attribute.
 */
void 
XMLToken::removeAttr (const std::string& name, const std::string& uri)
{
  if (mIsStart) mAttributes.remove(name, uri);
}


/*
 * Removes an attribute with the given XMLTriple from the attribute set 
 * in this XMLToken.  
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param triple an XMLTriple, the XML triple of the attribute.
 */
void 
XMLToken::removeAttr (const XMLTriple& triple)
{
  if (mIsStart) mAttributes.remove(triple);
}


/*
 * Clears (deletes) all attributes in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 */
void 
XMLToken::clearAttributes()
{
  if (mIsStart) mAttributes.clear();
}


/*
 * Return the index of an attribute with the given local name and namespace URI.
 *
 * @param name a string, the local name of the attribute.
 * @param uri  a string, the namespace URI of the attribute.
 *
 * @return the index of an attribute with the given local name and namespace URI, 
 * or -1 if not present.
 *
 * @docnote The native C++ implementation of this method defines a
 * default argument value.  In the documentation generated for different
 * libSBML language bindings, you may or may not see corresponding
 * arguments in the method declarations.  For example, in Java, a default
 * argument is handled by declaring two separate methods, with one of
 * them having the argument and the other one lacking the argument.
 * However, the libSBML documentation will be @em identical for both
 * methods.  Consequently, if you are reading this and do not see an
 * argument even though one is described, please look for descriptions of
 * other variants of this method near where this one appears in the
 * documentation.
 */
int 
XMLToken::getAttrIndex (const std::string& name, const std::string& uri) const
{
  return mAttributes.getIndex(name, uri);
}


/*
 * Return the index of an attribute with the given XMLTriple.
 *
 * @param triple an XMLTriple, the XML triple of the attribute for which 
 *        the index is required.
 *
 * @return the index of an attribute with the given XMLTriple, or -1 if not present.
 */
int 
XMLToken::getAttrIndex (const XMLTriple& triple) const
{
  return mAttributes.getIndex(triple);
}


/*
 * Return the number of attributes in the attributes set.
 *
 * @return the number of attributes in the attributes set in this XMLToken.
 */
int 
XMLToken::getAttributesLength () const
{
  return mAttributes.getLength();
}


/*
 * Return the local name of an attribute in the attributes set in this 
 * XMLToken (by position).
 *
 * @param index an integer, the position of the attribute whose local name 
 * is required.
 *
 * @return the local name of an attribute in this list (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned.  Use hasAttr(index) 
 * to test for the attribute existence.
 */
std::string 
XMLToken::getAttrName (int index) const
{
  return mAttributes.getName(index);
}


/*
 * Return the prefix of an attribute in the attribute set in this 
 * XMLToken (by position).
 *
 * @param index an integer, the position of the attribute whose prefix is 
 * required.
 *
 * @return the namespace prefix of an attribute in the attribute set
 * (by position).  
 *
 * @note If index is out of range, an empty string will be
 * returned. Use hasAttr(index) to test for the attribute existence.
 */
std::string 
XMLToken::getAttrPrefix (int index) const
{
  return mAttributes.getPrefix(index);
}


/*
 * Return the prefixed name of an attribute in the attribute set in this 
 * XMLToken (by position).
 *
 * @param index an integer, the position of the attribute whose prefixed 
 * name is required.
 *
 * @return the prefixed name of an attribute in the attribute set 
 * (by position).  
 *
 * @note If index is out of range, an empty string will be
 * returned.  Use hasAttr(index) to test for attribute existence.
 */
std::string 
XMLToken::getAttrPrefixedName (int index) const
{
  return mAttributes.getPrefixedName(index);
}


/*
 * Return the namespace URI of an attribute in the attribute set in this 
 * XMLToken (by position).
 *
 * @param index an integer, the position of the attribute whose namespace 
 * URI is required.
 *
 * @return the namespace URI of an attribute in the attribute set (by position).
 *
 * @note If index is out of range, an empty string will be returned.  Use
 * hasAttr(index) to test for attribute existence.
 */
std::string 
XMLToken::getAttrURI (int index) const
{
  return mAttributes.getURI(index);
}


/*
 * Return the value of an attribute in the attribute set in this XMLToken  
 * (by position).
 *
 * @param index an integer, the position of the attribute whose value is 
 * required.
 *
 * @return the value of an attribute in the attribute set (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned. Use hasAttr(index)
 * to test for attribute existence.
 */
std::string 
XMLToken::getAttrValue (int index) const
{
  return mAttributes.getValue(index);
}


/*
 * Return a value of an attribute with the given local name and namespace URI.
 *
 * @param name a string, the local name of the attribute whose value is required.
 * @param uri  a string, the namespace URI of the attribute.
 *
 * @return The attribute value as a string.  
 *
 * @note If an attribute with the 
 * given local name and namespace URI does not exist, an empty string will be 
 * returned.  
 * Use hasAttr(name, uri) to test for attribute existence.
 *
 * @docnote The native C++ implementation of this method defines a
 * default argument value.  In the documentation generated for different
 * libSBML language bindings, you may or may not see corresponding
 * arguments in the method declarations.  For example, in Java, a default
 * argument is handled by declaring two separate methods, with one of
 * them having the argument and the other one lacking the argument.
 * However, the libSBML documentation will be @em identical for both
 * methods.  Consequently, if you are reading this and do not see an
 * argument even though one is described, please look for descriptions of
 * other variants of this method near where this one appears in the
 * documentation.
 */
std::string 
XMLToken::getAttrValue (const std::string name, const std::string uri) const
{
  return mAttributes.getValue(name, uri);
}


/*
 * Return a value of an attribute with the given XMLTriple.
 *
 * @param triple an XMLTriple, the XML triple of the attribute whose 
 *        value is required.
 *
 * @return The attribute value as a string.  
 *
 * @note If an attribute with the
 * given XMLTriple does not exist, an empty string will be returned.  
 * Use hasAttr(triple) to test for attribute existence.
 */
std::string 
XMLToken::getAttrValue (const XMLTriple& triple) const
{
  return mAttributes.getValue(triple);
}


/*
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given index exists in the attribute set in this 
 * XMLToken.
 *
 * @param index an integer, the position of the attribute.
 *
 * @return @c true if an attribute with the given index exists in the attribute 
 * set in this XMLToken, @c false otherwise.
 */
bool 
XMLToken::hasAttr (int index) const
{
  return mAttributes.hasAttribute(index);
}


/*
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given local name and namespace URI exists 
 * in the attribute set in this XMLToken.
 *
 * @param name a string, the local name of the attribute.
 * @param uri  a string, the namespace URI of the attribute.
 *
 * @return @c true if an attribute with the given local name and namespace 
 * URI exists in the attribute set in this XMLToken, @c false otherwise.
 *
 * @docnote The native C++ implementation of this method defines a
 * default argument value.  In the documentation generated for different
 * libSBML language bindings, you may or may not see corresponding
 * arguments in the method declarations.  For example, in Java, a default
 * argument is handled by declaring two separate methods, with one of
 * them having the argument and the other one lacking the argument.
 * However, the libSBML documentation will be @em identical for both
 * methods.  Consequently, if you are reading this and do not see an
 * argument even though one is described, please look for descriptions of
 * other variants of this method near where this one appears in the
 * documentation.
 */
bool 
XMLToken::hasAttr (const std::string name, const std::string uri) const
{
  return mAttributes.hasAttribute(name, uri);
}


/*
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given XML triple exists in the attribute set in 
 * this XMLToken 
 *
 * @param triple an XMLTriple, the XML triple of the attribute 
 *
 * @return @c true if an attribute with the given XML triple exists
 * in the attribute set in this XMLToken, @c false otherwise.
 *
 */
bool 
XMLToken::hasAttr (const XMLTriple& triple) const
{
  return mAttributes.hasAttribute(triple);
}


/*
 * Predicate returning @c true or @c false depending on whether 
 * the attribute set in this XMLToken set is empty.
 * 
 * @return @c true if the attribute set in this XMLToken is empty, 
 * @c false otherwise.
 */
bool 
XMLToken::isAttributesEmpty () const
{
  return mAttributes.isEmpty();
}




/*
 * @return the XML namespace declarations for this XML element.
 */
const XMLNamespaces&
XMLToken::getNamespaces () const
{
  return mNamespaces;
}


/*
 * Sets an XMLnamespaces to this XML element.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @note This function replaces the existing XMLNamespaces with the new one.
 */
void 
XMLToken::setNamespaces(const XMLNamespaces& namespaces)
{
  if (mIsStart)
  {
    mNamespaces = namespaces;
  }
}


/*
 * Appends an XML namespace prefix and URI pair to this XMLToken.
 * If there is an XML namespace with the given prefix in this XMLToken, 
 * then the existing XML namespace will be overwritten by the new one.
 *
 * Nothing will be done if this XMLToken is not a start element.
 */
void 
XMLToken::addNamespace (const std::string& uri, const std::string& prefix)
{
   if (mIsStart)  mNamespaces.add(uri, prefix);
}


/*
 * Removes an XML Namespace stored in the given position of the XMLNamespaces
 * of this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param index an integer, position of the removed namespace.
 */
void 
XMLToken::removeNamespace (int index)
{
   if (mIsStart) mNamespaces.remove(index);
}


/*
 * Removes an XML Namespace with the given prefix.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param prefix a string, prefix of the required namespace.
 */
void 
XMLToken::removeNamespace (const std::string& prefix)
{
   if (mIsStart)  mNamespaces.remove(prefix);
}


/*
 * Clears (deletes) all XML namespace declarations in the XMLNamespaces of
 * this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 */
void 
XMLToken::clearNamespaces ()
{
   if (mIsStart) mNamespaces.clear();
}


/*
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


/*
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


/*
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


/*
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


/*
 * Look up the prefix of an XML namespace declaration by its URI.
 *
 * @return the prefix of an XML namespace declaration given its URI.  
 */
std::string 
XMLToken::getNamespacePrefix (const std::string& uri) const
{
  return mNamespaces.getPrefix(uri);
}


/*
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


/*
 * Look up the URI of an XML namespace declaration by its prefix.
 *
 * @return the URI of an XML namespace declaration given its prefix.  
 */
std::string 
XMLToken::getNamespaceURI (const std::string& prefix) const
{
  return mNamespaces.getURI(prefix);
}


/*
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


/*
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


/*
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


/*
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
 * Sets the XMLTripe (name, uri and prefix) of this XML element.
 * Nothing will be done if this XML element is a text node.
 */
void 
XMLToken::setTriple(const XMLTriple& triple)
{
  if (! mIsText ) 
  {
     mTriple = triple;
  }
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
 * Sets an XMLAttributes to this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure to be set.
 * @param attributes XMLAttributes to be set to this XMLToken.
 *
 * @note This function replaces the existing XMLAttributes with the new one.
 */
LIBLAX_EXTERN
void 
XMLToken_setAttributes(XMLToken_t *token, const XMLAttributes_t* attributes)
{
  if(!attributes)
  {
    return;
  }

  token->setAttributes(*attributes);
}


/**
 * Adds an attribute with the given local name to the attribute set in this XMLToken.
 * (namespace URI and prefix are empty)
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure to which an attribute to be added.
 * @param name a string, the local name of the attribute.
 * @param value a string, the value of the attribute.
 *
 * @note if the local name without namespace URI already exists in the
 * attribute set, its value will be replaced.
 *
 */
LIBLAX_EXTERN
void 
XMLToken_addAttr ( XMLToken_t *token,  const char* name, const char* value )
{
  token->addAttr(name, value, "", "");
}


/**
 * Adds an attribute with a prefix and namespace URI to the attribute set 
 * in this XMLToken optionally 
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure to which an attribute to be added.
 * @param name a string, the local name of the attribute.
 * @param value a string, the value of the attribute.
 * @param namespaceURI a string, the namespace URI of the attribute.
 * @param prefix a string, the prefix of the namespace
 *
 * @note if local name with the same namespace URI already exists in the
 * attribute set, its value and prefix will be replaced.
 *
 */
LIBLAX_EXTERN
void 
XMLToken_addAttrWithNS ( XMLToken_t *token,  const char* name
	                , const char* value
    	                , const char* namespaceURI
	                , const char* prefix      )
{
  token->addAttr(name, value, namespaceURI, prefix);
}



/**
 * Adds an attribute with the given XMLTriple/value pair to the attribute set
 * in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @note if local name with the same namespace URI already exists in the 
 * attribute set, its value and prefix will be replaced.
 *
 * @param token XMLToken_t structure to which an attribute to be added.
 * @param triple an XMLTriple, the XML triple of the attribute.
 * @param value a string, the value of the attribute.
 */
LIBLAX_EXTERN
void 
XMLToken_addAttrWithTriple (XMLToken_t *token, const XMLTriple_t *triple, const char* value)
{
  token->addAttr(*triple, value);
}


/**
 * Removes an attribute with the given index from the attribute set in
 * this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure from which an attribute to be removed.
 * @param n an integer the index of the resource to be deleted
 */
LIBLAX_EXTERN
void 
XMLToken_removeAttr (XMLToken_t *token, int n)
{
  token->removeAttr(n);
}


/**
 * Removes an attribute with the given local name (without namespace URI) 
 * from the attribute set in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure from which an attribute to be removed.
 * @param name   a string, the local name of the attribute.
 * @param uri    a string, the namespace URI of the attribute.
 */
LIBLAX_EXTERN
void 
XMLToken_removeAttrByName (XMLToken_t *token, const char* name)
{
  token->removeAttr(name, "");
}


/**
 * Removes an attribute with the given local name and namespace URI from 
 * the attribute set in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure from which an attribute to be removed.
 * @param name   a string, the local name of the attribute.
 * @param uri    a string, the namespace URI of the attribute.
 */
LIBLAX_EXTERN
void 
XMLToken_removeAttrByNS (XMLToken_t *token, const char* name, const char* uri)
{
  token->removeAttr(name, uri);
}


/**
 * Removes an attribute with the given XMLTriple from the attribute set 
 * in this XMLToken.  
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure from which an attribute to be removed.
 * @param triple an XMLTriple, the XML triple of the attribute.
 */
LIBLAX_EXTERN
void 
XMLToken_removeAttrByTriple (XMLToken_t *token, const XMLTriple_t *triple)
{
  token->removeAttr(*triple);
}


/**
 * Clears (deletes) all attributes in this XMLToken.
 * Nothing will be done if this XMLToken is not a start element.
 *
 * @param token XMLToken_t structure from which attributes to be removed.
 */
LIBLAX_EXTERN
void 
XMLToken_clearAttributes(XMLToken_t *token)
{
  token->clearAttributes();
}



/**
 * Return the index of an attribute with the given local name and namespace URI.
 *
 * @param token XMLToken_t structure to be queried.
 * @param name a string, the local name of the attribute.
 * @param uri  a string, the namespace URI of the attribute.
 *
 * @return the index of an attribute with the given local name and namespace URI, 
 * or -1 if not present.
 *
 */
LIBLAX_EXTERN
int 
XMLToken_getAttrIndex (const XMLToken_t *token, const char* name, const char* uri)
{
  return token->getAttrIndex(name, uri);
}


/**
 * Return the index of an attribute with the given XMLTriple.
 *
 * @param token XMLToken_t structure to be queried.
 * @param triple an XMLTriple, the XML triple of the attribute for which 
 *        the index is required.
 *
 * @return the index of an attribute with the given XMLTriple, or -1 if not present.
 */
LIBLAX_EXTERN
int 
XMLToken_getAttrIndexByTriple (const XMLToken_t *token, const XMLTriple_t *triple)
{
  return token->getAttrIndex(*triple);
}


/**
 * Return the number of attributes in the attributes set.
 *
 * @param token XMLToken_t structure to be queried.
 *
 * @return the number of attributes in the attributes set in this XMLToken.
 */
LIBLAX_EXTERN
int 
XMLToken_getAttributesLength (const XMLToken_t *token)
{
  return token->getAttributesLength();
}


/**
 * Return the local name of an attribute in the attributes set in this 
 * XMLToken (by position).
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, the position of the attribute whose local name 
 * is required.
 *
 * @return the local name of an attribute in this list (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned.  Use XMLToken_hasAttr(...) 
 * to test for the attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrName (const XMLToken_t *token, int index)
{
  const std::string str = token->getAttrName(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Return the prefix of an attribute in the attribute set in this 
 * XMLToken (by position).
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, the position of the attribute whose prefix is 
 * required.
 *
 * @return the namespace prefix of an attribute in the attribute set
 * (by position).  
 *
 * @note If index is out of range, an empty string will be
 * returned. Use XMLToken_hasAttr(...) to test for the attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrPrefix (const XMLToken_t *token, int index)
{
  const std::string str = token->getAttrPrefix(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Return the prefixed name of an attribute in the attribute set in this 
 * XMLToken (by position).
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, the position of the attribute whose prefixed 
 * name is required.
 *
 * @return the prefixed name of an attribute in the attribute set 
 * (by position).  
 *
 * @note If index is out of range, an empty string will be
 * returned.  Use XMLToken_hasAttr(...) to test for attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrPrefixedName (const XMLToken_t *token, int index)
{
  const std::string str = token->getAttrPrefixedName(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Return the namespace URI of an attribute in the attribute set in this 
 * XMLToken (by position).
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, the position of the attribute whose namespace 
 * URI is required.
 *
 * @return the namespace URI of an attribute in the attribute set (by position).
 *
 * @note If index is out of range, an empty string will be returned.  Use
 * XMLToken_hasAttr(index) to test for attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrURI (const XMLToken_t *token, int index)
{
  const std::string str = token->getAttrURI(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Return the value of an attribute in the attribute set in this XMLToken  
 * (by position).
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, the position of the attribute whose value is 
 * required.
 *
 * @return the value of an attribute in the attribute set (by position).  
 *
 * @note If index
 * is out of range, an empty string will be returned. Use XMLToken_hasAttr(...)
 * to test for attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrValue (const XMLToken_t *token, int index)
{
  const std::string str = token->getAttrValue(index);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}



/**
 * Return a value of an attribute with the given local name (without namespace URI).
 *
 * @param token XMLToken_t structure to be queried.
 * @param name a string, the local name of the attribute whose value is required.
 *
 * @return The attribute value as a string.  
 *
 * @note If an attribute with the given local name (without namespace URI) 
 * does not exist, an empty string will be returned.  
 * Use XMLToken_hasAttr(...) to test for attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrValueByName (const XMLToken_t *token, const char* name)
{
  const std::string str = token->getAttrValue(name, "");

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Return a value of an attribute with the given local name and namespace URI.
 *
 * @param token XMLToken_t structure to be queried.
 * @param name a string, the local name of the attribute whose value is required.
 * @param uri  a string, the namespace URI of the attribute.
 *
 * @return The attribute value as a string.  
 *
 * @note If an attribute with the 
 * given local name and namespace URI does not exist, an empty string will be 
 * returned.  
 * Use XMLToken_hasAttr(name, uri) to test for attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrValueByNS (const XMLToken_t *token, const char* name, const char* uri)
{
  const std::string str = token->getAttrValue(name, uri);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Return a value of an attribute with the given XMLTriple.
 *
 * @param token XMLToken_t structure to be queried.
 * @param triple an XMLTriple, the XML triple of the attribute whose 
 *        value is required.
 *
 * @return The attribute value as a string.  
 *
 * @note If an attribute with the
 * given XMLTriple does not exist, an empty string will be returned.  
 * Use XMLToken_hasAttr(...) to test for attribute existence.
 */
LIBLAX_EXTERN
char* 
XMLToken_getAttrValueByTriple (const XMLToken_t *token, const XMLTriple_t *triple)
{
  const std::string str = token->getAttrValue(*triple);

  if (str.empty())
    return NULL;
  else
    return safe_strdup(str.c_str());
}


/**
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given index exists in the attribute set in this 
 * XMLToken.
 *
 * @param token XMLToken_t structure to be queried.
 * @param index an integer, the position of the attribute.
 *
 * @return @c non-zero (true) if an attribute with the given index exists in 
 * the attribute set in this XMLToken, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_hasAttr (const XMLToken_t *token, int index)
{
  return token->hasAttr(index);
}


/**
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given local name (without namespace URI) 
 * exists in the attribute set in this XMLToken.
 *
 * @param token XMLToken_t structure to be queried.
 * @param name a string, the local name of the attribute.
 *
 * @return @c non-zero (true) if an attribute with the given local name 
 * (without namespace URI) exists in the attribute set in this XMLToken, 
 * @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_hasAttrWithName (const XMLToken_t *token, const char* name)
{
  return token->hasAttr(name, "");
}


/**
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given local name and namespace URI exists 
 * in the attribute set in this XMLToken.
 *
 * @param token XMLToken_t structure to be queried.
 * @param name a string, the local name of the attribute.
 * @param uri  a string, the namespace URI of the attribute.
 *
 * @return @c non-zero (true) if an attribute with the given local name 
 * and namespace URI exists in the attribute set in this XMLToken, 
 * @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_hasAttrWithNS (const XMLToken_t *token, const char* name, const char* uri)
{
  return token->hasAttr(name, uri);
}


/**
 * Predicate returning @c true or @c false depending on whether
 * an attribute with the given XML triple exists in the attribute set in 
 * this XMLToken 
 *
 * @param token XMLToken_t structure to be queried.
 * @param triple an XMLTriple, the XML triple of the attribute 
 *
 * @return @c non-zero (true) if an attribute with the given XML triple exists
 * in the attribute set in this XMLToken, @c zero (false) otherwise.
 *
 */
LIBLAX_EXTERN
int
XMLToken_hasAttrWithTriple (const XMLToken_t *token, const XMLTriple_t *triple)
{
  return token->hasAttr(*triple);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * the attribute set in this XMLToken set is empty.
 * 
 * @param token XMLToken_t structure to be queried.
 *
 * @return @c non-zero (true) if the attribute set in this XMLToken is empty, 
 * @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLToken_isAttributesEmpty (const XMLToken_t *token)
{
  return token->isAttributesEmpty();
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
 * Nothing will be done if this XMLToken is not a start element.
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
 * Nothing will be done if this XMLToken is not a start element.
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
 * Nothing will be done if this XMLToken is not a start element.
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
 * Nothing will be done if this XMLToken is not a start element.
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
 * Nothing will be done if this XMLToken is not a start element.
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
   return static_cast<int>(token->isNamespacesEmpty());
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
  return static_cast<int>(token->hasNamespaceURI(uri));
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
  return static_cast<int>(token->hasNamespacePrefix(prefix));
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
  return static_cast<int>(token->hasNamespaceNS(uri, prefix));
}


/**
 * Sets the XMLTripe (name, uri and prefix) of this XML element.
 * Nothing will be done if this XML element is a text node.
 *
 * @param token XMLToken_t structure to be queried. 
 * @param triple an XMLTriple, the XML triple to be set to this XML element.
 */
LIBLAX_EXTERN
void 
XMLToken_setTriple(XMLToken_t *token, const XMLTriple_t *triple)
{
  if(triple) token->setTriple(*triple);
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
