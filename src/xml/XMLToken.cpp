/**
 * \file    XMLToken.cpp
 * \brief   A unit of XML syntax, either an XML element or text.
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


#include "XMLOutputStream.h"
#include "XMLToken.h"


using namespace std;


/**
 * Creates a new empty XMLToken.
 */
XMLToken::XMLToken () :
   mIsStart( false )
 , mIsEnd  ( false )
 , mIsText ( false )
 , mLine   ( 0     )
 , mColumn ( 0     )
{
}


/**
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


/**
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


/**
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


/**
 * Creates a text XMLToken.
 */
XMLToken::XMLToken (  const string&       chars
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


/**
 * Destroys this XMLToken.
 */
XMLToken::~XMLToken ()
{
}


/**
 * Appends characters to this XML text content.
 */
void
XMLToken::append (const string& chars)
{
  mChars.append(chars);
}


/**
 * @return the XMLAttributes of this XML element.
 */
const XMLAttributes&
XMLToken::getAttributes () const
{
  return mAttributes;
}


/**
 * @return the characters of this XML text.
 */
const string&
XMLToken::getCharacters () const
{
  return mChars;
} 


/**
 * @return the column at which this XMLToken occurred.
 */
unsigned int
XMLToken::getColumn () const
{
  return mColumn;
}


/**
 * @return the line at which this XMLToken occurred.
 */
unsigned int
XMLToken::getLine () const
{
  return mLine;
}


/**
 * @return the XML namespace declarations for this XML element.
 */
const XMLNamespaces&
XMLToken::getNamespaces () const
{
  return mNamespaces;
}


/**
 * @return the (unqualified) name of this XML element.
 */
const string&
XMLToken::getName () const
{
  return mTriple.getName();
}


/**
 * @return the namespace prefix of this XML element.  If no prefix
 * exists, an empty string will be return.
 */
const string&
XMLToken::getPrefix () const
{
  return mTriple.getPrefix();
}


/**
 * @return the namespace URI of this XML element.
 */
const string&
XMLToken::getURI () const
{
  return mTriple.getURI();
}


/**
 * @return true if this XMLToken is an XML element.
 */
bool
XMLToken::isElement () const
{
  return mIsStart || mIsEnd;
}

 
/**
 * @return true if this XMLToken is an XML end element, false
 * otherwise.
 */
bool
XMLToken::isEnd () const
{
  return mIsEnd;
}


/**
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


/**
 * @return true if this XMLToken is an end of file (input) marker, false
 * otherwise.
 */
bool
XMLToken::isEOF () const
{
  return (mIsStart == false && mIsEnd == false && mIsText == false);
}


/**
 * @return true if this XMLToken is an XML start element, false
 * otherwise.
 */
bool
XMLToken::isStart () const
{
  return mIsStart;
}


/**
 * @return true if this XMLToken is text, false otherwise.
 */
bool
XMLToken::isText () const
{
  return mIsText;
}


/**
 * Declares this XML start element is also an end element.
 */
void
XMLToken::setEnd ()
{
  mIsEnd = true;
}


/**
 * Declares this XMLToken is an end-of-file (input) marker.
 */
void
XMLToken::setEOF ()
{
  mIsStart = false;
  mIsEnd   = false;
  mIsText  = false;
}


/**
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


/**
 * Inserts this XMLToken into stream.
 */
XMLOutputStream&
operator<< (XMLOutputStream& stream, const XMLToken& token)
{
  token.write(stream);
  return stream;
}
