/**
 * \file    XMLNode.cpp
 * \brief   A node in an XML document tree
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


#include "XMLInputStream.h"
#include "XMLOutputStream.h"

#include "XMLNode.h"


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
 * Creates a new empty XMLNode with no children.
 */
XMLNode::XMLNode ()
{
}


/**
 * Destroys this XMLNode.
 */
XMLNode::~XMLNode ()
{
}


/**
 * Creates a new XMLNode by copying token.
 */
XMLNode::XMLNode (const XMLToken& token) : XMLToken(token)
{
}


/**
 * Creates a new XMLNode by reading XMLTokens from stream.  The stream must
 * be positioned on a start element (stream.peek().isStart() == true) and
 * will be read until the matching end element is found.
 */
XMLNode::XMLNode (XMLInputStream& stream) : XMLToken( stream.next() )
{
  if ( isEnd() ) return;

  std::string s;

  while ( stream.isGood() )
  {
    const XMLToken& next = stream.peek();


    if ( next.isStart() )
    {
      addChild( XMLNode(stream) );
    }
    else if ( next.isText() )
    {
      s = trim(next.getCharacters());
      if (s != "")
        addChild( stream.next() );
      else
        stream.skipText();
    }
    else if ( next.isEnd() )
    {
      stream.next();
      break;
    }
  }
}


/**
 * Adds a copy of child node to this XMLNode.
 */
void
XMLNode::addChild (const XMLNode& node)
{
  mChildren.push_back(node);
}


/**
 * Returns the nth child of this XMLNode.
 */
const XMLNode&
XMLNode::getChild (unsigned int n) const
{
  return mChildren[n];
}


/**
 * @return the number of children for this XMLNode.
 */
unsigned int
XMLNode::getNumChildren () const
{
  return mChildren.size();
}


/**
 * Writes this XMLNode and its children to stream.
 */
void
XMLNode::write (XMLOutputStream& stream) const
{
  unsigned int children = getNumChildren();

  XMLToken::write(stream);

  if (children > 0)
  {
    for (unsigned int c = 0; c < children; ++c) stream << getChild(c);
    stream.endElement( mTriple );
  }
}


/**
 * Inserts this XMLNode and its children into stream.
 */
LIBLAX_EXTERN
XMLOutputStream& operator<< (XMLOutputStream& stream, const XMLNode& node)
{
  node.write(stream);
  return stream;
}
