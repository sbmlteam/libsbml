/**
 * @file    XMLNode.cpp
 * @brief   A node in an XML document tree
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

#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/xml/XMLNode.h>

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
 * Copy constructor; creates a copy of this XMLNode.
 */
XMLNode::XMLNode(const XMLNode& orig):
      XMLToken (orig)
{
  this->mChildren.assign( orig.mChildren.begin(), orig.mChildren.end() ); 
}


 /**
  * Assignment operator for XMLNode.
  */
XMLNode& 
XMLNode::operator=(const XMLNode& orig)
{
  this->XMLToken::operator=(orig);
  this->mChildren.assign( orig.mChildren.begin(), orig.mChildren.end() ); 
  return *this;
}

/**
 * Creates and returns a deep copy of this XMLNode.
 * 
 * @return a (deep) copy of this XMLNode.
 */
XMLNode* 
XMLNode::clone () const
{
  return new XMLNode(*this);
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
  if (n < getNumChildren())
  {
    return mChildren[n];
  }
  else
  {
    return *(new XMLNode());
  }
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


/** @cond doxygen-c-only */


/**
 * Creates a new empty XMLNode_t structure with no children
 * and returns a pointer to it.
 *
 * @return pointer to the new XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_create (void)
{
  return new(nothrow) XMLNode;
}


/**
 * Creates a new XMLNode_t structure by copying token and returns a pointer
 * to it.
 *
 * @param token XMLToken_t structure to be copied to XMLNode_t structure.
 *
 * @return pointer to the new XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_createFromToken (const XMLToken_t *token)
{
  return new(nothrow) XMLNode(*token);
}

#if 0

/**
 * Creates a new XMLNode_t structure by reading XMLTokens from stream.  
 *
 * The stream must
 * be positioned on a start element (stream.peek().isStart() == true) and
 * will be read until the matching end element is found.
 *
 * @param stream XMLInputStream from which XMLNode_t structure is to be created.
 *
 * @return pointer to the new XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_createFromStream (XMLInputStream_t *stream)
{
  return new(nothrow) XMLNode(stream);
}

#endif


/**
 * Destroys this XMLNode_t structure.
 *
 * @param node XMLNode_t structure to be freed.
 */
LIBLAX_EXTERN
void
XMLNode_free (XMLNode_t *node)
{
  delete static_cast<XMLNode*>(node);
}


/**
 * Adds a copy of child node to this XMLNode_t structure.
 *
 * @param node XMLNode_t structure to which child is to be added.
 * @param child XMLNode_t structure to be added as child.
 */
LIBLAX_EXTERN
void
XMLNode_addChild (XMLNode_t *node, const XMLNode_t *child)
{
  node->addChild(*child);
}

/**
 * Returns the attributes of this element.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the XMLAttributes_t of this XML element.
 */
LIBLAX_EXTERN
const XMLAttributes_t *
XMLNode_getAttributes (const XMLNode_t *node)
{
  return &(node->getAttributes());
}


/**
 * Returns the text of this element.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the characters of this XML text.
 */
LIBLAX_EXTERN
const char *
XMLNode_getCharacters (const XMLNode_t *node)
{
  return node->getCharacters().empty() ? NULL : node->getCharacters().c_str();
}

/**
 * Returns the XML namespace declarations for this XML element.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the XML namespace declarations for this XML element.
 */
LIBLAX_EXTERN
const XMLNamespaces_t *
XMLNode_getNamespaces (const XMLNode_t *node)
{
  return &(node->getNamespaces());
}


/**
 * Returns the (unqualified) name of this XML element.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the (unqualified) name of this XML element.
 */
LIBLAX_EXTERN
const char *
XMLNode_getName (const XMLNode_t *node)
{
  return node->getName().empty() ? NULL : node->getName().c_str();
}


/**
 * Returns the namespace prefix of this XML element.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the namespace prefix of this XML element.  
 *
 * @note If no prefix
 * exists, an empty string will be return.
 */
LIBLAX_EXTERN
const char *
XMLNode_getPrefix (const XMLNode_t *node)
{
  return node->getPrefix().empty() ? NULL : node->getPrefix().c_str();
}


/**
 * Returns the namespace URI of this XML element.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the namespace URI of this XML element.
 */
LIBLAX_EXTERN
const char *
XMLNode_getURI (const XMLNode_t *node)
{
  return node->getURI().empty() ? NULL : node->getURI().c_str();
}


/**
 * Returns the nth child of this XMLNode_t structure.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the nth child of this XMLNode_t structure.
 */
LIBLAX_EXTERN
const XMLNode_t *
XMLNode_getChild (const XMLNode_t *node, const int n)
{
  return &(node->getChild(n));
}


/**
 * Returns the number of children for this XMLNode_t structure.
 *
 * @param node XMLNode_t structure to be queried.
 *
 * @return the number of children for this XMLNode_t structure.
 */
LIBLAX_EXTERN
unsigned int
XMLNode_getNumChildren (const XMLNode_t *node)
{
  return node->getNumChildren();
}





/** @endcond doxygen-c-only */
