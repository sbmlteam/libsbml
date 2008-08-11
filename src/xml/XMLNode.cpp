/**
 * @file    XMLNode.cpp
 * @brief   A node in an XML document tree
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

#include <sbml/util/memory.h>
#include <sbml/util/util.h>

/** @cond doxygen-libsbml-internal */
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
/** @endcond doxygen-libsbml-internal */

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


/*
 * Creates a new empty XMLNode with no children.
 */
XMLNode::XMLNode ()
{
}


/*
 * Destroys this XMLNode.
 */
XMLNode::~XMLNode ()
{
}


/*
 * Creates a new XMLNode by copying token.
 */
XMLNode::XMLNode (const XMLToken& token) : XMLToken(token)
{
}


/**
 * Creates a new start element XMLNode with the given set of attributes and
 * namespace declarations.
 */
XMLNode::XMLNode (  const XMLTriple&     triple
                  , const XMLAttributes& attributes
                  , const XMLNamespaces& namespaces
		  , const unsigned int   line
                  , const unsigned int   column) 
                  : XMLToken(triple, attributes, namespaces, line, column)
{
}


/**
 * Creates a start element XMLNode with the given set of attributes.
 */
XMLNode::XMLNode (  const XMLTriple&      triple
                  , const XMLAttributes&  attributes
                  , const unsigned int    line
                  , const unsigned int    column )
                  : XMLToken(triple, attributes, line, column)
{
}  


/**
 * Creates an end element XMLNode with the given set of attributes.
 */
XMLNode::XMLNode (  const XMLTriple&   triple
                  , const unsigned int line
                  , const unsigned int column )
                  : XMLToken(triple, line, column)
{
}


/**
 * Creates a text XMLNode.
 */
XMLNode::XMLNode (  const std::string& chars
                  , const unsigned int line
                  , const unsigned int column )
                  : XMLToken(chars, line, column)
{
}


/** @cond doxygen-libsbml-internal */
/*
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
/** @endcond doxygen-libsbml-internal */


/*
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

/*
 * Creates and returns a deep copy of this XMLNode.
 * 
 * @return a (deep) copy of this XMLNode.
 */
XMLNode* 
XMLNode::clone () const
{
  return new XMLNode(*this);
}


/*
 * Adds a copy of child node to this XMLNode.
 */
void
XMLNode::addChild (const XMLNode& node)
{
  mChildren.push_back(node);
}


/*
 * Inserts a copy of child node as the nth child of this XMLNode.
 */
XMLNode&
XMLNode::insertChild (unsigned int n, const XMLNode& node)
{
  unsigned int size = mChildren.size();

  if ( (n >= size) || (size == 0) )
  {
    mChildren.push_back(node);
    return mChildren.back();
  }

  return *(mChildren.insert(mChildren.begin() + n, node));
}


/*
 * Returns the nth child of this XMLNode.
 */
XMLNode&
XMLNode::getChild (unsigned int n)
{
   return const_cast<XMLNode&>( 
            static_cast<const XMLNode&>(*this).getChild(n)
          );
}


/*
 * Returns the nth child of this XMLNode.
 */
const XMLNode&
XMLNode::getChild (unsigned int n) const
{
  unsigned int size = getNumChildren();
  if ( (n < size) && (size > 0) )
  {
    return mChildren[n];
  }
  else
  {
    // this should not happen, or memory leak occurs...
    // (I think some exception should be thrown here.)
    return *(new XMLNode());
  }
}


/*
 * @return the number of children for this XMLNode.
 */
unsigned int
XMLNode::getNumChildren () const
{
  return mChildren.size();
}


/** @cond doxygen-libsbml-internal */
/*
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
    if (!mTriple.isEmpty())
      stream.endElement( mTriple );
  }
  else if ( isStart() && !isEnd() ) 
  {
    stream.endElement( mTriple );
  }

}
/** @endcond doxygen-libsbml-internal */


/*
 * Returns a string which is converted from this XMLNode.
 */
std::string XMLNode::toXMLString() const
{
  std::ostringstream oss;
  XMLOutputStream xos(oss,"UTF-8",false);
  write(xos);

  return oss.str();
}


/*
 * Returns a string which is converted from a given XMLNode.
 */
std::string XMLNode::convertXMLNodeToString(const XMLNode* xnode)
{
  if(xnode == NULL) return "";

  std::ostringstream oss;
  XMLOutputStream xos(oss,"UTF-8",false);
  xnode->write(xos);

  return oss.str();
}


/*
 * Returns a XMLNode which is converted from a given string.
 */
XMLNode* XMLNode::convertStringToXMLNode(const std::string& xmlstr, const XMLNamespaces* xmlns)
{
  XMLNode* xmlnode     = NULL;
  std::ostringstream oss;
  const char* dummy_xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  const char* dummy_element_start = "<dummy";
  const char* dummy_element_end   = "</dummy>";


  oss << dummy_xml;
  oss << dummy_element_start;
  if(xmlns){
    for(int i=0; i < xmlns->getLength(); i++)
    {
      oss << " xmlns";
      if(xmlns->getPrefix(i) != "") oss << ":" << xmlns->getPrefix(i);
      oss << "=\"" << xmlns->getURI(i) << '"';
    }
  }
  oss << ">";
  oss << xmlstr;
  oss << dummy_element_end;


  const char* xmlstr_c = safe_strdup(oss.str().c_str());
  XMLInputStream xis(xmlstr_c,false);
  XMLNode* xmlnode_tmp = new XMLNode(xis);

  if(xis.isError() || (xmlnode_tmp->getNumChildren() == 0) )
  {
    delete xmlnode_tmp;
    return NULL;
  }


  /**
   * this is fine if the first child is a parent element
   * it actually falls down if all your elements have equal footing
   * eg 
   *  <p>The following is MathML markup:</p>
   *  <p xmlns="http://www.w3.org/1999/xhtml"> Test2 </p>
   */

  if (xmlnode_tmp->getChild(0).getName() == "html"
    || xmlnode_tmp->getChild(0).getName() == "body"
    || xmlnode_tmp->getChild(0).getName() == "annotation"
    || xmlnode_tmp->getChild(0).getName() == "notes")
  {
    xmlnode = new XMLNode(xmlnode_tmp->getChild(0));
    for(unsigned int i=1; i < xmlnode_tmp->getNumChildren(); i++)
    {
      xmlnode->addChild(xmlnode_tmp->getChild(i));
    }
  }
  else
  {
    xmlnode = new XMLNode();
    for(unsigned int i=0; i < xmlnode_tmp->getNumChildren(); i++)
    {
      xmlnode->addChild(xmlnode_tmp->getChild(i));
    }
  }

  delete xmlnode_tmp;
  safe_free(const_cast<char*>(xmlstr_c));

  return xmlnode;
}


/** @cond doxygen-libsbml-internal */
/*
 * Inserts this XMLNode and its children into stream.
 */
LIBLAX_EXTERN
XMLOutputStream& operator<< (XMLOutputStream& stream, const XMLNode& node)
{
  node.write(stream);
  return stream;
}
/** @endcond doxygen-libsbml-internal */


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


/**
 * Creates a new start element XMLNode_t structure with XMLTriple_t, 
 * XMLAttributes_t and XMLNamespaces_t structures set and returns a 
 * pointer to it.
 *
 * @param triple XMLTriple_t structure to be set.
 * @param attr XMLAttributes_t structure to be set.
 * @param ns XMLNamespaces_t structure to be set.
 *
 * @return pointer to new XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_createStartElementNS (const XMLTriple_t     *triple,
			      const XMLAttributes_t *attr,
			      const XMLNamespaces_t *ns)
{
  return new(nothrow) XMLNode(*triple, *attr, *ns);
}


/**
 * Creates a new start element XMLNode_t structure with XMLTriple_t 
 * and XMLAttributes_t structures set and returns a pointer to it.
 *
 * @param triple XMLTriple_t structure to be set.
 * @param attr XMLAttributes_t structure to be set.
 *
 * @return pointer to new XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_createStartElement  (const XMLTriple_t *triple,
			     const XMLAttributes_t *attr)
{
  return new(nothrow) XMLNode(*triple, *attr);
}


/**
 * Creates a new end element XMLNode_t structure with XMLTriple_t 
 * structure set and returns a pointer to it.
 *
 * @param triple XMLTriple_t structure to be set.
 *
 * @return pointer to new XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_createEndElement (const XMLTriple_t *triple)
{
  return new(nothrow) XMLNode(*triple);
}


LIBLAX_EXTERN
XMLNode_t *
XMLNode_createTextNode (const char *text)
{
  return (text != NULL) ? new(nothrow) XMLNode(text) : new(nothrow) XMLNode;
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
 * Creates a deep copy of the given XMLNode_t structure
 * 
 * @param n the XMLNode_t structure to be copied
 * 
 * @return a (deep) copy of the given XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_clone (const XMLNode_t* n)
{
  return static_cast<XMLNode*>( n->clone() );
}


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
 * Inserts a copy of child node to this XMLNode_t structure.
 *
 * @param node XMLNode_t structure to which child is to be added.
 * @pram n the index at which the given node is inserted
 * @param child XMLNode_t structure to be inserted as nth child.
 *
 * @return the newly inserted child in this XMLNode. 
 * NULL will be returned if the given child is NULL. 
 */
LIBLAX_EXTERN
XMLNode_t*
XMLNode_insertChild (XMLNode_t *node, unsigned int n, const XMLNode_t *child)
{
  if (!child)
  {
    return NULL;
  }

  return &(node->insertChild(n, *child));
}


/**
 * Removes all children from this node.
 */
LIBLAX_EXTERN
void
XMLNode_removeChildren (XMLNode_t *node)
{
  node->removeChildren();
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
 * @param n the index of the node to return
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
 * Returns the (non-const) nth child of this XMLNode_t structure.
 *
 * @param node XMLNode_t structure to be queried.
 * @param n the index of the node to return
 *
 * @return the non-const nth child of this XMLNode_t structure.
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_getChildNC (XMLNode_t *node, const unsigned int n)
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


/**
 * Returns a string which is converted from a given XMLNode. 
 *
 * @param node XMLNode_t to be converted to a string.
 *
 * @return a string (char*) which is converted from a given XMLNode.
 *
 * @note returned char* should be freed with safe_free() by the caller.
 */
LIBLAX_EXTERN
char *
XMLNode_toXMLString(const XMLNode_t *node)
{
  return safe_strdup(node->toXMLString().c_str());
}


/**
 * Returns a string which is converted from a given XMLNode. 
 *
 * @param node XMLNode_t to be converted to a string.
 *
 * @return a string (char*) which is converted from a given XMLNode.
 *
 * @note returned char* should be freed with safe_free() by the caller.
 */
LIBLAX_EXTERN
const char *
XMLNode_convertXMLNodeToString(const XMLNode_t *node)
{
  return safe_strdup((XMLNode::convertXMLNodeToString(node)).c_str());
}


/**
 * Returns an XMLNode_t pointer which is converted from a given string containing
 * XML content.
 *
 * XMLNamespaces (the second argument) must be given if the corresponding 
 * xmlns attribute is not included in the string of the first argument. 
 *
 * @param xml string to be converted to a XML node.
 * @param xmlns XMLNamespaces_t structure the namespaces to set.
 *
 * @return pointer to XMLNode_t structure which is converted from a given string. 
 */
LIBLAX_EXTERN
XMLNode_t *
XMLNode_convertStringToXMLNode(const char * xml, const XMLNamespaces_t* xmlns)
{
  return XMLNode::convertStringToXMLNode(xml, xmlns);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLNode_t structure is an XML element.
 * 
 * @param node XMLNode_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLNode_t structure is an XML element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLNode_isElement (const XMLNode_t *node)
{
  return static_cast<int>( node->isElement() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLNode_t structure is an XML end element.
 * 
 * @param node XMLNode_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLNode_t structure is an XML end element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLNode_isEnd (const XMLNode_t *node) 
{
  return static_cast<int>( node->isEnd() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLNode_t structure is an XML end element for the given start element.
 * 
 * @param node XMLNode_t structure to be queried.
 * @param element XMLNode_t structure, element for which query is made.
 *
 * @return @c non-zero (true) if this XMLNode_t structure is an XML end element for the given
 * XMLNode_t structure start element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLNode_isEndFor (const XMLNode_t *node, const XMLNode_t *element)
{
  return static_cast<int>( node->isEndFor(*element) );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLNode_t structure is an end of file marker.
 * 
 * @param node XMLNode_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLNode_t structure is an end of file (input) marker, @c zero (false)
 * otherwise.
 */
LIBLAX_EXTERN
int
XMLNode_isEOF (const XMLNode_t *node)
{
  return static_cast<int>( node->isEOF() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLNode_t structure is an XML start element.
 * 
 * @param node XMLNode_t structure to be queried.
 *
 * @return @c true if this XMLNode_t structure is an XML start element, @c false otherwise.
 */
LIBLAX_EXTERN
int
XMLNode_isStart (const XMLNode_t *node)
{
  return static_cast<int>( node->isStart() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLNode_t structure is an XML text element.
 * 
 * @param node XMLNode_t structure to be queried.
 *
 * @return @c non-zero (true) if this XMLNode_t structure is an XML text element, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLNode_isText (const XMLNode_t *node)
{
  return static_cast<int>( node->isText() );
}


/**
 * Declares this XML start element is also an end element.
 *
 * @param node XMLNode_t structure to be set.
 *
 */
LIBLAX_EXTERN
void
XMLNode_setEnd (XMLNode_t *node)
{
  node->setEnd();
}


/**
 * Declares this XMLNode_t structure is an end-of-file (input) marker.
 *
 * @param node XMLNode_t structure to be set.
 *
 */
LIBLAX_EXTERN
void
XMLNode_setEOF (XMLNode_t *node)
{
  node->setEOF();
}


/*
 * Declares this XML start/end element is no longer an end element.
 *
 * @param node XMLNode_t structure to be set.
 *
 */
LIBLAX_EXTERN
void
XMLNode_unsetEnd (XMLNode_t *node)
{
  node->unsetEnd();
}


/** @endcond doxygen-c-only */
