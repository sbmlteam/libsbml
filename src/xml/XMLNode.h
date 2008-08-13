/**
 * @file    XMLNode.h
 * @brief   Class definition of XMLNode, a node in an XML document tree.
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
 *----------------------------------------------------------------------- -->
 *
 * @class XMLNode
 * @brief Representation of a node in an XML document tree.
 * 
 * Beginning with version 3.0.0, libSBML implements an XML abstraction
 * layer.  This layer presents a uniform XML interface to calling programs
 * regardless of which underlying XML parser libSBML has actually been
 * configured to use.  The basic data object in the XML abstraction is a
 * @em node, represented by XMLNode.
 *
 * An XMLNode can contain any number of children.  Each child is another
 * XMLNode, forming a tree.  The methods getNumChildren() and getChild()
 * can be used to access the tree structure starting from a given node.
 *
 * Each XMLNode is subclassed from XMLToken, and thus has the same methods
 * available as XMLToken.  These methods include XMLToken::getNamespaces(),
 * XMLToken::getPrefix(), XMLToken::getName(), XMLToken::getURI(), and
 * XMLToken::getAttributes().
 */

#ifndef XMLNode_h
#define XMLNode_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <vector>
#include <cstdlib>

/** @cond doxygen-libsbml-internal */
class XMLInputStream;
class XMLOutputStream;
/** @endcond doxygen-libsbml-internal */


class LIBLAX_EXTERN XMLNode : public XMLToken
{
public:

  /**
   * Creates a new empty XMLNode with no children.
   */
  XMLNode ();


  /**
   * Creates a new XMLNode by copying token.
   *
   * @param token XMLToken to be copied to XMLNode
   */
  XMLNode (const XMLToken& token);

  /**
   * Creates a new start element XMLNode with the given set of attributes and
   * namespace declarations.
   *
   * @param triple XMLTriple.
   * @param attributes XMLAttributes, the attributes to set.
   * @param namespaces XMLNamespaces, the namespaces to set.
   * @param line an unsigned int, the line number (default = 0).
   * @param column an unsigned int, the column number (default = 0).
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
  XMLNode (  const XMLTriple&     triple
           , const XMLAttributes& attributes
           , const XMLNamespaces& namespaces
           , const unsigned int   line   = 0
           , const unsigned int   column = 0 );


  /**
   * Creates a start element XMLNode with the given set of attributes.
   *
   * @param triple XMLTriple.
   * @param attributes XMLAttributes, the attributes to set.
   * @param line an unsigned int, the line number (default = 0).
   * @param column an unsigned int, the column number (default = 0).
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
  XMLNode (  const XMLTriple&      triple
           , const XMLAttributes&  attributes
           , const unsigned int    line   = 0
           , const unsigned int    column = 0 );


  /**
   * Creates an end element XMLNode.
   *
   * @param triple XMLTriple.
   * @param line an unsigned int, the line number (default = 0).
   * @param column an unsigned int, the column number (default = 0).
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
  XMLNode (  const XMLTriple&    triple
           , const unsigned int  line   = 0
           , const unsigned int  column = 0 );


  /**
   * Creates a text XMLNode.
   *
   * @param chars a string, the text to be added to the XMLToken
   * @param line an unsigned int, the line number (default = 0).
   * @param column an unsigned int, the column number (default = 0).
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
  XMLNode (  const std::string&  chars
           , const unsigned int  line   = 0
           , const unsigned int  column = 0 );


  /** @cond doxygen-libsbml-internal */
  /**
   * Creates a new XMLNode by reading XMLTokens from stream.  
   *
   * The stream must be positioned on a start element
   * (<code>stream.peek().isStart() == true</code>) and will be read until
   * the matching end element is found.
   *
   * @param stream XMLInputStream from which XMLNode is to be created.
   */
  XMLNode (XMLInputStream& stream);
  /** @endcond doxygen-libsbml-internal */


  /**
   * Destroys this XMLNode.
   */
  virtual ~XMLNode ();

  
  /**
   * Copy constructor; creates a copy of this XMLNode.
   * 
   * @param orig the XMLNode instance to copy.
   */
  XMLNode(const XMLNode& orig);


  /**
   * Assignment operator for XMLNode.
   */
  XMLNode& operator=(const XMLNode& orig);


  /**
   * Creates and returns a deep copy of this XMLNode.
   * 
   * @return a (deep) copy of this XMLNode.
   */
  XMLNode* clone () const;


  /**
   * Adds a copy of child node to this XMLNode.
   *
   * @param node XMLNode to be added as child.
   *
   * @note The given node is added at the end of the children list.
   */
  void addChild (const XMLNode& node);


  /**
   * Inserts a copy of the given node as the nth child of this XMLNode.
   *
   * @param n the index at which the given node is inserted
   * @param node XMLNode to be inserted as child.
   *
   * @return the reference to the newly inserted child in this XMLNode.
   *
   * @note The given node is added at the end of the list of the 
   * children if the given index is out of range.
   */
  XMLNode& insertChild (unsigned int n, const XMLNode& node);


  /**
   * Removes the nth child of this XMLNode and returned the removed node.
   * The caller owns the returned node and is responsible for deleting it.
   *
   * @param n the index of the node to be removed
   *
   * @return the removed child, or NULL if the given index is out of range. 
   *
   * @note This function invalidates all existing references to child nodes 
   * after the position or first.
   */
  XMLNode* removeChild(unsigned int n);


  /**
   * Removes all children from this node.
   */
  void removeChildren() { mChildren.clear(); }


  /**
   * Returns the nth child of this XMLNode.
   *
   * @param n the index of the node to return
   * 
   * @return the nth child of this XMLNode.
   */
  XMLNode& getChild (unsigned int n);


  /**
   * Returns the nth child of this XMLNode.
   *
   * @param n the index of the node to return
   * 
   * @return the nth child of this XMLNode.
   */
  const XMLNode& getChild (unsigned int n) const;


  /**
   * Returns the number of children for this XMLNode.
   *
   * @return the number of children for this XMLNode.
   */
  unsigned int getNumChildren () const;


  /** @cond doxygen-libsbml-internal */
  /**
   * Writes this XMLNode and its children to stream.
   *
   * @param stream XMLOutputStream, stream to which this XMLNode
   * is to be written.
   */
  void write (XMLOutputStream& stream) const;
  /** @endcond doxygen-libsbml-internal */


  /**
   * Returns a string which is converted from this XMLNode. 
   *
   * @return a string which is converted from this XMLNode.
   */
  std::string toXMLString() const;


  /**
   * Returns a string which is converted from a given XMLNode. 
   *
   * @param node XMLNode to be converted to a string.
   *
   * @return a string which is converted from a given XMLNode.
   */
  static std::string convertXMLNodeToString(const XMLNode* node);


  /**
   * Returns an XMLNode which is converted from a given string containing
   * XML content.
   *
   * XMLNamespaces (the second argument) must be given if the corresponding 
   * xmlns attribute is not included in the string of the first argument. 
   *
   * @param xmlstr string to be converted to a XML node.
   * @param xmlns XMLNamespaces the namespaces to set (default value is NULL).
   *
   * @return a XMLNode which is converted from a given string. The caller owns
   * the returned XMLNode and is reponsible for deleting it.
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
  static XMLNode* convertStringToXMLNode(const std::string& xmlstr,
                                         const XMLNamespaces* xmlns = NULL);


#ifndef SWIG

  /** @cond doxygen-libsbml-internal */
  /**
   * Inserts this XMLNode and its children into stream.
   *
   * @param stream XMLOutputStream, stream to which the XMLNode
   * is to be written.
   * @param node XMLNode, node to be written to stream.
   *
   * @return the stream with the node inserted.
   */
  LIBLAX_EXTERN
  friend
  XMLOutputStream& operator<< (XMLOutputStream& stream, const XMLNode& node);
  /** @endcond doxygen-libsbml-internal */

#endif  /* !SWIG */


protected:
  /** @cond doxygen-libsbml-internal */

  std::vector<XMLNode> mChildren;

  /** @endcond doxygen-libsbml-internal */
};

#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBLAX_EXTERN
XMLNode_t *
XMLNode_create (void);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_createFromToken (const XMLToken_t *token);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_createStartElement  (const XMLTriple_t *triple,
			     const XMLAttributes_t *attr);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_createStartElementNS (const XMLTriple_t *triple,
			      const XMLAttributes_t *attr,
			      const XMLNamespaces_t *ns);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_createEndElement (const XMLTriple_t *triple);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_createTextNode (const char *text);


LIBLAX_EXTERN
void
XMLNode_free (XMLNode_t *node);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_clone (const XMLNode_t* c);


LIBLAX_EXTERN
void
XMLNode_addChild (XMLNode_t *node, const XMLNode_t *child);


LIBLAX_EXTERN
XMLNode_t*
XMLNode_insertChild (XMLNode_t *node, unsigned int n, const XMLNode_t *child);


LIBLAX_EXTERN
XMLNode_t* 
XMLNode_removeChild(XMLNode_t *node, unsigned int n);


LIBLAX_EXTERN
void
XMLNode_removeChildren (XMLNode_t *node);


LIBLAX_EXTERN
const char *
XMLNode_getCharacters (const XMLNode_t *node);


LIBLAX_EXTERN
const char *
XMLNode_getName (const XMLNode_t *node);


LIBLAX_EXTERN
void 
XMLNode_setTriple(XMLNode_t *node, const XMLTriple_t *triple);


LIBLAX_EXTERN
const char *
XMLNode_getPrefix (const XMLNode_t *node);


LIBLAX_EXTERN
const char *
XMLNode_getURI (const XMLNode_t *node);



LIBLAX_EXTERN
const XMLAttributes_t *
XMLNode_getAttributes (const XMLNode_t *node);


LIBLAX_EXTERN
void 
XMLNode_setAttributes (XMLNode_t *node, const XMLAttributes_t* attributes);


LIBLAX_EXTERN
void 
XMLNode_addAttr ( XMLNode_t *node,  const char* name, const char* value );
		   

LIBLAX_EXTERN
void 
XMLNode_addAttrWithNS ( XMLNode_t *node,  const char* name
	                , const char* value
    	                , const char* namespaceURI
	                , const char* prefix      );


LIBLAX_EXTERN
void 
XMLNode_addAttrWithTriple (XMLNode_t *node, const XMLTriple_t *triple, const char* value);


LIBLAX_EXTERN
void 
XMLNode_removeAttr (XMLNode_t *node, int n);


LIBLAX_EXTERN
void 
XMLNode_removeAttrByName (XMLNode_t *node, const char* name);


LIBLAX_EXTERN
void 
XMLNode_removeAttrByNS (XMLNode_t *node, const char* name, const char* uri);


LIBLAX_EXTERN
void 
XMLNode_removeAttrByTriple (XMLNode_t *node, const XMLTriple_t *triple);


LIBLAX_EXTERN
void 
XMLNode_clearAttributes(XMLNode_t *node);


LIBLAX_EXTERN
int 
XMLNode_getAttrIndex (const XMLNode_t *node, const char* name, const char* uri);


LIBLAX_EXTERN
int 
XMLNode_getAttrIndexByTriple (const XMLNode_t *node, const XMLTriple_t *triple);


LIBLAX_EXTERN
int 
XMLNode_getAttributesLength (const XMLNode_t *node);


LIBLAX_EXTERN
char* 
XMLNode_getAttrName (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getAttrPrefix (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getAttrPrefixedName (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getAttrURI (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getAttrValue (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getAttrValueByName (const XMLNode_t *node, const char* name);


LIBLAX_EXTERN
char* 
XMLNode_getAttrValueByNS (const XMLNode_t *node, const char* name, const char* uri);


LIBLAX_EXTERN
char* 
XMLNode_getAttrValueByTriple (const XMLNode_t *node, const XMLTriple_t *triple);


LIBLAX_EXTERN
int
XMLNode_hasAttr (const XMLNode_t *node, int index);


LIBLAX_EXTERN
int
XMLNode_hasAttrWithName (const XMLNode_t *node, const char* name);

LIBLAX_EXTERN
int
XMLNode_hasAttrWithNS (const XMLNode_t *node, const char* name, const char* uri);


LIBLAX_EXTERN
int
XMLNode_hasAttrWithTriple (const XMLNode_t *node, const XMLTriple_t *triple);


LIBLAX_EXTERN
int
XMLNode_isAttributesEmpty (const XMLNode_t *node);



LIBLAX_EXTERN
const XMLNamespaces_t *
XMLNode_getNamespaces (const XMLNode_t *node);


LIBLAX_EXTERN
void 
XMLNode_setNamespaces(XMLNode_t *node, const XMLNamespaces_t* namespaces);


LIBLAX_EXTERN
void 
XMLNode_addNamespace (XMLNode_t *node, const char* uri, const char* prefix);


LIBLAX_EXTERN
void 
XMLNode_removeNamespace (XMLNode_t *node, int index);


LIBLAX_EXTERN
void 
XMLNode_removeNamespaceByPrefix (XMLNode_t *node, const char* prefix);


LIBLAX_EXTERN
void 
XMLNode_clearNamespaces (XMLNode_t *node);


LIBLAX_EXTERN
int 
XMLNode_getNamespaceIndex (const XMLNode_t *node, const char* uri);


LIBLAX_EXTERN
int 
XMLNode_getNamespaceIndexByPrefix (const XMLNode_t *node, const char* prefix);


LIBLAX_EXTERN
int 
XMLNode_getNamespacesLength (const XMLNode_t *node);


LIBLAX_EXTERN
char* 
XMLNode_getNamespacePrefix (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getNamespacePrefixByURI (const XMLNode_t *node, const char* uri);


LIBLAX_EXTERN
char* 
XMLNode_getNamespaceURI (const XMLNode_t *node, int index);


LIBLAX_EXTERN
char* 
XMLNode_getNamespaceURIByPrefix (const XMLNode_t *node, const char* prefix);


LIBLAX_EXTERN
int
XMLNode_isNamespacesEmpty (const XMLNode_t *node);


LIBLAX_EXTERN
int
XMLNode_hasNamespaceURI(const XMLNode_t *node, const char* uri);


LIBLAX_EXTERN
int
XMLNode_hasNamespacePrefix(const XMLNode_t *node, const char* prefix);


LIBLAX_EXTERN
int
XMLNode_hasNamespaceNS(const XMLNode_t *node, const char* uri, const char* prefix);
                        


LIBLAX_EXTERN
int
XMLNode_isElement (const XMLNode_t *node);


LIBLAX_EXTERN
int
XMLNode_isEnd (const XMLNode_t *node); 


LIBLAX_EXTERN
int
XMLNode_isEndFor (const XMLNode_t *node, const XMLNode_t *element);


LIBLAX_EXTERN
int
XMLNode_isEOF (const XMLNode_t *node);


LIBLAX_EXTERN
int
XMLNode_isStart (const XMLNode_t *node);


LIBLAX_EXTERN
int
XMLNode_isText (const XMLNode_t *node);


LIBLAX_EXTERN
void
XMLNode_setEnd (XMLNode_t *node);


LIBLAX_EXTERN
void
XMLNode_setEOF (XMLNode_t *node);


LIBLAX_EXTERN
void
XMLNode_unsetEnd (XMLNode_t *node);


LIBLAX_EXTERN
const XMLNode_t *
XMLNode_getChild (const XMLNode_t *node, const int n);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_getChildNC (XMLNode_t *node, const unsigned int n);


LIBLAX_EXTERN
unsigned int
XMLNode_getNumChildren (const XMLNode_t *node);


LIBLAX_EXTERN
char *
XMLNode_toXMLString(const XMLNode_t *node);


LIBLAX_EXTERN
const char *
XMLNode_convertXMLNodeToString(const XMLNode_t *node);


LIBLAX_EXTERN
XMLNode_t *
XMLNode_convertStringToXMLNode(const char * xml, const XMLNamespaces_t* xmlns);

END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLNode_h */
