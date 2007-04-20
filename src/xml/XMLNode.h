/**
 * @file    XMLNode.h
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 * @class XMLNode.
 * @brief Implementation of %XMLNode construct.
 */


#ifndef XMLNode_h
#define XMLNode_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <vector>


class XMLInputStream;
class XMLOutputStream;


class LIBLAX_EXTERN XMLNode : public XMLToken
{
public:

  /**
   * Creates a new empty XMLNode with no children.
   */
  XMLNode ();

  /**
   * Creates a new XMLNode by copying token.
   */
  XMLNode (const XMLToken& token);

  /**
   * Creates a new XMLNode by reading XMLTokens from stream.  The stream must
   * be positioned on a start element (stream.peek().isStart() == true) and
   * will be read until the matching end element is found.
   */
  XMLNode (XMLInputStream& stream);

  /**
   * Destroys this XMLNode.
   */
  virtual ~XMLNode ();

  /**
   * Adds a copy of child node to this XMLNode.
   */
  void addChild (const XMLNode& node);

  /**
   * Returns the nth child of this XMLNode.
   */
  const XMLNode& getChild (unsigned int n) const;

  /**
   * @return the number of children for this XMLNode.
   */
  unsigned int getNumChildren () const;

  /**
   * Writes this XMLNode and its children to stream.
   */
  void write (XMLOutputStream& stream) const;


#ifndef SWIG

  /**
   * Inserts this XMLNode and its children into stream.
   */
  LIBLAX_EXTERN
  friend
  XMLOutputStream& operator<< (XMLOutputStream& stream, const XMLNode& node);

#endif  /* !SWIG */


protected:

  std::vector<XMLNode> mChildren;
};

#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS


/**
 * 
 **/
LIBLAX_EXTERN
XMLNode_t *
XMLNode_create (void);


/**
 * 
 **/
LIBLAX_EXTERN
XMLNode_t *
XMLNode_createFromToken (const XMLToken_t *token);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNode_free (XMLNode_t *node);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLNode_addChild (XMLNode_t *node, const XMLNode_t *child);


/**
 * 
 **/
LIBLAX_EXTERN
const XMLNode_t *
XMLNode_getChild (const XMLNode_t *node, const int n);


/**
 * 
 **/
LIBLAX_EXTERN
unsigned int
XMLNode_getNumChildren (const XMLNode_t *node);



END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLNode_h */
