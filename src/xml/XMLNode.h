/**
 * \file    XMLNode.h
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


#ifndef XMLNode_h
#define XMLNode_h

#ifdef __cplusplus

#include <vector>

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLToken.h>


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

#endif  /* XMLNode_h */
