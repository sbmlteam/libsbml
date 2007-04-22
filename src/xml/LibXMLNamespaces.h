/**
 * \file    LibXMLNamespaces.h
 * \brief   Extracts XML namespace declarations from LibXML prefix/URI pairs.
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


#ifndef LibXMLNamespaces_h
#define LibXMLNamespaces_h

#ifdef __cplusplus

#include <string>

#include <libxml/parser.h>
#include <sbml/xml/XMLNamespaces.h>

  /** @cond doxygen-libsbml-internal */

class LibXMLNamespaces : public XMLNamespaces
{
public:

  /**
   * Creates a new list of XML namespaces declarations from a "raw" LibXML
   * prefix/URI pairs.
   */
  LibXMLNamespaces (const xmlChar** namespaces, const unsigned int& size);

  /**
   * Destroys this list of XML namespace declarations.
   */
  virtual ~LibXMLNamespaces ();
};

  /** @endcond doxygen-libsbml-internal */
#endif  /* __cplusplus */
#endif  /* LibXMLNamespaces_h */
