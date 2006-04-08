/**
 * \file    XercesNamespaces.h
 * \brief   Extracts XML namespace declarations from Xerces-C++ attributes.
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


#ifndef XercesNamespaces_h
#define XercesNamespaces_h


#include <xercesc/sax2/Attributes.hpp>
#include <sbml/xml/XMLNamespaces.h>



class XercesNamespaces : public XMLNamespaces
{
public:

  /**
   * Creates a new list of XML namespaces declarations from a "raw" Xerces-C++
   * Attributes set.
   */
  XercesNamespaces (const xercesc::Attributes& attrs);

  /**
   * Destroys this list of XML namespace declarations.
   */
  virtual ~XercesNamespaces ();
};


#endif  /* XercesNamespaces_h */
