/**
 * \file    XercesAttributes.h
 * \brief   Creates new XMLAttributes from "raw" Xerces-C++ attributes.
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


#ifndef XercesAttributes_h
#define XercesAttributes_h
#ifdef __cplusplus

#include <string>


#include <xercesc/sax2/Attributes.hpp>
#include <sbml/xml/XMLAttributes.h>

  /** @cond doxygen-libsbml-internal */

class XercesAttributes : public XMLAttributes
{
public:

  /**
   * Creates a new XMLAttributes set that wraps the given "raw" Xerces-C++
   * Attributes set.
   */
  XercesAttributes (const xercesc::Attributes& attrs);

  /**
   * Destroys this XercesAttributes set.
   */
  virtual ~XercesAttributes ();
};

  /** @endcond doxygen-libsbml-internal */
#endif  /* __cplusplus */
#endif  /* XercesAttributes_h */
