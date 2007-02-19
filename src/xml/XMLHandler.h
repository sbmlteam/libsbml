/**
 * \file    XMLHandler.h
 * \brief   XMLHandler interface
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


#ifndef XMLHandler_h
#define XMLHandler_h

#ifdef __cplusplus

#include <string>
#include <sbml/xml/XMLExtern.h>


class XMLToken;


class LIBLAX_EXTERN XMLHandler
{
public:

  /**
   * Creates a new XMLHandler.
   */
  XMLHandler ();

  /**
   * Destroys this XMLHandler.
   */
  virtual ~XMLHandler ();

  /**
   * Receive notification of the beginning of the document.
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the start of the document.
   */
  virtual void startDocument ();

  /**
   * Receive notification of the XML declaration, i.e.
   * <?xml version="1.0" encoding="UTF-8"?>
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the declaration.
   */
  virtual void XML (const std::string& version, const std::string& encoding);

  /**
   * Receive notification of the start of an element.
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the start of each element.
   */
  virtual void startElement (const XMLToken& element);

  /**
   * Receive notification of the end of the document.
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the end of the document.
   */
  virtual void endDocument ();

  /**
   * Receive notification of the end of an element.
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the end of each element.
   */
  virtual void endElement (const XMLToken& element);

  /**
   * Receive notification of character data inside an element.
   *
   * By default, do nothing. Application writers may override this method
   * to take specific actions for each chunk of character data.
   */
  virtual void characters (const XMLToken& data);
};


#endif  /* __cplusplus */

#endif  /* XMLHandler_h */
