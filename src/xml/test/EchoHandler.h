/**
 * \file    EchoHandler.h
 * \brief   Example XMLHandler that simply echos (prints out) an XML document.
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


#ifndef EchoHandler_h
#define EchoHandler_h


#include <string>
#include <sbml/xml/XMLHandler.h>


class EchoHandler : public XMLHandler
{
public:

  /**
   * Creates a new XMLEchoHandler.
   */
  EchoHandler ();

  /**
   * Destroys this XMLEchoHandler.
   */
  virtual ~EchoHandler ();


  /**
   * Receive notification of the XML declaration.
   */
  virtual void XML (const std::string& version, const std::string& encoding);

  /**
   * Receive notification of the beginning of the document.
   */
  virtual void startDocument ();

  /**
   * Receive notification of the start of an element.
   */
  virtual void startElement (const XMLToken& element);

  /**
   * Receive notification of the end of the document.
   */
  virtual void endDocument ();

  /**
   * Receive notification of the end of an element.
   */
  virtual void endElement (const XMLToken& element);

  /**
   * Receive notification of character data inside an element.
   */
  virtual void characters (const XMLToken& data);


protected:

  bool mInStart;
};


#endif  /* EchoHandler_h */
