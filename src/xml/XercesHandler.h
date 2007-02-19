/**
 * \file    XercesHandler.h
 * \brief   Redirect Xerces-C++ SAX2 events to an XMLHandler
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


#ifndef XercesHandler_h
#define XercesHandler_h

#ifdef __cplusplus

#include <string>

#include <xercesc/sax2/DefaultHandler.hpp>


class XMLHandler;

class XercesHandler : public xercesc::DefaultHandler
{
public:

  /**
   * Creates a new XercesHandler.  Xerces-C++ SAX2 events will be
   * redirected to the given XMLHandler.
   */
  XercesHandler (XMLHandler& handler);

  /**
   * Destroys this XercesHandler.
   */
  virtual ~XercesHandler ();


  /**
   * Receive notification of the beginning of the document.
   */
  void startDocument ();

  /**
   * Receive notification of the start of an element.
   *
   * @param  uri        The URI of the associated namespace for this element
   * @param  localname  The local part of the element name
   * @param  qname      The qualified name of this element
   * @param  attrs      The specified or defaulted attributes
   */
  virtual void startElement
  (
     const XMLCh* const  uri
   , const XMLCh* const  localname
   , const XMLCh* const  qname
   , const xercesc::Attributes& attrs
  );

  /**
   * Receive notification of the end of the document.
   */
  void endDocument ();

  /**
   * Receive notification of the end of an element.
   *
   * @param  uri        The URI of the associated namespace for this element
   * @param  localname  The local part of the element name
   * @param  qname      The qualified name of this element
   */
  void endElement
  (
     const XMLCh* const  uri
   , const XMLCh* const  localname
   , const XMLCh* const  qname
  );

  /**
   * Receive notification of character data inside an element.
   *
   * @param  chars   The characters
   * @param  length  The number of characters to use from the character array
   */
  void characters (const XMLCh* const chars, const unsigned int length);

  /**
   * @return the column number of the current XML event.
   */
  unsigned int getColumn () const;

  /**
   * @return the line number of the current XML event.
   */
  unsigned int getLine () const;

  /**
   * Receive a Locator object for document events.
   */
  void setDocumentLocator (const xercesc::Locator* const locator);


protected:

  XMLHandler&              mHandler;
  const xercesc::Locator*  mLocator;
};

#endif  /* __cplusplus */
#endif  /* XercesHandler_h */
