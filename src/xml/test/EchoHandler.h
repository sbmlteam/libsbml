/**
 * \file    EchoHandler.h
 * \brief   Example XMLHandler that simply echos (prints out) an XML document.
 * \author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

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
