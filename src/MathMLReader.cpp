/**
 * Filename    : MathMLReader.cpp
 * Description : Reads a MathML string into an abstract syntax tree
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-06
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 *   Stefan Hoops
 */


#include <iostream>
#include "sbml/common.h"


#ifndef USE_EXPAT
#  include <xercesc/framework/MemBufInputSource.hpp>
#  include <xercesc/sax2/DefaultHandler.hpp>
#  include <xercesc/sax2/SAX2XMLReader.hpp>
#  include <xercesc/sax2/XMLReaderFactory.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#endif  // !USE_EXPAT


#include "sbml/List.h"
#include "sbml/MathMLHandler.hpp"
#include "sbml/MathMLReader.h"


/**
 * Reads the MathML from the given XML string, constructs a corresponding
 * abstract syntax tree and returns a pointer to the root of the tree.
 */
LIBSBML_EXTERN
MathMLDocument_t *
readMathMLFromString (const char *xml)
{
  if (xml == NULL) return NULL;


#ifdef USE_EXPAT

  static MathMLDocument_t* d = NULL;

  if (!d)
  {
    d = MathMLDocument_create();
  }
  else
  {
    d->math = NULL;
  }
  
  static MathMLHandler* handler = NULL;

  if (!handler)
  {
    handler = new MathMLHandler(d);
    handler->enableElementHandler();
  }
  else
  {
    handler->create();
    handler->enableElementHandler();
  }

  try
  {
    handler->startDocument();
    handler->parse(xml, -1, true);
    handler->endDocument();
  }
  catch (...)
  {

  }

#else

  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (...)
  {
    return NULL;
  }

  MathMLDocument_t*  d       = MathMLDocument_create();
  SAX2XMLReader*     reader  = XMLReaderFactory::createXMLReader();
  DefaultHandler*    handler = new MathMLHandler(d);
  MemBufInputSource* input   = NULL;

  input = new MemBufInputSource( (const XMLByte*) xml,
                                 strlen(xml),
                                 "FromString",
                                 false );

  reader->setFeature( XMLUni::fgSAX2CoreNameSpaces       , true );
  reader->setFeature( XMLUni::fgSAX2CoreNameSpacePrefixes, true );

  reader->setContentHandler(handler);
  reader->setErrorHandler(handler);

  try
  {
    reader->parse(*input);
  }
  catch (...)
  {

  }

  delete input;
  delete reader;
  delete handler;

#endif  // USE_EXPAT

  return d;
}
