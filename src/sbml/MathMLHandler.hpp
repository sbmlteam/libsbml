/**
 * Filename    : MathMLHandler.hpp
 * Description : MathML SAX2 Handler
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef MathMLHandler_hpp
#define MathMLHandler_hpp


#include <xercesc/sax/Locator.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>

#include "common.hpp"
#include "MathMLTagCodes.hpp"

#include "ASTNode.h"
#include "MathMLDocument.h"
#include "Stack.h"


//
// MathMLHandler
//
// This XML document handler is responsible for constructing an an Abstract
// Syntax Tree from SAX2 events deliverd by a SAX2XMLReader.
//
class MathMLHandler : public DefaultHandler
{

public:

  //
  // Ctor
  //
  // Creates a new MathMLHandler.  The given MathMLDocument should be empty
  // and will be populated as the document is parsed.
  //
  MathMLHandler (MathMLDocument_t *d) : fDocument(d) { };


  void startDocument ();
  void endDocument   ();

  void startElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname,
    const Attributes&   attrs
  );

  void endElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname
  );

  void characters(const XMLCh* const chars, const unsigned int length);

  void setDocumentLocator (const Locator *const locator);


private:

  void checkFunctionArgs (ASTNode_t*  node);

  MathMLTagCode_t getTagCode (const XMLCh* uri, const XMLCh* localname);

  void parseCN (const char* str);
  void parseCI (const char* str);

  void reduceExpression ();

  void setTypeCN (ASTNode_t* node, const Attributes& a);
  void setTypeCS (ASTNode_t* node, const Attributes& a);


  MathMLDocument_t* fDocument;
  Stack_t*          fObjStack;
  Stack_t*          fTagStack;

  const Locator* fLocator;

  bool fSeenSep;
};


#endif  // MathMLHandler_hpp
