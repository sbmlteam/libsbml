/**
 * Filename    : SBMLHandler.hpp
 * Description : Register with XML Parser to process an SBML document
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-10-25
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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


#ifndef SBMLHandler_hh
#define SBMLHandler_hh


#include <xercesc/sax/Locator.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>

#include "Stack.h"
#include "SBMLReader.h"
#include "SBMLTypes.h"

#include "common.hpp"
#include "XMLStringFormatter.hpp"


//
// SBMLHandler
//
// This XML document handler is responsible for constructing an
// SBMLDocument from SAX2 events deliverd by a SAX2XMLReader.
//
class SBMLHandler : public DefaultHandler
{

public:

  //
  // Ctor
  //
  // Creates a new SBMLHandler.  The given SBMLDocument should be empty
  // and will be populated as the document is parsed.
  //
  SBMLHandler (SBMLDocument_t *d) : fDocument(d) { };

  //
  // Dtor
  //
  ~SBMLHandler ();


  void startDocument ();

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

  void ignorableWhitespace
  (
    const XMLCh* const  chars,
    const unsigned int  length
  );

  void setDocumentLocator (const Locator *const locator);

  inline void warning    (const SAXParseException&);
  inline void error      (const SAXParseException&);
  inline void fatalError (const SAXParseException&);

  inline void warning    (const char* message);
  inline void error      (const char* message);
  inline void fatalError (const char* message);

  inline ParseMessage_t* ParseMessage_createFrom (const char* message);
         ParseMessage_t* ParseMessage_createFrom (const SAXParseException& e);


private:

  SBase_t* handleSBML                     (const Attributes& a);
  SBase_t* handleModel                    (const Attributes& a);
  SBase_t* handleUnitDefinition           (const Attributes& a);
  SBase_t* handleUnit                     (const Attributes& a);
  SBase_t* handleCompartment              (const Attributes& a);
  SBase_t* handleSpecies                  (const Attributes& a);
  SBase_t* handleParameter                (const Attributes& a);
  SBase_t* handleReaction                 (const Attributes& a);
  SBase_t* handleSpeciesReference         (const Attributes& a);
  SBase_t* handleKineticLaw               (const Attributes& a);
  SBase_t* handleAlgebraicRule            (const Attributes& a);
  SBase_t* handleCompartmentVolumeRule    (const Attributes& a);
  SBase_t* handleParameterRule            (const Attributes& a);
  SBase_t* handleSpeciesConcentrationRule (const Attributes& a);


  SBMLDocument_t*     fDocument;
  Model_t*            fModel;
  Stack_t*            fObjStack;
  Stack_t*            fTagStack;
  XMLStringFormatter* fFormatter;

  const Locator* fLocator;

  /**
   * Since annotation is defined to be of type any, technically
   * <annotation> tags may be nested, for e.g.:
   *
   *   <annotation xmlns:mysim="http://www.mysim.org/ns">
   *     <mysim:nodecolors mysim:bgcolor="green" mysim:fgcolor="white"/>
   *     <annotation value="some other annotation"/>
   *   </annotation>
   *
   * This SBML files for KEGG contained such a corner case.
   *
   * Nested <annotation> tags are part of the top-level <annotation> string
   * and therefore should ignored by this handler, i.e. no special action
   * should be taken.  However, the handler still receives startElement()
   * and endElement() events for the nested tags and needs to be aware of
   * which ones to ignore.  This can be done by tracking nested tags.
   *
   * To track nested <annotation>s, instead of allowing the fTagStack to
   * grow arbitrarily large, an int is used to indicate the depth of
   * <annotation> tags encountered in the parse. An inAnnotation of zero
   * (0) means no <annotation>s are being processed currently.
   *
   * While notes may not be nested (they are of type XHTML) they are
   * tracked in a similiar manner to issue warnings and help recover from
   * just such an error.
   */ 
  int inNotes;
  int inAnnotation;
};


#endif  // SBMLHandler_hh
