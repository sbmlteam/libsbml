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

#include "MathMLDocument.h"
#include "ParseMessage.h"
#include "Stack.h"
#include "SBMLTypes.h"

#include "common.hpp"
#include "SBMLTagCodes.hpp"
#include "MathMLHandler.hpp"
#include "XMLStringFormatter.hpp"


class SBMLHandler;
typedef SBase_t* (SBMLHandler::*TagHandler_t)(const Attributes& attrs);


/**
 * SBMLHandler
 *
 * This XML document handler is responsible for constructing an
 * SBMLDocument from SAX2 events deliverd by a SAX2XMLReader.
 */
class SBMLHandler : public DefaultHandler
{

public:

  /**
   * Ctor
   *
   * Creates a new SBMLHandler.  The given SBMLDocument should be empty
   * and will be populated as the document is parsed.
   */
  SBMLHandler (SBMLDocument_t *d) : fDocument(d) { };

  /**
   * Dtor
   */
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

  SBase_t* doSBML                      (const Attributes& a);
  SBase_t* doModel                     (const Attributes& a);

  SBase_t* doListOfFunctionDefinitions (const Attributes& a);
  SBase_t* doListOfUnitDefinitions     (const Attributes& a);
  SBase_t* doListOfUnits               (const Attributes& a);
  SBase_t* doListOfCompartments        (const Attributes& a);
  SBase_t* doListOfSpecies             (const Attributes& a);
  SBase_t* doListOfParameters          (const Attributes& a);
  SBase_t* doListOfRules               (const Attributes& a);
  SBase_t* doListOfReactions           (const Attributes& a);
  SBase_t* doListOfReactants           (const Attributes& a);
  SBase_t* doListOfProducts            (const Attributes& a);
  SBase_t* doListOfModifiers           (const Attributes& a);
  SBase_t* doListOfEvents              (const Attributes& a);
  SBase_t* doListOfEventAssignments    (const Attributes& a);

  SBase_t* doFunctionDefinition        (const Attributes& a);
  SBase_t* doUnitDefinition            (const Attributes& a);
  SBase_t* doUnit                      (const Attributes& a);
  SBase_t* doCompartment               (const Attributes& a);
  SBase_t* doSpecies                   (const Attributes& a);
  SBase_t* doParameter                 (const Attributes& a);
  SBase_t* doReaction                  (const Attributes& a);
  SBase_t* doSpeciesReference          (const Attributes& a);
  SBase_t* doModifierSpeciesReference  (const Attributes& a);
  SBase_t* doKineticLaw                (const Attributes& a);
  SBase_t* doAssignmentRule            (const Attributes& a);
  SBase_t* doRateRule                  (const Attributes& a);
  SBase_t* doAlgebraicRule             (const Attributes& a);
  SBase_t* doCompartmentVolumeRule     (const Attributes& a);
  SBase_t* doParameterRule             (const Attributes& a);
  SBase_t* doSpeciesConcentrationRule  (const Attributes& a);
  SBase_t* doEvent                     (const Attributes& a);
  SBase_t* doEventAssignment           (const Attributes& a);
  SBase_t* doStackPeek                 (const Attributes& a);

  SBMLTagCode_t getTagCode (const XMLCh *uri, const XMLCh* localname);

  void setMath(ASTNode_t* math);
  void setStoichiometryMath(SpeciesReference_t* sr, ASTNode_t* math);

  /*
  void debugPrintStartElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname,
    const Attributes&   attrs
  );
  */


  SBMLDocument_t* fDocument;
  Model_t*        fModel;
  Stack_t*        fObjStack;
  Stack_t*        fTagStack;

  XMLStringFormatter* fFormatter;

  MathMLHandler*    fMathHandler;
  MathMLDocument_t* fMathDocument;

  const Locator* fLocator;

  const static TagHandler_t TagHandler[];

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
  int inMath;
};


#endif  // SBMLHandler_hh
