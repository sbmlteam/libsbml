/**
 * \file    SBMLHandler.h
 * \brief   Register with XML Parser to process an SBML document
 * \author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Stefan Hoops
 */


#ifndef SBMLHandler_h
#define SBMLHandler_h


#include "common/libsbml-config.h"


#ifdef __cplusplus


#ifdef USE_EXPAT
#  include "xml/Expat.h"
#  include "xml/ExpatAttributes.h"
#  include "xml/ExpatToXerces.h"
#else
#  include <xercesc/sax/Locator.hpp>
#  include <xercesc/sax2/DefaultHandler.hpp>
   using xercesc::Attributes;
   using xercesc::DefaultHandler;
   using xercesc::Locator;
   using xercesc::SAXParseException;
#endif  /* USE_EXPAT */

#include "util/Stack.h"
#include "SBMLTagCodes.h"


class SBase;
class SBMLDocument;
class Model;
class SpeciesReference;

class ASTNode;
class MathMLHandler;
class MathMLDocument;

class ParseMessage;
class XMLStringFormatter;
class SBMLHandler;

#ifdef USE_LAYOUT
  class LayoutHandler;
#endif  /* USE_LAYOUT */


typedef SBase* (SBMLHandler::*TagHandler_t)(const Attributes& attrs);


#ifdef USE_EXPAT
typedef ExpatToXerces DefaultHandler;
#endif  /* USE_EXPAT */


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
  SBMLHandler (SBMLDocument* d);

  /**
   * Dtor
   */
  ~SBMLHandler ();


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

  void characters (const XMLCh* const chars, const unsigned int length);

  void ignorableWhitespace
  (
    const XMLCh* const  chars,
    const unsigned int  length
  );

  void setDocumentLocator (const Locator *const locator);

  /* 
   * to use in memory schemas the handler must
   * resolve the entity
   */
  InputSource* resolveEntity(const XMLCh* const    publicId,
                             const XMLCh* const    systemId);

#ifndef USE_EXPAT
  void warning    (const SAXParseException&);
  void error      (const SAXParseException&);
  void fatalError (const SAXParseException&);

  ParseMessage* ParseMessage_createFrom (const SAXParseException& e);
#endif  /* !USE_EXPAT */

  void warning    (const char* message);
  void error      (const char* message);
  void fatalError (const char* message);

  ParseMessage* ParseMessage_createFrom (const char* message);


private:

  SBase* doSBML                      (const Attributes& a);
  SBase* doModel                     (const Attributes& a);

  SBase* doListOfFunctionDefinitions (const Attributes& a);
  SBase* doListOfUnitDefinitions     (const Attributes& a);
  SBase* doListOfUnits               (const Attributes& a);
  SBase* doListOfCompartments        (const Attributes& a);
  SBase* doListOfSpecies             (const Attributes& a);
  SBase* doListOfParameters          (const Attributes& a);
  SBase* doListOfRules               (const Attributes& a);
  SBase* doListOfReactions           (const Attributes& a);
  SBase* doListOfReactants           (const Attributes& a);
  SBase* doListOfProducts            (const Attributes& a);
  SBase* doListOfModifiers           (const Attributes& a);
  SBase* doListOfEvents              (const Attributes& a);
  SBase* doListOfEventAssignments    (const Attributes& a);

  SBase* doFunctionDefinition        (const Attributes& a);
  SBase* doUnitDefinition            (const Attributes& a);
  SBase* doUnit                      (const Attributes& a);
  SBase* doCompartment               (const Attributes& a);
  SBase* doSpecies                   (const Attributes& a);
  SBase* doParameter                 (const Attributes& a);
  SBase* doReaction                  (const Attributes& a);
  SBase* doSpeciesReference          (const Attributes& a);
  SBase* doModifierSpeciesReference  (const Attributes& a);
  SBase* doKineticLaw                (const Attributes& a);
  SBase* doAssignmentRule            (const Attributes& a);
  SBase* doRateRule                  (const Attributes& a);
  SBase* doAlgebraicRule             (const Attributes& a);
  SBase* doCompartmentVolumeRule     (const Attributes& a);
  SBase* doParameterRule             (const Attributes& a);
  SBase* doSpeciesConcentrationRule  (const Attributes& a);
  SBase* doEvent                     (const Attributes& a);
  SBase* doEventAssignment           (const Attributes& a);
  SBase* doStackPeek                 (const Attributes& a);

  SBMLTagCode_t getTagCode (const XMLCh *uri, const XMLCh* localname);

  void setLineAndColumn     (SBase *sb);
  void setMath              (ASTNode* math);
  void setStoichiometryMath (SpeciesReference* sr, ASTNode* math);


#ifdef USE_LAYOUT
  /**
   * Sets the id attribute of a SpeciesReference.
   */
  std::string doSpeciesReferenceId (const Attributes& a);
#endif  /* USE_LAYOUT */


  /*
  void debugPrintStartElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname,
    const Attributes&   attrs
  );

  void debugPrintAttrs (const Attributes& attrs);
  */



  SBMLDocument* fDocument;
  Model*        fModel;
  Stack_t*      fObjStack;
  Stack_t*      fTagStack;

  XMLStringFormatter* fFormatter;

  MathMLHandler*  fMathHandler;
  MathMLDocument* fMathDocument;

#ifdef USE_LAYOUT
  LayoutHandler* fLayoutHandler;
#endif  /* USE_LAYOUT */

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

#ifdef USE_LAYOUT
  int inLayout;
#endif  /* USE_LAYOUT */
};


/**
 * @return a duplicate of s with the last character removed.  Free the
 * returned string delete [ ].
 */
XMLCh*
removeLastChar (const XMLCh* const s);

/**
 * @return true if prefix begins with 'xmlns:' (case-insensitive), false
 * otherwise.
 */
bool
startsWithXMLNS (const XMLCh* const prefix);

/**
 * Stores any namespace definitions (attribute names that begin with
 * xmlns:) in the SBase object's collection of namespaces.
 */
void
storeNamespaceDefinitions (SBase *obj, const Attributes& a);


#endif  /* __cplusplus */
#endif  /* SBMLHandler_h */
