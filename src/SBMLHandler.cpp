/**
 * Filename    : SBMLHandler.cpp
 * Description : Register with XML Parser to process an SBML document
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
#include "sbml/List.h"


#ifdef USE_EXPAT
#  include <expat.h>
#  include "ExpatXMLString.hpp"
#else
#  include <xercesc/sax2/Attributes.hpp>
#  include <xercesc/util/XMLString.hpp>
#endif  // USE_EXPAT


#include "sbml/FormulaFormatter.h"

#include "sbml/SBMLUnicodeConstants.hpp"
#include "sbml/XMLStringFormatter.hpp"
#include "sbml/XMLUtil.hpp"

#include "sbml/SBMLHandler.hpp"
#include "sbml/ParseMessage.h"


const TagHandler_t
SBMLHandler::TagHandler[] =
{
    /* SBML tag                        SBMLHandler method                  */
    /* ----------------------------   ------------------------------------ */
    /* <algebraicRule>             */ &SBMLHandler::doAlgebraicRule
  , /* <annotation>                */ NULL
  , /* <annotations>               */ NULL
  , /* <assignmentRule>            */ &SBMLHandler::doAssignmentRule
  , /* <compartment>               */ &SBMLHandler::doCompartment
  , /* <compartmentVolumeRule>     */ &SBMLHandler::doCompartmentVolumeRule
  , /* <delay>                     */ &SBMLHandler::doStackPeek
  , /* <event>                     */ &SBMLHandler::doEvent
  , /* <eventAssignment>           */ &SBMLHandler::doEventAssignment
  , /* <functionDefinition>        */ &SBMLHandler::doFunctionDefinition
  , /* <kineticLaw>                */ &SBMLHandler::doKineticLaw
  , /* <listOfCompartments>        */ &SBMLHandler::doListOfCompartments
  , /* <listOfEventassignments>    */ &SBMLHandler::doListOfEventAssignments
  , /* <listOfEvents>              */ &SBMLHandler::doListOfEvents
  , /* <listOfFunctionDefinitions> */ &SBMLHandler::doListOfFunctionDefinitions
  , /* <listOfModifiers>           */ &SBMLHandler::doListOfModifiers
  , /* <listOfParameters>          */ &SBMLHandler::doListOfParameters
  , /* <listOfProducts>            */ &SBMLHandler::doListOfProducts
  , /* <listOfReactants>           */ &SBMLHandler::doListOfReactants
  , /* <listOfReactions>           */ &SBMLHandler::doListOfReactions
  , /* <listOfRules>               */ &SBMLHandler::doListOfRules
  , /* <listOfSpecies>             */ &SBMLHandler::doListOfSpecies
  , /* <listOfUnitDefinitions>     */ &SBMLHandler::doListOfUnitDefinitions
  , /* <listOfUnits>               */ &SBMLHandler::doListOfUnits
  , /* <math>                      */ NULL
  , /* <model>                     */ &SBMLHandler::doModel
  , /* <modifierSpeciesReference>  */ &SBMLHandler::doModifierSpeciesReference
  , /* <notes>                     */ NULL
  , /* <parameter>                 */ &SBMLHandler::doParameter
  , /* <parameterRule>             */ &SBMLHandler::doParameterRule
  , /* <rateRule>                  */ &SBMLHandler::doRateRule
  , /* <reaction>                  */ &SBMLHandler::doReaction
  , /* <sbml>                      */ &SBMLHandler::doSBML
  , /* <specie>                    */ &SBMLHandler::doSpecies
  , /* <specieConcentrationRule>   */ &SBMLHandler::doSpeciesConcentrationRule
  , /* <specieReference>           */ &SBMLHandler::doSpeciesReference
  , /* <species>                   */ &SBMLHandler::doSpecies
  , /* <SpeciesConcentrationRule>  */ &SBMLHandler::doSpeciesConcentrationRule
  , /* <speciesReference>          */ &SBMLHandler::doSpeciesReference
  , /* <stoichiometryMath>         */ &SBMLHandler::doStackPeek
  , /* <trigger>                   */ &SBMLHandler::doStackPeek
  , /* <unit>                      */ &SBMLHandler::doUnit
  , /* <unitDefinition>            */ &SBMLHandler::doUnitDefinition
};




/**
 * Ctor
 */
SBMLHandler::SBMLHandler (SBMLDocument* d) : fDocument(d)
{

#ifdef USE_EXPAT
   create();
#endif  // USE_EXPAT

  //
  // An XMLStringFormatter is used to reconstruct <notes> and <annotation>
  // sections from SAX2 events.
  //
  fFormatter = new XMLStringFormatter("ASCII");

  //
  // MathML is parsed by delegating SAX2 events recieved by this handler to
  // a MathMLHandler.
  //
  fMathDocument = new MathMLDocument;
  fMathHandler  = new MathMLHandler( fMathDocument );

  //
  // Two separate but parallel stacks are used: one to track XML elements
  // (XML tags) and the other, each element's corresponding SBML objects.
  //
  // For <listOf...> elements, a sentinal NULL is placed on the SBML object
  // stack.  <notes>, <annotation> and any elements contained within them
  // are never recorded on either stack.
  //
  // The stacks will double in size automatically if their initial capacity
  // is exceeded.  But why rely on this if we can avoid it in most cases?
  // By my calculations, the deepest the stacks can get for SBML documents
  // is 7:
  //
  // 1: <sbml level='1' version='1'>
  // 2:   <model name='myModel'>
  // 3:    <listOfReactions>
  // 4:      <reaction name='r1'>
  // 5:         <kineticLaw formula='k*S0'>
  // 6:           <listOfParameters>
  // 7:             <parameter name='foo' value='1'>
  //
  fObjStack = Stack_create(7);
  fTagStack = Stack_create(7);

  inNotes      = 0;
  inAnnotation = 0;
  inMath       = 0;
}


/**
 * Dtor
 */
SBMLHandler::~SBMLHandler ()
{
  Stack_free( fObjStack );
  Stack_free( fTagStack );

  delete fMathDocument;
}




/* ----------------------------------------------------------------------
 *                         SAX2 Event Handlers
 * ----------------------------------------------------------------------
 */


/**
 * startElement
 */
#ifdef USE_EXPAT
void SBMLHandler::onStartElement(const XML_Char *localname,
                                 const XML_Char **papszAttrs)
#else
void
SBMLHandler::startElement (const XMLCh* const  uri,
                           const XMLCh* const  localname,
                           const XMLCh* const  qname,
                           const Attributes&   attrs)
#endif  // USE_EXPAT
{

#ifdef USE_EXPAT
  XMLCh*              uri = NULL;
  Attributes          attrs(papszAttrs);
  const XMLCh* const  qname = localname;
#endif  // USE_EXPAT

  SBase*        obj = NULL;
  SBMLTagCode_t tag = getTagCode(uri, localname);


  /* debugPrintStartElement(uri, localname, qname, attrs); */

  //
  // If we are already inside an <annotation>, <notes> or <math> tag
  // delegate to the appropriate sub-handler.
  //
  if (inAnnotation)
  {
    fFormatter->startElement(qname, attrs);

    //
    // Track nested <annotation> tags.
    //
    if (tag == TAG_ANNOTATION || tag == TAG_ANNOTATIONS)
    {
      inAnnotation++;
    }
  }
  else if (inNotes)
  {
    fFormatter->startElement(qname, attrs);

    //
    // Track nested <notes> tags.  While this is technically not proper
    // SBML, it's easy to be a little bit more robust.
    //
    if (tag == TAG_NOTES)
    {
      warning("<notes> elements cannot be nested.");
      inNotes++;
    }
  }
  else if (inMath)
  {
#ifdef USE_EXPAT
    fMathHandler->onStartElement(localname, papszAttrs);
#else
    fMathHandler->startElement(uri, localname, qname, attrs);
#endif  // USE_EXPAT
  }

  //
  // Otherwise, check for the special tags <annotation>, <notes> or <math>
  // and delegate to the appropriate sub-handler.
  //
  else if (tag == TAG_ANNOTATION || tag == TAG_ANNOTATIONS)
  {
#ifdef USE_EXPAT
    if (!inAnnotation) enableCharacterDataHandler();
#endif  // USE_EXPAT
    fFormatter->startElement(qname, attrs);

    inAnnotation++;
  }
  else if (tag == TAG_NOTES)
  {
#ifdef USE_EXPAT
    if (!inNotes) enableCharacterDataHandler();
#endif  // USE_EXPAT

    inNotes++;
  }
  else if (tag == TAG_MATH)
  {
#ifdef USE_EXPAT
    if (!inMath) enableCharacterDataHandler();
#endif  // USE_EXPAT

     fMathHandler->startDocument();

#ifdef USE_EXPAT
    fMathHandler->onStartElement(localname, papszAttrs);
#else
    fMathHandler->startElement(uri, localname, qname, attrs);
#endif  // USE_EXPAT

    inMath++;
  }

  //
  // Finally, if none of the above conditions were true, proccess the SBML
  // tag.
  //
  else if (tag != TAG_UNKNOWN)
  {
    obj = (this->*TagHandler[tag])(attrs);

    if (obj != NULL)
    {
      setLineAndColumn(obj);

      //
      // metaid: ID  { use="optional" }  (L2v1)
      //
      XMLUtil::scanAttr(attrs, ATTR_META_ID, obj->metaid);
    }

    Stack_push(fTagStack, (void *) tag);
    Stack_push(fObjStack, obj);
  }
}


/**
 * endElement()
 *
 * FIXME: This method has grown quite hairy and is in desperate need of
 * FIXME: refactoring.
 */
#ifdef USE_EXPAT
void SBMLHandler::onEndElement(const XML_Char *localname)
#else
void
SBMLHandler::endElement (const XMLCh* const  uri,
                         const XMLCh* const  localname,
                         const XMLCh* const  qname)
#endif  // USE_EXPAT
{

#ifdef USE_EXPAT
  XMLCh*              uri   = NULL;
  const XMLCh* const  qname = localname;
#endif  // USE_EXPAT

  static const char ERRMSG_NO_SBML_NOTE[] =
    "The <sbml> element cannot contain a <note>.  "
    "Use the <model> element instead.";

  static const char ERRMSG_NO_SBML_ANNOTATION[] =
    "The <sbml> element cannot contain an <annotation>.  "
    "Use the <model> element instead.";


  SBase*        obj = (SBase*) Stack_peek(fObjStack);
  SBMLTagCode_t tag = getTagCode(uri, localname);


  //
  // Notes
  //
  if (tag == TAG_NOTES)
  {
    if (inNotes > 1)
    {
      fFormatter->endElement(qname);
    }
    else if (inNotes == 1)
    {
      if ((obj->getTypeCode() == SBML_DOCUMENT) && (fDocument->level == 1))
      {
        error(ERRMSG_NO_SBML_NOTE);
      }

      SBase_setNotes(obj, fFormatter->getString());
      fFormatter->reset();
    }

    inNotes--;

#ifdef USE_EXPAT
    if (!inNotes) enableCharacterDataHandler(false);
#endif  // USE_EXPAT
  }

  //
  // Annotation
  //
  else if (tag == TAG_ANNOTATION || tag == TAG_ANNOTATIONS)
  {
    fFormatter->endElement(qname);

    if (inAnnotation == 1)
    {
      if ((obj->getTypeCode() == SBML_DOCUMENT) && (fDocument->level == 1))
      {
        error(ERRMSG_NO_SBML_ANNOTATION);
      }

      SBase_setAnnotation(obj, fFormatter->getString());
      fFormatter->reset();
    }

    inAnnotation--;

#ifdef USE_EXPAT
    if (!inAnnotation) enableCharacterDataHandler(false);
#endif  // USE_EXPAT
  }

  //
  // MathML
  //
  else if (tag == TAG_MATH && !(inNotes || inAnnotation))
  {
#ifdef USE_EXPAT
    fMathHandler->onEndElement(localname);
#else
    fMathHandler->endElement(uri, localname, qname);
#endif  // USE_EXPAT

    fMathHandler->endDocument();
    setMath(fMathDocument->math);
    fMathDocument->math = NULL;

    inMath--;

#ifdef USE_EXPAT
    if (!inMath) enableCharacterDataHandler(false);
#endif  // USE_EXPAT
  }

  else if (inNotes || inAnnotation)
  {
    fFormatter->endElement(qname);
  }
  else if (inMath)
  {
#ifdef USE_EXPAT
    fMathHandler->onEndElement(localname);
#else
    fMathHandler->endElement(uri, localname, qname);
#endif  // !USE_EXPAT
  }
  else if (tag != TAG_UNKNOWN)
  {
    Stack_pop(fTagStack);
    Stack_pop(fObjStack);
  }
}


/**
 * Characters are either part of <notes>, <annotation> or MathML <cn> and
 * <ci> elements.  Everything else is ignored.
 */
#ifdef USE_EXPAT
void SBMLHandler::onCharacterData(const XML_Char *chars, int length)
#else
void
SBMLHandler::characters (const XMLCh* const  chars,
                         const unsigned int  length)
#endif  // USE_EXPAT
{
  if (inNotes || inAnnotation)
  {
    fFormatter->characters(chars, length);
  }
  else if (inMath)
  {
#ifdef USE_EXPAT
    fMathHandler->onCharacterData(chars, length);
#else
    fMathHandler->characters(chars, length);
#endif  // USE_EXPAT
  }
}


/**
 * Ignorable whitespace is recorded for <notes> and <annotation> elements
 * in the interest of exactly reproducing their content.
 */
void
SBMLHandler::ignorableWhitespace (const XMLCh* const  chars,
                                  const unsigned int  length)
{
  if (inNotes || inAnnotation)
  {
    fFormatter->ignorableWhitespace(chars, length);
  }
}


/**
 * A SAX2 XMLReader uses this method to register a document Locator with
 * this handler.  Locators track line and column numbers during the parse,
 * which are used when creating warning or error messages.
 */
void
SBMLHandler::setDocumentLocator (const Locator *const locator)
{
  fLocator = locator;
  fMathHandler->setDocumentLocator(locator);
}


/* ----------------------------------------------------------------------
 *                         SAX2 Error Handlers
 * ----------------------------------------------------------------------
 */


#ifndef USE_EXPAT
void
SBMLHandler::warning (const SAXParseException& e)
{
  fDocument->warning.add( ParseMessage_createFrom(e) );
}


void
SBMLHandler::error (const SAXParseException& e)
{
  fDocument->error.add( ParseMessage_createFrom(e) );
}


void
SBMLHandler::fatalError (const SAXParseException& e)
{
  fDocument->fatal.add( ParseMessage_createFrom(e) );
}
#endif  // !USE_EXPAT


/* ----------------------------------------------------------------------
 *                          Custom Error Handlers
 * ----------------------------------------------------------------------
 */


void
SBMLHandler::warning (const char* message)
{
  fDocument->warning.add( ParseMessage_createFrom(message) );
}


void
SBMLHandler::error (const char* message)
{
  fDocument->error.add( ParseMessage_createFrom(message) );
}


void
SBMLHandler::fatalError (const char* message)
{
  fDocument->fatal.add( ParseMessage_createFrom(message) );
}


/**
 * Creates a new ParseMessage from the given message and returns a pointer
 * to it.
 *
 * The line and column number where the error occurred are obtained from
 * this handler's document Locator and are stored in the ParseMessage.
 */
ParseMessage_t*
SBMLHandler::ParseMessage_createFrom (const char* message)
{
  return
#ifdef USE_EXPAT
    ParseMessage_createWith( message, 
                             getCurrentLineNumber(),
                             getCurrentColumnNumber() );
#else
    ParseMessage_createWith( message, 
                             (unsigned int) fLocator->getLineNumber(),
                             (unsigned int) fLocator->getColumnNumber() );
#endif  // USE_EXPAT
}


#ifndef USE_EXPAT
/**
 * Creates a new ParseMessage from the given SAXException and returns a
 * pointer to it.
 *
 * The exception's message will be the text of the ParseMessage.  The line
 * and column number where the error occurred are obtained from this
 * handler's document Locator and are also stored in the ParseMessage.
 */
ParseMessage_t*
SBMLHandler::ParseMessage_createFrom (const SAXParseException& e)
{
  char*           message;
  ParseMessage_t* pm;


  message = XMLString::transcode( e.getMessage() );

  pm = ParseMessage_createWith( message, 
                                (unsigned int) e.getLineNumber(),
                                (unsigned int) e.getColumnNumber() );

  XMLString::release(&message);

  return pm;
}
#endif  // !USE_EXPAT


/* ----------------------------------------------------------------------
 *                          SBML Tag Handlers
 * ----------------------------------------------------------------------
 */


/**
 * Initializes the SBMLDocument fDocument from the given XML attributes.
 */
SBase*
SBMLHandler::doSBML (const Attributes& a)
{
  XMLUtil::scanAttr( a, ATTR_LEVEL  , &(fDocument->level)   );
  XMLUtil::scanAttr( a, ATTR_VERSION, &(fDocument->version) );

  return fDocument;
}


/**
 * Adds a new Model to the SBMLDocument being read and returns a pointer to
 * it.  The Model is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doModel (const Attributes& a)
{
  fModel = &fDocument->createModel();

  //
  // id: SId  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, fModel->id);

  //
  // name: SName   { use="optional" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_NAME, fModel->name);

  return fModel;
}


/**
 * @return the list of FunctionDefinitions for the Model being read.
 */
SBase*
SBMLHandler::doListOfFunctionDefinitions (const Attributes& a)
{
  return & fModel->getListOfFunctionDefinitions();
}


/**
 * @return the list of UnitDefinitions for the Model being read.
 */
SBase*
SBMLHandler::doListOfUnitDefinitions (const Attributes& a)
{
  return & fModel->getListOfUnitDefinitions();
}


/**
 * @return the list of Units for the UnitDefinition being read.
 */
SBase*
SBMLHandler::doListOfUnits (const Attributes& a)
{
  SBase*          obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  ListOf*         lo  = NULL;
  UnitDefinition* ud;


  if (obj->getTypeCode() == SBML_UNIT_DEFINITION)
  {
    ud = static_cast<UnitDefinition*>(obj);
    lo = & ud->getListOfUnits();
  }

  return lo;
}


/**
 * @return the list of Compartments for the Model being read.
 */
SBase*
SBMLHandler::doListOfCompartments (const Attributes& a)
{
  return & fModel->getListOfCompartments();
}


/**
 * @return the list of Species for the Model being read.
 */
SBase*
SBMLHandler::doListOfSpecies (const Attributes& a)
{
  return & fModel->getListOfSpecies();
}


/**
 * @return the list of Parameters for either the Model or KineticLaw being
 * read.  The context is determined by the top object on fObjStack.
 */
SBase*
SBMLHandler::doListOfParameters (const Attributes& a)
{
  SBase*      obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  ListOf*     lo  = NULL;
  KineticLaw* kl;

  if (obj->getTypeCode() == SBML_KINETIC_LAW)
  {
    kl = static_cast<KineticLaw*>(obj);
    lo = & kl->getListOfParameters();
  }
  else
  {
    lo = & fModel->getListOfParameters();
  }

  return lo;
}


/**
 * @return the list of Rules for the Model being read.
 */
SBase*
SBMLHandler::doListOfRules (const Attributes& a)
{
  return & fModel->getListOfRules();
}


/**
 * @return the list of Reactions for the Model being read.
 */
SBase*
SBMLHandler::doListOfReactions (const Attributes& a)
{
  return & fModel->getListOfReactions();
}


/**
 * @return the list of Reactants for the Reaction being read.
 */
SBase*
SBMLHandler::doListOfReactants (const Attributes& a)
{
  SBase*    obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  ListOf*   lo  = NULL;
  Reaction* r;


  if (obj->getTypeCode() == SBML_REACTION)
  {
    r  = static_cast<Reaction*>(obj);
    lo = & r->getListOfReactants();
  }

  return lo;
}


/**
 * @return the list of Products for the Reaction being read.
 */
SBase*
SBMLHandler::doListOfProducts (const Attributes& a)
{
  SBase*    obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  ListOf*   lo  = NULL;
  Reaction* r;


  if (obj->getTypeCode() == SBML_REACTION)
  {
    r  = static_cast<Reaction*>(obj);
    lo = & r->getListOfProducts();
  }

  return lo;
}


/**
 * @return the list of Modifiers for the Reaction being read.
 */
SBase*
SBMLHandler::doListOfModifiers (const Attributes& a)
{
  SBase*    obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  ListOf*   lo  = NULL;
  Reaction* r;


  if (obj->getTypeCode() == SBML_REACTION)
  {
    r  = static_cast<Reaction*>(obj);
    lo = & r->getListOfModifiers();
  }

  return lo;
}


/**
 * @return the list of Events for the Model being read.
 */
SBase*
SBMLHandler::doListOfEvents (const Attributes& a)
{
  return & fModel->getListOfEvents();
}


/**
 * @return the list of EventAssignments for the Event being read.
 */
SBase*
SBMLHandler::doListOfEventAssignments (const Attributes& a)
{
  SBase*  obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  ListOf* lo  = NULL;
  Event*  e;


  if (obj->getTypeCode() == SBML_EVENT)
  {
    e  = static_cast<Event*>(obj);
    lo = & e->getListOfEventAssignments();
  }

  return lo;
}


/**
 * Adds a new FunctionDefinition to the Model being read and returns a
 * pointer to it.  The FunctionDefinition is initialized from the given XML
 * attributes.
 */
SBase*
SBMLHandler::doFunctionDefinition (const Attributes& a)
{
  FunctionDefinition* fd = & fModel->createFunctionDefinition();


  //
  // id: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, fd->id);

  //
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_NAME, fd->name);

  return fd;
}


/**
 * Adds a new UnitDefinition to the Model being read and returns a pointer
 * to it.  The UnitDefinition is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doUnitDefinition (const Attributes& a)
{
  UnitDefinition* ud = & fModel->createUnitDefinition();


  //
  // id: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, ud->id);

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr( a, ATTR_NAME, ud->name );

  return ud;
}


/**
 * Adds a new Unit to the Model being read and returns a pointer to it.
 * The Unit is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doUnit (const Attributes& a)
{
  Unit*  u    = fModel->createUnit();
  char*  kind = XMLString::transcode( a.getValue(ATTR_KIND) ); 

  int    ivalue;
  double dvalue;


  //
  // kind: UnitKind  (L1v1, L1v2, L2v1)
  //
  u->kind = UnitKind_forName(kind);
  XMLString::release(&kind);

  //
  // exponent: integer  { use="optional" default="1" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_EXPONENT, &ivalue) == true)
  {
    u->setExponent(ivalue);
  }

  //
  // scale: integer  { use="optional" default="0" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_SCALE, &ivalue) == true)
  {
    u->setScale(ivalue);
  }

  //
  // multiplier: double  { use="optional" default="1" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_MULTIPLIER, &dvalue) == true)
  {
    u->setMultiplier(dvalue);
  }

  //
  // offset: double  { use="optional" default="0" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_OFFSET, &dvalue) == true)
  {
    u->setOffset(dvalue);
  }

  return u;
}


/**
 * Adds a new Compartment to the Model being read and returns a pointer to
 * it.  The Compartment is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doCompartment (const Attributes& a)
{
  Compartment* c = & fModel->createCompartment();

  bool   bvalue;
  double dvalue;
  int    ivalue;


  //
  // id: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, c->id);

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_NAME, c->name);

  //
  // spatialDimensions: integer  { use="optional" default="3" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_SPATIAL_DIMENSIONS, &ivalue) == true)
  {
    c->setSpatialDimensions(ivalue);
  }

  //
  // volume: double  { use="optional" default="1" }  (L1v1, L1v2)
  //
  if (XMLUtil::scanAttr(a, ATTR_VOLUME, &dvalue) == true)
  {
    c->setVolume(dvalue);
  }

  //
  // size: double  { use="optional" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_SIZE, &dvalue) == true)
  {
    c->setSize(dvalue);
  }

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  // units: SId    { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_UNITS, c->units);

  //
  // outside: SName  { use="optional" }  (L1v1, L1v2)
  // outside: SId    { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr( a, ATTR_OUTSIDE, c->outside );

  //
  // constant: boolean  { use="optional" default="true" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_CONSTANT, &bvalue) == true)
  {
    c->setConstant(bvalue);
  }

  return c;
}


/**
 * Adds a new Species to the Model being read and returns a pointer to it.
 * The Species is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doSpecies (const Attributes& a)
{
  Species* s = & fModel->createSpecies();

  bool   bvalue;
  double dvalue;
  int    ivalue;


  //
  // id: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, s->id);

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_NAME , s->name);

  //
  // compartment: SName   { use="required" }  (L1v1, L1v2)
  // compartment: SId     { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_COMPARTMENT, s->compartment);

  //
  // initialAmount: double  { use="required" }  (L1v1, L1v2)
  // initialAmount: double  { use="optional" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_INITIAL_AMOUNT, &dvalue) == true)
  {
    s->setInitialAmount(dvalue);
  }

  //
  // initialConcentration: double  { use="optional" }  (L2v1)
  //
  else if (XMLUtil::scanAttr(a, ATTR_INITIAL_CONCENTRATION, &dvalue) == true)
  {
    s->setInitialConcentration(dvalue);
  }

  //
  //          units: SName  { use="optional" }  (L1v1, L1v2)
  // substanceUnits: SId    { use="optional" }  (L2v1)
  //
  ivalue = a.getIndex(ATTR_UNITS);
  if (ivalue >= 0)
  {
    XMLUtil::scanAttr(a, ivalue, s->substanceUnits);
  }
  else
  {
    XMLUtil::scanAttr(a, ATTR_SUBSTANCE_UNITS, s->substanceUnits);
  }

  //
  // spatialSizeUnits: SId  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_SPATIAL_SIZE_UNITS, s->spatialSizeUnits);

  //
  // hasOnlySubstanceUnits: boolean  { use="optional" default="true" }
  // (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_HAS_ONLY_SUBSTANCE_UNITS, &bvalue) == true)
  {
    s->setHasOnlySubstanceUnits(bvalue);
  }

  //
  // boundaryCondition: boolean  { use="optional" default="false" }
  // (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_BOUNDARY_CONDITION, &bvalue) == true)
  {
    s->setBoundaryCondition(bvalue);
  }

  //
  // charge: integer  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_CHARGE, &ivalue) == true)
  {
    s->setCharge(ivalue);
  }

  //
  // constant: boolean  { use="optional" default="false" }  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_CONSTANT, &bvalue) == true)
  {
    s->setConstant(bvalue);
  }

  return s;
}


/**
 * Adds a new Parameter (either global or local to a reaction) to the Model
 * being read and returns a pointer to it.  The Parameter is initialized
 * from the given XML attributes.
 */
SBase*
SBMLHandler::doParameter (const Attributes& a)
{
  Parameter*    p   = NULL;
  SBMLTagCode_t tag = (SBMLTagCode_t) Stack_peekAt(fTagStack, 1);

  bool   bvalue;
  double dvalue;


  //
  // Determine if this is a global parameter or local to a particular
  // reaction.
  //
  if (tag == TAG_KINETIC_LAW)
  {
    p = fModel->createKineticLawParameter();
  }
  else
  {
    p = & fModel->createParameter();
  }

  if (p != NULL)
  {
    //
    // id: SId  { use="required" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_ID, p->id);

    //
    // name: SName   { use="required" }  (L1v1, L1v2)
    // name: string  { use="optional" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_NAME, p->name);

    //
    // value: double  { use="required" }  (L1v1)
    // value: double  { use="optional" }  (L1v2, L2v1)
    //
    if (XMLUtil::scanAttr(a, ATTR_VALUE, &dvalue) == true)
    {
      p->setValue(dvalue);
    }

    //
    // units: SName  { use="optional" }  (L1v1, L1v2)
    // units: SId    { use="optional" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_UNITS, p->units);

    //
    // constant: boolean  { use="optional" default="true" }  (L2v1)
    //
    if (XMLUtil::scanAttr(a, ATTR_CONSTANT, &bvalue) == true)
    {
      p->setConstant(bvalue);
    }
  }

  return p;
}


/**
 * Adds a new Reaction to the Model being read and returns a pointer to it.
 * The Reaction is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doReaction (const Attributes& a)
{
  Reaction* r = & fModel->createReaction();

  bool bvalue;


  //
  // id: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, r->id);

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_NAME, r->name);

  //
  // reversible: boolean  { use="optional" default="true" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_REVERSIBLE, &bvalue) == true)
  {
    r->setReversible(bvalue);
  }

  //
  // fast: boolean  { use="optional" default="false" }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }                  (L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_FAST, &bvalue) == true)
  {
    r->setFast(bvalue);
  }

  return r;
}


/**
 * Adds a new SpeciesReference (as a Reactant or Product) to the Reaction
 * being read and returns a pointer to it.  The SpeciesReference is
 * initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doSpeciesReference (const Attributes& a)
{
  SpeciesReference* sr    = NULL;
  SBMLTagCode_t     tag   = (SBMLTagCode_t) Stack_peek(fTagStack);

  int index;


  //
  // Determine if this SpeciesReference is a reactant or product.
  //
  if (tag == TAG_LIST_OF_REACTANTS)
  {
    sr = fModel->createReactant();
  }
  else if (tag == TAG_LIST_OF_PRODUCTS)
  {
    sr = fModel->createProduct();
  }

  if (sr != NULL)
  {
    //
    // specie : SName   { use="required" }  (L1v1)
    // species: SName   { use="required" }  (L1v2)
    // species: SId     { use="required" }  (L2v1)
    //
    // Look first for "species" and if not found, look for "specie".
    //
    index = a.getIndex(ATTR_SPECIES);
    if (index >= 0)
    {
      XMLUtil::scanAttr(a, index, sr->species);
    }
    else
    {
      XMLUtil::scanAttr(a, ATTR_SPECIE, sr->species);
    }

    //
    // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
    // stoichiometry: double   { use="optional" default="1" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_STOICHIOMETRY, &(sr->stoichiometry));

    //
    // denominator: integer  { use="optional" default="1" }  (L1v1, L1v2)
    //
    XMLUtil::scanAttr(a, ATTR_DENOMINATOR, &(sr->denominator));
  }

  return sr;
}


/**
 * Adds a new ModifierSpeciesReference (as a modifier) to the Reaction
 * being read and returns a pointer to it.  The ModifierSpeciesReference is
 * initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doModifierSpeciesReference (const Attributes& a)
{
  ModifierSpeciesReference* msr = fModel->createModifier();
  int index;


  if (msr != NULL)
  {
    //
    // species: SId  { use="required" }  (L2v1)
    //
    // Look first for "species" and if not found, look for "specie".
    // Even though "specie" is not allowed, why not be robust?
    //
    index = a.getIndex(ATTR_SPECIES);
    if (index >= 0)
    {
      XMLUtil::scanAttr(a, index, msr->species);
    }
    else
    {
      XMLUtil::scanAttr(a, ATTR_SPECIE, msr->species);
    }
  }

  return msr;
}


/**
 * Adds a new KineticLaw to the Reaction being read and returns a pointer
 * to it.  The KineticLaw is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doKineticLaw (const Attributes& a)
{
  KineticLaw* kl = fModel->createKineticLaw();


  if (kl != NULL)
  {
    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    XMLUtil::scanAttr(a, ATTR_FORMULA, kl->formula);

    //
    // timeUnits: SName  { use="optional" }  (L1v1, L1v2)
    // timeUnits: SId    { use="optional" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_TIME_UNITS, kl->timeUnits);

    //
    // substanceUnits: SName  { use="optional" }  (L1v1, L1v2)
    // substanceUnits: SId    { use="optional" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_SUBSTANCE_UNITS, kl->substanceUnits);
  }

  return kl;
}


/**
 * Adds a new AssignmentRule to the Model being read and returns a pointer
 * to it.  The AssignmentRule is initialized from the given XML attributes.
 *
 * (L2 only)
 */
SBase*
SBMLHandler::doAssignmentRule (const Attributes& a)
{
  AssignmentRule* ar = & fModel->createAssignmentRule();


  //
  // variable: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_VARIABLE, ar->variable);

  return ar;
}


/**
 * Adds a new RateRule to the Model being read and returns a pointer
 * to it.  The RateRule is initialized from the given XML attributes.
 *
 * (L2 only)
 */
SBase*
SBMLHandler::doRateRule (const Attributes& a)
{
  RateRule* rr = & fModel->createRateRule();


  //
  // variable: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_VARIABLE, rr->variable);

  return rr;
}


/**
 * Adds a new AlgebraicRule to the Model being read and returns a pointer
 * to it.  The AlgebraicRule is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doAlgebraicRule (const Attributes& a)
{
  AlgebraicRule* ar = & fModel->createAlgebraicRule();


  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  XMLUtil::scanAttr(a, ATTR_FORMULA, ar->formula);

  return ar;
}


/**
 * Adds a new CompartmentVolume to the Model being read and returns a
 * pointer to it.  The CompartmentVolume is initialized from the given XML
 * attributes.
 *
 * (L1 only)
 */
SBase*
SBMLHandler::doCompartmentVolumeRule (const Attributes& a)
{
  CompartmentVolumeRule* cvr = & fModel->createCompartmentVolumeRule();
  int index;


  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  XMLUtil::scanAttr(a, ATTR_FORMULA, cvr->formula);

  //
  // type { use="optional" default="scalar" }  (L1v1, L1v2)
  //
  index = a.getIndex(ATTR_TYPE);
  if (index > 0)
  {
    char* type = XMLString::transcode( a.getValue(index) );
    cvr->type  = RuleType_forName(type);
    XMLString::release(&type);
  }

  //
  // compartment: SName  { use="required" }  (L1v1, L1v2)
  //
  // In L2 CompartmentVolumeRule has been removed ('compartment' is
  // replaced by 'variable' and 'variable' is inherited from
  // AssignmentRule).
  //
  XMLUtil::scanAttr( a, ATTR_COMPARTMENT, cvr->variable );

  return cvr;
}


/**
 * Adds a new ParameterRule to the Model being read and returns a pointer
 * to it.  The ParameterRule is initialized from the given XML attributes.
 *
 * (L1 only)
 */
SBase*
SBMLHandler::doParameterRule (const Attributes& a)
{
  ParameterRule* pr = & fModel->createParameterRule();
  int index;


  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  XMLUtil::scanAttr(a, ATTR_FORMULA, pr->formula);

  //
  // type { use="optional" value="scalar" }  (L1v1, L1v2)
  //
  index = a.getIndex(ATTR_TYPE);
  if (index > 0)
  {
    char* type = XMLString::transcode( a.getValue(index) );
    pr->type   = RuleType_forName(type);
    XMLString::release(&type);
  }

  //
  // name: SName  { use="required" } (L1v1, L1v2)
  //
  // In L2 ParameterRule has been removed ('name' is replaced by 'variable'
  // and 'variable' is inherited from AssignmentRule).
  //
  XMLUtil::scanAttr(a, ATTR_NAME, pr->variable);

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  //
  XMLUtil::scanAttr(a, ATTR_UNITS, pr->units);

  return pr;
}


/**
 * Adds a new SpeciesConcentrationRule to the Model being read and returns
 * a pointer to it.  The SpeciesConcentrationRule is initialized from the
 * given XML attributes.
 *
 * (L1 only)
 */
SBase*
SBMLHandler::doSpeciesConcentrationRule (const Attributes& a)
{
  SpeciesConcentrationRule* scr;
  int index;


  scr = & fModel->createSpeciesConcentrationRule();

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  XMLUtil::scanAttr(a, ATTR_FORMULA, scr->formula);

  //
  // type { use="optional" value="scalar" }  (L1v1, L1v2)
  //
  index = a.getIndex(ATTR_TYPE);
  if (index >= 0)
  {
    char* type = XMLString::transcode( a.getValue(index) );
    scr->type  = RuleType_forName(type);
    XMLString::release(&type);
  }

  //
  // specie : SName   { use="required" }  (L1v1)
  // species: SName   { use="required" }  (L1v2)
  //
  // Look first for "species" and if not found, look for "specie".
  //
  // In L2 SpeciesConcentrationRule has been removed ('species' is replaced
  // by 'variable' and 'variable' is inherited from AssignmentRule).
  //
  index = a.getIndex(ATTR_SPECIES);
  if (index >= 0)
  {
    XMLUtil::scanAttr(a, index, scr->variable);
  }
  else
  {
    XMLUtil::scanAttr(a, ATTR_SPECIE, scr->variable);
  }

  return (SBase*) scr;
}


/**
 * Adds a new Event to the Model being read and returns a pointer to it.
 * The Event is initialized from the given XML attributes.
 */
SBase*
SBMLHandler::doEvent (const Attributes& a)
{
  Event* e = & fModel->createEvent();


  //
  // id: SId  { use="required" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_ID, e->id);

  //
  // name: string  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_NAME, e->name);

  //
  // timeUnits: SId  { use="optional" }  (L2v1)
  //
  XMLUtil::scanAttr(a, ATTR_TIME_UNITS, e->timeUnits);


  return e;
}


/**
 * Adds a new EventAssignment to the Event being read and returns a pointer
 * to it.  The EventAssignment is initialized from the given XML
 * attributes.
 */
SBase*
SBMLHandler::doEventAssignment (const Attributes& a)
{
  EventAssignment* ea = fModel->createEventAssignment();


  if (ea != NULL)
  {
    //
    // variable: SId  { use="required" }  (L2v1)
    //
    XMLUtil::scanAttr(a, ATTR_VARIABLE, ea->variable);
  }

  return ea;
}


/**
 * Simply returns the top object on fObjStack without popping it.
 *
 * This is useful when setting the MathML fields of some elements where
 * <math> is not an immediate sub-element (Event <delay> and <trigger> and
 * SpeciesReference <stoichiometryMath>).  By duplicating (a pointer to)
 * the top object, the implementatation of setMath() is simplified,
 * fObjStack and fTagStack stay synchronized and a NULL is not pushed onto
 * fObjStack.
 */
SBase*
SBMLHandler::doStackPeek (const Attributes& a)
{
  return static_cast<SBase*>( Stack_peek(fObjStack) );
}


/**
 * @return the SBMLTagCode for the given namespace URI and element name
 * (localname).
 */
SBMLTagCode_t
SBMLHandler::getTagCode (const XMLCh *uri, const XMLCh* localname)
{
  unsigned int  len = XMLString::stringLen(uri);
  SBMLTagCode_t tag = TAG_UNKNOWN;
  
  XMLCh ch;


  if (len == 0)
  {
    tag = SBMLTagCode_forElement(localname);
  }
  else
  {
    ch = uri[len - 1];

    if ( (ch == chDigit_2 && !XMLString::compareString(XMLNS_SBML_L2, uri)) ||
         (ch == chDigit_1 && !XMLString::compareString(XMLNS_SBML_L1, uri)) )
    {
      tag = SBMLTagCode_forElement(localname);
    }
  }

  if (tag == TAG_UNKNOWN && !XMLString::compareString(localname, ELEM_MATH))
  {
    tag = TAG_MATH;
  }

  return tag;
}


/**
 * Sets the line and column number of the given SBase object to the current
 * position in the SBML document.  If the line and column number of the
 * document are not available, this method does nothing.
 */
void
SBMLHandler::setLineAndColumn (SBase* sb)
{
  int line   = 0;
  int column = 0;


#ifdef USE_EXPAT

  line   = getCurrentLineNumber  ();
  column = getCurrentColumnNumber();

#else

  if (fLocator != 0)
  {
    line   = fLocator->getLineNumber  ();
    column = fLocator->getColumnNumber();
  }

#endif  // USE_EXPAT

  if (line > 0)
  {
    sb->line = line;
  }

  if (column > 0)
  {
    sb->column = column;
  }
}


/**
 * Sets the math field of the top object on fObjStack.  If the top object
 * has more than one MathML field, the fTagStack is examined to choose the
 * correct field.  If the top object does not contain a math field, the
 * given math AST is freed.
 *
 * @see endElement()
 */
void
SBMLHandler::setMath (ASTNode* math)
{
  SBase*        obj = static_cast<SBase*>( Stack_peek(fObjStack) );
  SBMLTagCode_t tag = (SBMLTagCode_t) Stack_peek(fTagStack);

  int freeMath = false;


  switch (obj->typecode)
  {
    case SBML_FUNCTION_DEFINITION:
      FunctionDefinition_setMath((FunctionDefinition_t*) obj, math);
      break;

    case SBML_ALGEBRAIC_RULE:
    case SBML_ASSIGNMENT_RULE:
    case SBML_RATE_RULE:
      Rule_setMath           ((Rule_t*) obj, math);
      Rule_setFormulaFromMath((Rule_t*) obj);
      break;

    case SBML_SPECIES_REFERENCE:
      setStoichiometryMath((SpeciesReference_t*) obj, math);
      break;

    case SBML_KINETIC_LAW:
      KineticLaw_setMath           ((KineticLaw_t*) obj, math);
      KineticLaw_setFormulaFromMath((KineticLaw_t*) obj);
      break;

    case SBML_EVENT:
      if (tag == TAG_TRIGGER)
      {
        Event_setTrigger((Event_t*) obj, math);
      }
      else if (tag == TAG_DELAY)
      {
        Event_setDelay((Event_t*) obj, math);
      }
      else
      {
        freeMath = true;
      }
      break;

    case SBML_EVENT_ASSIGNMENT:
      EventAssignment_setMath((EventAssignment_t*) obj, math);
      break;

    default:
      freeMath = true;
  }

  if (freeMath)
  {
    delete math;
  }
}


/**
 * Sets the stoichiometryMath of the given SpeciesReference to math.  If
 * math is a number, a simplification is performed by setting stoichiometry
 * field (in the case of AST_INTEGER, AST_REAL, AST_REAL_E or AST_RATIONAL)
 * and the denominator field (if math is AST_RATIONAL).
 */
void
SBMLHandler::setStoichiometryMath (SpeciesReference_t* sr, ASTNode* math)
{
  bool freeMath = true;


  switch ( math->getType() )
  {
    case AST_INTEGER:
      SpeciesReference_setStoichiometry(sr, math->getInteger());
      break;

    case AST_REAL:
    case AST_REAL_E:
      SpeciesReference_setStoichiometry(sr, math->getReal());
      break;

    case AST_RATIONAL:
      SpeciesReference_setStoichiometry( sr, math->getNumerator()   );
      SpeciesReference_setDenominator  ( sr, math->getDenominator() );
      break;

    default:
      SpeciesReference_setStoichiometryMath(sr, math);
      freeMath = false;
      break;
  }

  if (freeMath)
  {
    delete math;
  }
}


/**
 * Prints, to stdout, the value of parameters passed to startElement().
 *
void
SBMLHandler::debugPrintStartElement (const XMLCh* const  uri,
                                     const XMLCh* const  localname,
                                     const XMLCh* const  qname,
                                     const Attributes&   attrs)
{
  cout << "SBMLHandler::startElement(...): " << endl;
  cout << "        uri   = " << XMLString::transcode(uri)       << endl;
  cout << "  localname   = " << XMLString::transcode(localname) << endl;
  cout << "      qname   = " << XMLString::transcode(qname)     << endl;
  cout << "       line   = " << fLocator->getLineNumber()       << endl;
  cout << "       col    = " << fLocator->getColumnNumber()     << endl;
  cout << "SBMLTagCode_t = " << getTagCode(uri, localname)      << endl;
  cout << endl;
}
*/
