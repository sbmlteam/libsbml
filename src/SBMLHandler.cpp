/**
 * Filename    : SBMLHandler.cpp
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


#include <iostream>

#include "sbml/common.h"
#include "sbml/List.h"

#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/util/XMLString.hpp>

#include "sbml/SBMLHashCodes.hpp"
#include "sbml/SBMLUnicodeConstants.hpp"
#include "sbml/XMLStringFormatter.hpp"
#include "sbml/XMLUtil.hpp"

#include "sbml/SBMLHandler.hpp"


//
// Dtor
//
SBMLHandler::~SBMLHandler ()
{
  Stack_free( fObjStack );
  Stack_free( fTagStack );
}


/* ----------------------------------------------------------------------
 *                         SAX2 Event Handlers
 * ----------------------------------------------------------------------
 */


void
SBMLHandler::startDocument ()
{
  /**
   * Used to reconstruct <notes> and <annotation> sections from SAX2
   * events.
   */
  fFormatter = new XMLStringFormatter("LATIN1");

  /**
   * Two separate but parallel stacks are used: one to track XML elements
   * (XML tags) and the other, each element's corresponding SBML objects.
   *
   * For <listOf...> elements, a sentinal NULL is placed on the SBML object
   * stack.  <notes>, <annotation> and any elements contained within them
   * are never recorded on either stack.
   *
   * The stacks will double in size automatically if their initial capacity
   * is exceeded.  But why rely on this if we can avoid it in most cases?
   * By my calculations, the deepest the stacks can get for SBML documents
   * is 7:
   *
   * 1: <sbml level='1' version='1'>
   * 2:   <model name='myModel'>
   * 3:    <listOfReactions>
   * 4:      <reaction name='r1'>
   * 5:         <kineticLaw formula='k*S0'>
   * 6:           <listOfParameters>
   * 7:             <parameter name='foo' value='1'>
   */
  fObjStack = Stack_create(7);
  fTagStack = Stack_create(7);

  inNotes      = 0;
  inAnnotation = 0;
}


void
SBMLHandler::startElement (const XMLCh* const  uri,
                               const XMLCh* const  localname,
                               const XMLCh* const  qname,
                               const Attributes&   attrs)
{
  SBase_t*   obj = NULL;
  HashCode_t tag = HASH_UNKNOWN;

/*
  cout << "startElement(...): " << endl;
  cout << "        uri = " << XMLString::transcode(uri)       << endl;
  cout << "  localname = " << XMLString::transcode(localname) << endl;
  cout << "      qname = " << XMLString::transcode(qname)     << endl;
  cout << "       line = " << fLocator->getLineNumber()   << endl;
  cout << "       col  = " << fLocator->getColumnNumber() << endl;
  cout << endl;
*/

  if ( (XMLString::stringLen(uri) == 0) ||
       (XMLString::compareString(XMLNS_SBML_L1, uri) == 0) )
  {
    tag = HashCode_forElement(localname);
  }


  if (tag == HASH_NOTES)
  {
    //
    // Technically, a note like:
    //
    //   <notes> This is a test <notes>nested</notes> note. </notes>
    //
    // is not allowed.  But since someone may try to do this, the number of
    // notes elements seen (start-end pairs) needs to be tracked.
    //
    // See test: test_element_notes_nested
    //
    if (inNotes)
    {
      warning("<note> elements cannot be nested.");
      fFormatter->startElement(qname, attrs);
    }

    inNotes++;
  }
  else if (tag == HASH_ANNOTATION || tag == HASH_ANNOTATIONS)
  {
    inAnnotation++;
    fFormatter->startElement(qname, attrs);
  }
  else if (inNotes || inAnnotation)
  {
    fFormatter->startElement(qname, attrs);
  }
  else if (tag != HASH_UNKNOWN)
  {
    switch (tag)
    {
      case HASH_SBML:             obj = handleSBML            (attrs); break;
      case HASH_MODEL:            obj = handleModel           (attrs); break;
      case HASH_UNIT_DEFINITION:  obj = handleUnitDefinition  (attrs); break;
      case HASH_UNIT:             obj = handleUnit            (attrs); break;
      case HASH_COMPARTMENT:      obj = handleCompartment     (attrs); break;
      case HASH_PARAMETER:        obj = handleParameter       (attrs); break;
      case HASH_REACTION:         obj = handleReaction        (attrs); break;
      case HASH_KINETIC_LAW:      obj = handleKineticLaw      (attrs); break;
      case HASH_ALGEBRAIC_RULE:   obj = handleAlgebraicRule   (attrs); break;
      case HASH_PARAMETER_RULE:   obj = handleParameterRule   (attrs); break;

      case HASH_SPECIE:
      case HASH_SPECIES:
        obj = handleSpecies(attrs);
        break;

      case HASH_SPECIE_REFERENCE:
      case HASH_SPECIES_REFERENCE:
        obj = handleSpeciesReference(attrs);
        break;

      case HASH_COMPARTMENT_VOLUME_RULE:
        obj = handleCompartmentVolumeRule(attrs);
        break;

      case HASH_SPECIE_CONCENTRATION_RULE:
      case HASH_SPECIES_CONCENTRATION_RULE:
        obj = handleSpeciesConcentrationRule(attrs);
        break;

      default:
        break;
    }

    Stack_push(fTagStack, (void *) tag);
    Stack_push(fObjStack, obj);
  }
}


void
SBMLHandler::endElement (const XMLCh* const  uri,
                             const XMLCh* const  localname,
                             const XMLCh* const  qname)
{
  static const char ERRMSG_NO_SBML_NOTE[] =
    "The <sbml> element cannot contain a <note>.  "
    "Use the <model> element instead.";

  static const char ERRMSG_NO_SBML_ANNOTATION[] =
    "The <sbml> element cannot contain an <annotation>.  "
    "Use the <model> element instead.";


  SBase_t*   obj = (SBase_t*) Stack_peek(fObjStack);
  HashCode_t tag = HASH_UNKNOWN;


  if ( (XMLString::stringLen(uri) == 0) ||
       (XMLString::compareString(XMLNS_SBML_L1, uri) == 0) )
  {
    tag = HashCode_forElement(localname);
  }


  //
  // Notes
  //
  if (tag == HASH_NOTES)
  {
    if (inNotes > 1)
    {
      fFormatter->endElement(qname);
    }
    else if (inNotes == 1)
    {
      if (obj->typecode == SBML_DOCUMENT)
      {
        error(ERRMSG_NO_SBML_NOTE);
      }

      SBase_setNotes(obj, fFormatter->getString());
      fFormatter->reset();
    }

    inNotes--;
  }

  //
  // Annotation
  //
  else if (tag == HASH_ANNOTATION || tag == HASH_ANNOTATIONS)
  {
    fFormatter->endElement(qname);

    if (inAnnotation == 1)
    {
      if (obj->typecode == SBML_DOCUMENT)
      {
        error(ERRMSG_NO_SBML_ANNOTATION);
      }

      SBase_setAnnotation(obj, fFormatter->getString());
      fFormatter->reset();
    }

    inAnnotation--;
  }
  else if (inNotes || inAnnotation)
  {
    fFormatter->endElement(qname);
  }
  else if (tag != HASH_UNKNOWN)
  {
    Stack_pop(fTagStack);
    Stack_pop(fObjStack);
  }
}


void
SBMLHandler::characters (const XMLCh* const  chars,
                             const unsigned int  length)
{
  if (inNotes || inAnnotation)
  {
    fFormatter->characters(chars, length);
  }
}


void
SBMLHandler::ignorableWhitespace (const XMLCh* const  chars,
                                      const unsigned int  length)
{
  if (inNotes || inAnnotation)
  {
    fFormatter->ignorableWhitespace(chars, length);
  }
}


void
SBMLHandler::setDocumentLocator (const Locator *const locator)
{
  fLocator = locator;
}


/* ----------------------------------------------------------------------
 *                         SAX2 Error Handlers
 * ----------------------------------------------------------------------
 */


void
SBMLHandler::warning (const SAXParseException& e)
{
  List_add( fDocument->warning, createParseMessage(e) );
}


void
SBMLHandler::error (const SAXParseException& e)
{
  List_add( fDocument->error, createParseMessage(e) );
}


void
SBMLHandler::fatalError (const SAXParseException& e)
{
  List_add( fDocument->fatal, createParseMessage(e) );
}


/* ----------------------------------------------------------------------
 *                          Custom Error Handlers
 * ----------------------------------------------------------------------
 */


void
SBMLHandler::warning (const char* message)
{
  List_add( fDocument->warning, createParseMessage(message) );
}


void
SBMLHandler::error (const char* message)
{
  List_add( fDocument->error, createParseMessage(message) );
}


void
SBMLHandler::fatalError (const char* message)
{
  List_add( fDocument->fatal, createParseMessage(message) );
}


ParseMessage_t*
SBMLHandler::createParseMessage (const char* message)
{
  ParseMessage_t* pm;


  pm = (ParseMessage_t *) safe_calloc(1, sizeof(ParseMessage_t));

  pm->message = safe_strdup(message);
  pm->line    = (unsigned int) fLocator->getLineNumber();
  pm->column  = (unsigned int) fLocator->getColumnNumber();

  return pm;
}


ParseMessage_t*
SBMLHandler::createParseMessage (const SAXParseException& e)
{
  char*           msg;
  ParseMessage_t* pm;


  msg = XMLString::transcode( e.getMessage() );
  pm  = (ParseMessage_t *) safe_calloc(1, sizeof(ParseMessage_t));

  pm->message = safe_strdup(msg);
  pm->line    = (unsigned int) e.getLineNumber();
  pm->column  = (unsigned int) e.getColumnNumber();

  delete [] msg;

  return pm;
}


/* ----------------------------------------------------------------------
 *                          SBML Tag Handlers
 * ----------------------------------------------------------------------
 */


SBase_t*
SBMLHandler::handleSBML (const Attributes& a)
{
  XMLUtil::scanAttr( a, ATTR_LEVEL  , &(fDocument->level)   );
  XMLUtil::scanAttr( a, ATTR_VERSION, &(fDocument->version) );

  return (SBase_t*) fDocument;
}


SBase_t*
SBMLHandler::handleModel (const Attributes& a)
{
  fModel           = Model_create();
  fDocument->model = fModel;

  XMLUtil::scanAttrCStr(a, ATTR_NAME, &(fModel->name));

  return (SBase_t*) fModel;
}


SBase_t*
SBMLHandler::handleUnitDefinition (const Attributes& a)
{
  UnitDefinition_t* ud = Model_createUnitDefinition(fModel);


  XMLUtil::scanAttrCStr(a, ATTR_NAME, &(ud->name));

  return (SBase_t*) ud;
}


SBase_t*
SBMLHandler::handleUnit (const Attributes& a)
{
  Unit_t* u    = Model_createUnit(fModel);
  char*   kind = XMLString::transcode( a.getValue(ATTR_KIND) ); 
  int     value;


  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_EXPONENT, &value) == true)
  {
    Unit_setExponent(u, value);
  }

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_SCALE, &value) == true)
  {
    Unit_setScale(u, value);
  }

  u->kind = UnitKind_forName(kind);

  delete [] kind;

  return (SBase_t*) u;
}


SBase_t*
SBMLHandler::handleCompartment (const Attributes& a)
{
  Compartment_t* c = Model_createCompartment(fModel);
  double value;


  XMLUtil::scanAttrCStr(a, ATTR_NAME, &(c->name));

  //
  // volume  { use="optional" default="1.0" }  (L1v1, L1v2)
  //
  if (XMLUtil::scanAttr(a, ATTR_VOLUME, &value) == true)
  {
    Compartment_setVolume(c, value);
  }

  XMLUtil::scanAttrCStr( a, ATTR_UNITS  , &(c->units)   );
  XMLUtil::scanAttrCStr( a, ATTR_OUTSIDE, &(c->outside) );

  return (SBase_t*) c;
}


SBase_t*
SBMLHandler::handleSpecies (const Attributes& a)
{
  Species_t* s = Model_createSpecies(fModel);
  bool value;


  XMLUtil::scanAttrCStr(a, ATTR_NAME       , &(s->name)        );
  XMLUtil::scanAttrCStr(a, ATTR_COMPARTMENT, &(s->compartment) );

  s->isSet.initialAmount =
    XMLUtil::scanAttr(a, ATTR_INITIAL_AMOUNT, &(s->initialAmount));

  XMLUtil::scanAttrCStr(a, ATTR_UNITS, &(s->units));

  //
  // boundaryCondition
  // { use="optional" default="false" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_BOUNDARY_CONDITION, &value) == true)
  {
    Species_setBoundaryCondition(s, value);
  }

  s->isSet.charge =
    XMLUtil::scanAttr(a, ATTR_CHARGE, &(s->charge));

  return (SBase_t*) s;
}


SBase_t*
SBMLHandler::handleParameter (const Attributes& a)
{
  Parameter_t* p  = NULL;
  HashCode_t tag  = (HashCode_t) Stack_peekAt(fTagStack, 1);


  if (tag == HASH_KINETIC_LAW)
  {
    p = Model_createKineticLawParameter(fModel);
  }
  else
  {
    p = Model_createParameter(fModel);
  }

  XMLUtil::scanAttrCStr( a, ATTR_NAME , &(p->name)  );
  XMLUtil::scanAttrCStr( a, ATTR_UNITS, &(p->units) );

  //
  // value  { use="required" }  (L1v1)
  // value  { use="optional" }  (L1v2, L2v1)
  //
  p->isSet.value =
    XMLUtil::scanAttr(a, ATTR_VALUE, &(p->value));

  return (SBase_t*) p;
}


SBase_t*
SBMLHandler::handleReaction (const Attributes& a)
{
  Reaction_t* r = Model_createReaction(fModel);
  bool value;


  XMLUtil::scanAttrCStr(a, ATTR_NAME, &(r->name));

  //
  // reversible  { use="optional" default="true" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_REVERSIBLE, &value) == true)
  {
    r->reversible = value;
  }

  //
  // fast  { use="optional" default="false" }  (L1v1, L1v2, L2v1)
  //
  if (XMLUtil::scanAttr(a, ATTR_FAST, &value) == true)
  {
    r->fast = value;
  }

  return (SBase_t*) r;
}


SBase_t*
SBMLHandler::handleSpeciesReference (const Attributes& a)
{
  SpeciesReference_t* sr    = NULL;
  HashCode_t          tag   = (HashCode_t) Stack_peek(fTagStack);
  int                 index = 0;


  if (tag == HASH_LIST_OF_REACTANTS)
  {
    sr = Model_createReactant(fModel);
  }
  else if (tag == HASH_LIST_OF_PRODUCTS)
  {
    sr = Model_createProduct(fModel);
  }

  //
  // Two spellings of "species" are allowed.  Look first for "species" and
  // if not found, look for "specie".
  //
  index = a.getIndex(ATTR_SPECIES);
  if (index >= 0)
  {
    XMLUtil::scanAttrCStr(a, index, &(sr->species));
  }
  else
  {
    XMLUtil::scanAttrCStr(a, ATTR_SPECIE, &(sr->species));
  }

  XMLUtil::scanAttr( a, ATTR_STOICHIOMETRY, &(sr->stoichiometry) );
  XMLUtil::scanAttr( a, ATTR_DENOMINATOR  , &(sr->denominator)   );

  return (SBase_t*) sr;
}


SBase_t*
SBMLHandler::handleKineticLaw (const Attributes& a)
{
  KineticLaw_t* kl = Model_createKineticLaw(fModel);

  // If tag != HASH_REACTION -> Creation outside a reaction
  // If kl == NULL           -> KineticLaw already exists
  XMLUtil::scanAttrCStr( a, ATTR_FORMULA        , &(kl->formula)        );
  XMLUtil::scanAttrCStr( a, ATTR_TIME_UNITS     , &(kl->timeUnits)      );
  XMLUtil::scanAttrCStr( a, ATTR_SUBSTANCE_UNITS, &(kl->substanceUnits) );

  return (SBase_t*) kl;
}


SBase_t*
SBMLHandler::handleAlgebraicRule (const Attributes& a)
{
  AlgebraicRule_t* ar = Model_createAlgebraicRule(fModel);


  XMLUtil::scanAttrCStr(a, ATTR_FORMULA, &(ar->formula));

  return (SBase_t*) ar;
}


SBase_t*
SBMLHandler::handleCompartmentVolumeRule (const Attributes& a)
{
  CompartmentVolumeRule_t* cvr  = Model_createCompartmentVolumeRule(fModel);
  char*                    type = XMLString::transcode( a.getValue(ATTR_TYPE) );


  XMLUtil::scanAttrCStr( a, ATTR_FORMULA    , &(cvr->formula)     );
  XMLUtil::scanAttrCStr( a, ATTR_COMPARTMENT, &(cvr->compartment) );

  //
  // type { use="optional" default="scalar" )  (L1v1, L1v2)
  //
  if (type != NULL)
  {
    cvr->type = RuleType_forName(type);
    delete [] type;
  }

  return (SBase_t*) cvr;
}


SBase_t*
SBMLHandler::handleParameterRule (const Attributes& a)
{
  ParameterRule_t* pr   = Model_createParameterRule(fModel);
  char*            type = XMLString::transcode( a.getValue(ATTR_TYPE) );


  XMLUtil::scanAttrCStr( a, ATTR_FORMULA, &(pr->formula) );
  XMLUtil::scanAttrCStr( a, ATTR_NAME   , &(pr->name)    );
  XMLUtil::scanAttrCStr( a, ATTR_UNITS  , &(pr->units)   );

  //
  // type { use="optional" value="scalar" }  (L1v1, L1v2)
  //
  if (type != NULL)
  {
    pr->type = RuleType_forName(type);
    delete [] type;
  }

  return (SBase_t*) pr;
}


SBase_t*
SBMLHandler::handleSpeciesConcentrationRule (const Attributes& a)
{
  SpeciesConcentrationRule_t* scr =
    Model_createSpeciesConcentrationRule(fModel);

  int index = 0;


  XMLUtil::scanAttrCStr(a, ATTR_FORMULA, &(scr->formula));

  //
  // type { use="optional" value="scalar" }  (L1v1, L1v2)
  //
  index = a.getIndex(ATTR_TYPE);
  if (index >= 0)
  {
    char* type = XMLString::transcode( a.getValue(index) );
    scr->type  = RuleType_forName(type);
    delete [] type;
  }

  //
  // Two spellings of "species" are allowed.  Look first for "species" and
  // if not found, look for "specie".
  //
  index = a.getIndex(ATTR_SPECIES);
  if (index >= 0)
  {
    XMLUtil::scanAttrCStr(a, index, &(scr->species));
  }
  else
  {
    XMLUtil::scanAttrCStr(a, ATTR_SPECIE, &(scr->species));
  }

  return (SBase_t*) scr;
}
