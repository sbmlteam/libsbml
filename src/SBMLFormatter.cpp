/**
 * Filename    : SBMLFormatter.cpp
 * Description : Formats SBML ...
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-03-07
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


#include "sbml/common.h"
#include "sbml/SBMLFormatter.hpp"


//
// <?xml version="1.0" encoding="
//
static const XMLCh XML_DECL_1[] =
{
  chOpenAngle, chQuestion,

  chLatin_x, chLatin_m, chLatin_l, chSpace,

  chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i, chLatin_o,
  chLatin_n,

  chEqual,

  chDoubleQuote, chDigit_1, chPeriod, chDigit_0, chDoubleQuote, chSpace,

  chLatin_e, chLatin_n, chLatin_c, chLatin_o, chLatin_d, chLatin_i,
  chLatin_n, chLatin_g,

  chEqual, chDoubleQuote, chNull
};


//
// "?>
//
static const XMLCh  XML_DECL_2[] =
{
  chDoubleQuote, chQuestion, chCloseAngle, chLF, chNull
};


//
// Ctor
//
// Creates a new SBMLFormatter
//
// The underlying character encoding is configurable and defaults to
// "LATIN1", which is probably safe for most applications.
//
SBMLFormatter::SBMLFormatter (const char* outEncoding, XMLFormatTarget* target)
{
  //
  // Initialize() is static and may be called more than once safely.
  //
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (const XMLException& e)
  {
  }

  fLevel   = 1;
  fVersion = 2;

  fIndentLevel = 0;

  fTarget      = target;
  fFormatter   = new XMLFormatter( outEncoding,
                                   fTarget,
                                   XMLFormatter::NoEscapes,
                                   XMLFormatter::UnRep_CharRef );

  *fFormatter
    << XML_DECL_1
    << fFormatter->getEncodingName()
    << XML_DECL_2;
}


//
// Dtor
//
SBMLFormatter::~SBMLFormatter ()
{
  delete fFormatter;
}




/* ----------------------------------------------------------------------
 *                          Insertion operator
 * ----------------------------------------------------------------------
 */


/**
 * Sets the SBML Level number (used to format subsequent insertions).
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SBMLLevel_t level)
{
  fLevel = (unsigned int) level;
  return *this;
}


/**
 * Sets the SBML Version number (used to format subsequent insertions).
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SBMLVersion_t version)
{
  fVersion = (unsigned int) version;
  return *this;
}


//
// SBMLDocument
//
SBMLFormatter&
SBMLFormatter::operator<< (const SBMLDocument_t* d)
{
  fLevel   = d->level;
  fVersion = d->version;

  openStartElement(ELEM_SBML);

  attribute( ATTR_XMLNS  , XMLNS_SBML_L1 );
  attribute( ATTR_LEVEL  , d->level      );
  attribute( ATTR_VERSION, d->version    );

  if (d->model == NULL)
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    *this << d->model;

    downIndent();

    endElement(ELEM_SBML);
  }

  return *this;
}


//
// Model
//
SBMLFormatter&
SBMLFormatter::operator<< (const Model_t* m)
{
  if (m == NULL) return *this;


  openStartElement(ELEM_MODEL);

  //
  // name  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (m->name != NULL)
  {
    attribute(ATTR_NAME, m->name);
  }

  if ( isEmpty(m) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    notesAndAnnotation( (SBase_t*) m );

    listOfUnitDefinitions( m->unitDefinition );
    listOfCompartments   ( m->compartment    );
    listOfSpecies        ( m->species        );
    listOfParameters     ( m->parameter      );
    listOfRules          ( m->rule           );
    listOfReactions      ( m->reaction       );
    
    downIndent();

    endElement(ELEM_MODEL);
  }

  return *this;
}


//
// UnitDefinition
//
SBMLFormatter&
SBMLFormatter::operator<< (const UnitDefinition_t* ud)
{
  if (ud == NULL) return *this;


  openStartElement(ELEM_UNIT_DEFINITION);

  attribute(ATTR_NAME, ud->name);


  if ( isEmpty(ud) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    notesAndAnnotation( (SBase_t*) ud );
    listOfUnits(ud->unit);

    downIndent();

    endElement(ELEM_UNIT_DEFINITION);
  }

  return *this;
}


//
// Unit
//
SBMLFormatter&
SBMLFormatter::operator<< (const Unit_t* u)
{
  if (u == NULL) return *this;


  openStartElement(ELEM_UNIT);

  attribute(ATTR_KIND, UnitKind_toString(u->kind));

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1)
  //
  if (u->exponent != 1)
  {
    attribute(ATTR_EXPONENT, u->exponent);
  }

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1)
  //
  if (u->scale != 0)
  {
    attribute(ATTR_SCALE, u->scale);
  }

  if ( isEmpty(u) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_UNIT, (SBase_t*) u);
  }

  return *this;
}


//
// Compartment
//
SBMLFormatter&
SBMLFormatter::operator<< (const Compartment_t* c)
{
  if (c == NULL) return *this;


  openStartElement(ELEM_COMPARTMENT);

  attribute(ATTR_NAME, c->name);

  //
  //
  // volume  { use="optional" default="1" }  (L1v1, L1v2)
  //
  // Although compartment has a default volume of 1 in SBML L1, IEEE 754
  // doubles (or floats) cannot be reliably compared for equality.  To be
  // safe, output the compartment volume even if equal to the default.
  //
  // However, do not output if unset.
  //
  //
  if (Compartment_isSetVolume(c))
  {
    attribute(ATTR_VOLUME, c->volume);
  }

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (c->units != NULL)
  {
    attribute(ATTR_UNITS, c->units);
  }

  //
  // outside  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (c->outside != NULL)
  {
    attribute(ATTR_OUTSIDE, c->outside);
  }

  if ( isEmpty(c) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_COMPARTMENT, (SBase_t*) c);
  }

  return *this;
}


//
// Species
//
SBMLFormatter&
SBMLFormatter::operator<< (const Species_t* s)
{
  if (s == NULL) return *this;


  const XMLCh* elem = ELEM_SPECIES;


  if ((fLevel == 1) && (fVersion == 1))
  {
    elem = ELEM_SPECIE;
  }

  openStartElement(elem);

  attribute(ATTR_NAME, s->name);

  //
  // 
  // compartment  { use="required" }  (L1v1, L2v1, L2v1)
  //
  attribute(ATTR_COMPARTMENT, s->compartment);

  //
  // initialAmount  { use="required" }  (L1v1, L1v2)
  //
  attribute(ATTR_INITIAL_AMOUNT, s->initialAmount);

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (s->units != NULL)
  {
    attribute(ATTR_UNITS, s->units);
  }

  //
  // boundaryCondition
  // { use="optional" default="false" (0) }  (L1v1, L1v2, L2v1)
  // 
  //
  if (s->boundaryCondition != 0)
  {
    attribute(ATTR_BOUNDARY_CONDITION, (bool) s->boundaryCondition);
  }

  //
  // charge  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (s->isSet.charge)
  {
    attribute(ATTR_CHARGE, s->charge);
  }

  if ( isEmpty(s) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(elem, (SBase_t*) s);
  }

  return *this;
}


//
// Parameter
//
SBMLFormatter&
SBMLFormatter::operator<< (const Parameter_t* p)
{
  if (p == NULL) return *this;


  openStartElement(ELEM_PARAMETER);

  attribute(ATTR_NAME, p->name);

  //
  // value  { use="required" }  (L1v2)
  // value  { use="optional" }  (L1v2, L2v1)
  //
  if ((fLevel == 1 && fVersion == 1) || p->isSet.value)
  {
    attribute(ATTR_VALUE, p->value);
  }

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (p->units != NULL)
  {
    attribute(ATTR_UNITS, p->units);
  }

  if ( isEmpty(p) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_PARAMETER, (SBase_t*) p);
  }

  return *this;
}


//
// Rule
//
SBMLFormatter&
SBMLFormatter::operator<< (const Rule_t* r)
{
  if (r == NULL) return *this;


  switch (r->typecode)
  {
    case SBML_ALGEBRAIC_RULE:
      *this << (AlgebraicRule_t*) r;
      break;

    case SBML_SPECIES_CONCENTRATION_RULE:
      *this << (SpeciesConcentrationRule_t*) r;
      break;
      
    case SBML_COMPARTMENT_VOLUME_RULE:
      *this << (CompartmentVolumeRule_t*) r;
      break;

    case SBML_PARAMETER_RULE:
      *this << (ParameterRule_t*) r;
      break;

    default:
      break;
  }

  return *this;
}


//
// AlgebraicRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const AlgebraicRule_t* ar)
{
  if (ar == NULL) return *this;


  openStartElement(ELEM_ALGEBRAIC_RULE);

  attribute(ATTR_FORMULA, ar->formula);

  if ( isEmpty(ar) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_ALGEBRAIC_RULE, (SBase_t*) ar);
  }

  return *this;
}


//
// SpeciesConcentrationRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const SpeciesConcentrationRule_t* scr)
{
  if (scr == NULL) return *this;


  const XMLCh* elem = ELEM_SPECIES_CONCENTRATION_RULE;
  const XMLCh* attr = ATTR_SPECIES;


  if ((fLevel == 1) && (fVersion == 1))
  {
    elem = ELEM_SPECIE_CONCENTRATION_RULE;
    attr = ATTR_SPECIE;
  }

  openStartElement(elem);

  attribute(ATTR_FORMULA, scr->formula);
  ruleType(scr->type);
  attribute(attr, scr->species);

  if ( isEmpty(scr) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(elem, (SBase_t*) scr);
  }

  return *this;
}


//
// CompartmentVolumeRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const CompartmentVolumeRule_t* cvr)
{
  if (cvr == NULL) return *this;

  openStartElement(ELEM_COMPARTMENT_VOLUME_RULE);

  attribute(ATTR_FORMULA, cvr->formula);
  ruleType(cvr->type);
  attribute(ATTR_COMPARTMENT, cvr->compartment);

  if ( isEmpty(cvr) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_COMPARTMENT_VOLUME_RULE, (SBase_t*) cvr);
  }

  return *this;
}


//
// ParameterRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const ParameterRule_t* pr)
{
  if (pr == NULL) return *this;


  openStartElement(ELEM_PARAMETER_RULE);

  attribute(ATTR_FORMULA, pr->formula);
  ruleType(pr->type);

  attribute(ATTR_NAME, pr->name);

  //
  // units  { use="optional" }  (L1v1, L1v2);
  //
  if (pr->units != NULL)
  {
    attribute(ATTR_UNITS, pr->units);
  }

  if ( isEmpty(pr) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_PARAMETER_RULE, (SBase_t*) pr);
  }

  return *this;
}


//
// Reaction
//
SBMLFormatter&
SBMLFormatter::operator<< (const Reaction_t* r)
{
  if (r == NULL) return *this;


  openStartElement(ELEM_REACTION);

  attribute(ATTR_NAME, r->name);

  //
  // reversible  { use="optional"  default="true" (1) }  (L1v1, L1v2, L2v1)
  //
  if (r->reversible != 1)
  {
    attribute(ATTR_REVERSIBLE, (bool) r->reversible);
  }

  //
  // fast  { use="optional" default="false" (0) }  (L1v1, L1v2, L2v1)
  //
  if (r->fast != 0)
  {
    attribute(ATTR_FAST, (bool) r->fast);
  }

  if ( isEmpty(r) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    notesAndAnnotation( (SBase_t*) r );
    listOfReactants( r->reactant );
    listOfProducts ( r->product  );
    *this << r->kineticLaw;

    downIndent();

    endElement(ELEM_REACTION);
  }

  return *this;
}


//
// SpeciesReference
//
SBMLFormatter&
SBMLFormatter::operator<< (const SpeciesReference_t* sr)
{
  if (sr == NULL) return *this;


  const XMLCh* elem = ELEM_SPECIES_REFERENCE;
  const XMLCh* attr = ATTR_SPECIES;


  if ((fLevel == 1) && (fVersion == 1))
  {
    elem = ELEM_SPECIE_REFERENCE;
    attr = ATTR_SPECIE;
  }

  openStartElement(elem);

  attribute(attr, sr->species);

  //
  // stoichiometry  { use="optional" default="1" }  (L1v1, L1v2, L2v1)
  //
  if (sr->stoichiometry != 1)
  {
    attribute(ATTR_STOICHIOMETRY, sr->stoichiometry);
  }

  //
  // denominator  { use="optional" default="1" }  (L1v1, L1v2, L2v1)
  //
  if (sr->denominator != 1)
  {
    attribute(ATTR_DENOMINATOR, sr->denominator);
  }

  if ( isEmpty(sr) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(elem, (SBase_t*) sr);
  }

  return *this;
}


//
// KineticLaw
//
SBMLFormatter&
SBMLFormatter::operator<< (const KineticLaw_t* kl)
{
  if (kl == NULL) return *this;


  openStartElement(ELEM_KINETIC_LAW);

  attribute(ATTR_FORMULA, kl->formula);

  //
  // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (kl->timeUnits != NULL)
  {
    attribute(ATTR_TIME_UNITS, kl->timeUnits);
  }

  //
  // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if (kl->substanceUnits != NULL)
  {
    attribute(ATTR_SUBSTANCE_UNITS, kl->substanceUnits);
  }

  if ( isEmpty(kl) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    notesAndAnnotation( (SBase_t*) kl );
    listOfParameters(kl->parameter);

    downIndent();

    endElement(ELEM_KINETIC_LAW);
  } 

  return *this;
}




/* ----------------------------------------------------------------------
 *                 Insertion Operator Supporting Functions
 * ----------------------------------------------------------------------
 */


//
// listOfXXXs
//
// The listOfXXX() functions are similar, with changes in element (tag)
// names and C types only.
//
// This #define macro provides a template which is used to create each
// specific function.  Think, C++ templates without all the added baggage.
//
#define makeFn(name, element, type)          \
void                                         \
SBMLFormatter::name (List_t* list)           \
{                                            \
  unsigned int size = List_size(list);       \
                                             \
                                             \
  if (size > 0)                              \
  {                                          \
    startElement( element );                 \
                                             \
    upIndent();                              \
                                             \
    for (unsigned int n = 0; n < size; n++)  \
    {                                        \
      *this << ( type * ) List_get(list, n); \
    }                                        \
                                             \
    downIndent();                            \
                                             \
    endElement( element );                   \
  }                                          \
}

makeFn( listOfUnitDefinitions, ELEM_LIST_OF_UNIT_DEFINITIONS, UnitDefinition_t )
makeFn( listOfUnits          , ELEM_LIST_OF_UNITS           , Unit_t           )
makeFn( listOfCompartments   , ELEM_LIST_OF_COMPARTMENTS    , Compartment_t    )
makeFn( listOfSpecies        , ELEM_LIST_OF_SPECIES         , Species_t        )
makeFn( listOfParameters     , ELEM_LIST_OF_PARAMETERS      , Parameter_t      )
makeFn( listOfRules          , ELEM_LIST_OF_RULES           , Rule_t           )
makeFn( listOfReactions      , ELEM_LIST_OF_REACTIONS       , Reaction_t       )
makeFn( listOfReactants      , ELEM_LIST_OF_REACTANTS     , SpeciesReference_t )
makeFn( listOfProducts       , ELEM_LIST_OF_PRODUCTS      , SpeciesReference_t )

#undef makeFn


//
// Rule Type
//
void
SBMLFormatter::ruleType (const RuleType_t type)
{
  //
  // type  { use="optional" default="scalar" }  (L1v1, L1v2, L2v1)
  //
  if (type != RULE_TYPE_SCALAR)
  {
    attribute(ATTR_TYPE, RuleType_toString(type));
  }
}


//
// Notes
//
void
SBMLFormatter::notes (const char* s)
{
  if (isEmpty(s)) return;


  startElement(ELEM_NOTES);

  upIndent();
  indent();

  XMLCh* x = XMLString::transcode(s);
  *fFormatter << x << chLF;
  delete [] x;

  downIndent();

  endElement(ELEM_NOTES);
}


//
// Annotation
//
void
SBMLFormatter::annotation (const char* s)
{
  if (isEmpty(s)) return;


  indent();

  XMLCh* x = XMLString::transcode(s);
  *fFormatter << x << chLF;
  delete [] x;
}


//
// Notes and Annotation
//
void
SBMLFormatter::notesAndAnnotation(const SBase_t* sb)
{
  notes(sb->notes);
  annotation(sb->annotation);
}




/* ----------------------------------------------------------------------
 *                               isEmpty()
 * ----------------------------------------------------------------------
 */


/**
 * Returns true if the string pointed to by s is NULL or zero-length.
 */
bool
SBMLFormatter::isEmpty (const char* s)
{
  return !(s && *s);
}


//
// In this context "empty" means either no notes, annotations and other
// SBML (XML) subelements.
//

bool
SBMLFormatter::isEmpty (const SBase_t* sb)
{
  return isEmpty(sb->notes) && isEmpty(sb->annotation);
}

bool
SBMLFormatter::isEmpty (const Model_t* m)
{
  return isEmpty((SBase_t*) m)                 &&
         (Model_getNumUnitDefinitions(m) == 0) &&
         (Model_getNumCompartments   (m) == 0) &&
         (Model_getNumSpecies        (m) == 0) &&
         (Model_getNumParameters     (m) == 0) &&
         (Model_getNumRules          (m) == 0) &&
         (Model_getNumReactions      (m) == 0);
}

bool
SBMLFormatter::isEmpty (const UnitDefinition_t* ud)
{
  return isEmpty((SBase_t*) ud) && (UnitDefinition_getNumUnits(ud) == 0);
}

bool
SBMLFormatter::isEmpty (const Reaction_t* r)
{
  return isEmpty((SBase_t*) r)              &&
         (Reaction_getNumReactants(r) == 0) &&
         (Reaction_getNumProducts (r) == 0) &&
         r->kineticLaw == NULL;
}

bool
SBMLFormatter::isEmpty (const KineticLaw_t* kl)
{
  return isEmpty((SBase_t*) kl) && (KineticLaw_getNumParameters(kl) == 0);
}


//
// The rest of the isEmpty() functions have the same basic form.  In fact,
// they exist simply to avoid an explicit type cast.
//
#define makeFn(type)                   \
bool                                   \
SBMLFormatter::isEmpty (const type *t) \
{                                      \
  return isEmpty((SBase_t*) t);        \
}

makeFn( Unit_t                     )
makeFn( Compartment_t              )
makeFn( Species_t                  )
makeFn( Parameter_t                )
makeFn( AlgebraicRule_t            )
makeFn( SpeciesConcentrationRule_t )
makeFn( CompartmentVolumeRule_t    )
makeFn( ParameterRule_t            )
makeFn( SpeciesReference_t         )

#undef makeFn



/* ----------------------------------------------------------------------
 *                      XML Elements and Attributes
 * ----------------------------------------------------------------------
 */


/**
 * Sends '<name>\n' to the underlying XMLFormatter.
 */
void
SBMLFormatter::startElement (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes
              << chOpenAngle << name << chCloseAngle << chLF;
}


/**
 * Sends '</name>\n' to the underlying XMLFormatter.
 */
void
SBMLFormatter::endElement (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes
              << chOpenAngle  << chForwardSlash << name
              << chCloseAngle << chLF;
}


/**
 * Encapsulates a common operation for ending SBML (XML) elements that
 * contain non-empty <notes>, <annotation>s or both, but are not allowed to
 * contain other subelements like <listOfXXXs> or <kineticLaw>s.
 */
void
SBMLFormatter::endElement(const XMLCh* name, const SBase_t* sb)
{
    closeStartElement();

    upIndent();
    notesAndAnnotation(sb);
    downIndent();

    endElement(name);
}


/**
 * Sends '<name' to the underlying XMLFormatter.  Use when name has one or
 * more attributes.
 *
 * See also closeStartElement() or slashCloseStartElement().
 */
void
SBMLFormatter::openStartElement (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes << chOpenAngle << name;
}


/**
 * Sends '>\n' to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
SBMLFormatter::closeStartElement ()
{
  *fFormatter << XMLFormatter::NoEscapes << chCloseAngle << chLF;
}


/**
 * Sends "/>\n" to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
SBMLFormatter::slashCloseStartElement ()
{
  *fFormatter << XMLFormatter::NoEscapes
              << chForwardSlash << chCloseAngle << chLF;
}


/**
 * Sends ' name="true"' or ' name="false"' to the underlying XMLFormatter
 */
void
SBMLFormatter::attribute (const XMLCh* name, bool value)
{
  (value == true) ? attribute(name, VAL_TRUE) : attribute(name, VAL_FALSE);
}


/**
 * Sends ' name="%d" to the underlying XMLFormatter (where %d is an integer).
 */
void
SBMLFormatter::attribute (const XMLCh* name, int value)
{
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%d", value);
  attribute(name, fNumberBuffer);
}


/**
 * Sends ' name="%u" to the underlying XMLFormatter (where %u is an unsigned
 * integer).
 */
void
SBMLFormatter::attribute (const XMLCh* name, unsigned int value)
{
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%u", value);
  attribute(name, fNumberBuffer);
}


/**
 * Sends ' name="%g" to the underlying XMLFormatter (where %g is a double).
 */
void
SBMLFormatter::attribute (const XMLCh* name, double value)
{
  static const XMLCh NaN[] = { chLatin_N, chLatin_a, chLatin_N, chNull };


  if (value != value)
  {
    attribute(name, NaN);
  }
  else
  {
    snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%g", value);
    attribute(name, fNumberBuffer);
  }
}


/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a C string).
 */
void
SBMLFormatter::attribute (const XMLCh* name, const char* value)
{
  XMLCh* s;


  if (value == NULL)
  {
    attribute(name, (const XMLCh*) NULL);
  }
  else
  {
    s = XMLString::transcode(value);
    attribute(name, s);

    delete [] s;
  }
}


/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a Unicode
 * string).
 */
void
SBMLFormatter::attribute (const XMLCh* name, const XMLCh* value)
{
  *fFormatter
    << XMLFormatter::NoEscapes
    << chSpace
    << name
    << chEqual
    << chDoubleQuote
    << XMLFormatter::AttrEscapes;

  if (value != NULL)
  {
    *fFormatter << value;
  }

  *fFormatter << XMLFormatter::NoEscapes << chDoubleQuote;
}


/**
 * Sends whitespace to the underlying XMLFormatter based on the current
 * indentation level.
 */
void
SBMLFormatter::indent ()
{
  for (unsigned int n = 0; n < fIndentLevel; n++)
  {
    *fFormatter << chSpace << chSpace;
  }
}
