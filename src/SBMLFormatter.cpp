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

static const XMLCh fIndent[] = { chSpace, chSpace, chNull };


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
  // Initialize() is static, but may be called more than once safely.
  //
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (const XMLException& e)
  {
  }

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
  delete fTarget;
  delete fFormatter;
}


//
// SBMLDocument
//
SBMLFormatter&
SBMLFormatter::operator<< (const SBMLDocument_t *d)
{
  startElement(ELEM_SBML);

  attribute( ATTR_LEVEL  , d->level   );
  attribute( ATTR_VERSION, d->version );

  closeStartElement();

  if (d->model != NULL)
  {
    *this << d->model;
  }

  endElement(ELEM_SBML);

  return *this;
}


//
// Model
//
SBMLFormatter&
SBMLFormatter::operator<< (const Model_t *m)
{
  startElement(ELEM_MODEL);

  attribute(ATTR_NAME, m->name);

  closeStartElement();

  endElement(ELEM_MODEL);

  return *this;
}


//
// Unit
//
SBMLFormatter&
SBMLFormatter::operator<< (const Unit_t *u)
{
  const char* kind = UnitKind_toString(u->kind);


  startElement(ELEM_UNIT);

  attribute( ATTR_KIND,     kind        );
  attribute( ATTR_EXPONENT, u->exponent );
  attribute( ATTR_SCALE,    u->scale    );

  if ( isEmpty(u->notes) && isEmpty(u->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_UNIT);
  }

  return *this;
}


//
// UnitDefinition
//
SBMLFormatter&
SBMLFormatter::operator<< (const UnitDefinition_t *ud)
{
  unsigned int n, size;


  startElement(ELEM_UNIT_DEFINITION);

  attribute(ATTR_NAME, ud->name);
  
  closeStartElement();

  size = UnitDefinition_getNumUnits(ud);

  if (size > 0)
  {
    upIndent();
    startElement(ELEM_LIST_OF_UNITS);
    closeStartElement();
    upIndent();

    for (n = 0; n < size; n++)
    {
      *this << UnitDefinition_getUnit(ud, n);
    }

    downIndent();
    endElement(ELEM_LIST_OF_UNITS);
    downIndent();
  }

  endElement(ELEM_UNIT_DEFINITION);

  return *this;
}


//
// Compartment
//
SBMLFormatter&
SBMLFormatter::operator<< (const Compartment_t *c)
{
  startElement(ELEM_COMPARTMENT);

  attribute( ATTR_NAME   , c->name    );
  attribute( ATTR_VOLUME , c->volume  );
  attribute( ATTR_UNITS  , c->units   );
  attribute( ATTR_OUTSIDE, c->outside );

  if ( isEmpty(c->notes) && isEmpty(c->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_COMPARTMENT);
  }

  return *this;
}


//
// Species
//
SBMLFormatter&
SBMLFormatter::operator<< (const Species_t *s)
{
  startElement(ELEM_SPECIES);

  attribute( ATTR_NAME              , s->name                     );
  attribute( ATTR_COMPARTMENT       , s->compartment              );
  attribute( ATTR_INITIAL_AMOUNT    , s->initialAmount            );
  attribute( ATTR_UNITS             , s->units                    );
  attribute( ATTR_BOUNDARY_CONDITION, (bool) s->boundaryCondition );
  attribute( ATTR_CHARGE            , s->charge                   );

  if ( isEmpty(s->notes) && isEmpty(s->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_SPECIES);
  }

  return *this;
}


//
// Parameter
//
SBMLFormatter&
SBMLFormatter::operator<< (const Parameter_t *p)
{
  startElement(ELEM_PARAMETER);

  attribute( ATTR_NAME , p->name  );
  attribute( ATTR_VALUE, p->value );
  attribute( ATTR_UNITS, p->units );

  if ( isEmpty(p->notes) && isEmpty(p->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_PARAMETER);
  }

  return *this;
}


//
// AlgebraicRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const AlgebraicRule_t *ar)
{
  startElement(ELEM_ALGEBRAIC_RULE);

  attribute(ATTR_FORMULA, ar->formula);

  if ( isEmpty(ar->notes) && isEmpty(ar->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_ALGEBRAIC_RULE);
  }

  return *this;
}


//
// SpeciesConcentrationRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const SpeciesConcentrationRule_t *scr)
{
  startElement(ELEM_SPECIES_CONCENTRATION_RULE);

  attribute( ATTR_FORMULA, scr->formula );
  attribute( ATTR_TYPE   , scr->type    );
  attribute( ATTR_SPECIES, scr->species );

  if ( isEmpty(scr->notes) && isEmpty(scr->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_SPECIES_CONCENTRATION_RULE);
  }

  return *this;
}


//
// CompartmentVolumeRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const CompartmentVolumeRule_t *cvr)
{
  startElement(ELEM_COMPARTMENT_VOLUME_RULE);

  attribute( ATTR_FORMULA    , cvr->formula     );
  attribute( ATTR_TYPE       , cvr->type        );
  attribute( ATTR_COMPARTMENT, cvr->compartment );

  if ( isEmpty(cvr->notes) && isEmpty(cvr->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_COMPARTMENT_VOLUME_RULE);
  }

  return *this;
}


//
// ParameterRule
//
SBMLFormatter&
SBMLFormatter::operator<< (const ParameterRule_t *pr)
{
  startElement(ELEM_PARAMETER_RULE);

  attribute( ATTR_FORMULA, pr->formula );
  attribute( ATTR_NAME   , pr->name    );
  attribute( ATTR_UNITS  , pr->units   );

  if ( isEmpty(pr->notes) && isEmpty(pr->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_PARAMETER_RULE);
  }

  return *this;
}


//
// Reaction
//
SBMLFormatter&
SBMLFormatter::operator<< (const Reaction_t *r)
{
  startElement(ELEM_REACTION);

  attribute( ATTR_NAME      , r->name              );
  attribute( ATTR_REVERSIBLE, (bool) r->reversible );
  attribute( ATTR_FAST      , (bool) r->fast       );

  if ( isEmpty(r->notes) && isEmpty(r->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_REACTION);
  }

  return *this;
}


//
// SpeciesReference
//
SBMLFormatter&
SBMLFormatter::operator<< (const SpeciesReference_t *sr)
{
  startElement(ELEM_SPECIES_REFERENCE);

  attribute( ATTR_SPECIES      , sr->species       );
  attribute( ATTR_STOICHIOMETRY, sr->stoichiometry );
  attribute( ATTR_DENOMINATOR  , sr->denominator   );

  if ( isEmpty(sr->notes) && isEmpty(sr->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_SPECIES_REFERENCE);
  }

  return *this;
}


//
// KineticLaw
//
SBMLFormatter&
SBMLFormatter::operator<< (const KineticLaw_t *kl)
{
  startElement(ELEM_KINETIC_LAW);

  attribute( ATTR_FORMULA        , kl->formula        );
  attribute( ATTR_TIME_UNITS     , kl->timeUnits      );
  attribute( ATTR_SUBSTANCE_UNITS, kl->substanceUnits );

  if ( isEmpty(kl->notes) && isEmpty(kl->annotation) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    endElement(ELEM_KINETIC_LAW);
  }

  return *this;
}


void
SBMLFormatter::attribute (const XMLCh* name, bool value)
{
  (value == true) ? attribute(name, "true") : attribute(name, "false");
}


void
SBMLFormatter::attribute (const XMLCh* name, int value)
{
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%d", value);
  attribute(name, fNumberBuffer);
}


void
SBMLFormatter::attribute (const XMLCh* name, unsigned int value)
{
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%u", value);
  attribute(name, fNumberBuffer);
}


void
SBMLFormatter::attribute (const XMLCh* name, double value)
{
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%g", value);
  attribute(name, fNumberBuffer);
}


void
SBMLFormatter::attribute (const XMLCh* name, const char *value)
{
  XMLCh* s;


  if (value == NULL) return;

  s = XMLString::transcode(value);
  attribute(name, s);

  delete s;
}


void
SBMLFormatter::attribute (const XMLCh* name, const XMLCh* value)
{
  *fFormatter
    << XMLFormatter::NoEscapes
    << chSpace
    << name
    << chEqual
    << chDoubleQuote
    << XMLFormatter::AttrEscapes
    << value
    << XMLFormatter::NoEscapes
    << chDoubleQuote;

}

/*
void
SBMLFormatter::doIndent ()
{
  for (unsigned int n = 0; n < fIndentLevel; n++)
  {
    *fFormatter << fIndent;
  }
}
*/
