/**
 * Filename    : SBMLFormatter.cpp
 * Description : Formats SBML ...
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
 */


#include "sbml/common.h"
#include "sbml/SBMLFormatter.hpp"
#include "sbml/SBMLUnicodeConstants.hpp"
#include "sbml/XMLUnicodeConstants.hpp"
#include "sbml/XMLUtil.hpp"


/**
 * Ctor
 *
 * Creates a new SBMLFormatter
 *
 * The underlying character encoding is configurable.
 */
SBMLFormatter::SBMLFormatter (const char* outEncoding, XMLFormatTarget* target)
{
  //
  // Initialize() is static and may be called more than once safely.
  //
  XMLPlatformUtils::Initialize();

  fLevel   = 2;
  fVersion = 1;

  fIndentLevel = 0;

  fTarget        = target;
  fMathFormatter = new MathMLFormatter(outEncoding, fTarget, false);
  fFormatter     = XMLUtil::createXMLFormatter(outEncoding, fTarget);

  *fFormatter
    << XML_DECL_1
    << fFormatter->getEncodingName()
    << XML_DECL_2;
}


/**
 * Dtor
 */
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


/**
 * SBMLDocument
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SBMLDocument_t* d)
{
  const XMLCh* xmlns = (fLevel == 1) ? XMLNS_SBML_L1 : XMLNS_SBML_L2;


  fLevel   = d->level;
  fVersion = d->version;

  openStartElement(ELEM_SBML);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) d );

  //
  // xmlns="http://www.sbml.org/level1"  (L1v1, L1v2)
  // xmlns="http://www.sbml.org/level2"  (L2v1)
  //
  attribute(ATTR_XMLNS, xmlns);

  //
  // level: positiveInteger  { use="required" fixed="1" }  (L1v1)
  // level: positiveInteger  { use="required" fixed="2" }  (L2v1)
  //
  attribute(ATTR_LEVEL, d->level);

  //
  // version: positiveInteger  { use="required" fixed="1" }  (L1v1, L2v1)
  // version: positiveInteger  { use="required" fixed="2" }  (L1v2)
  //
  attribute(ATTR_VERSION, d->version);


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


/**
 * Model
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Model_t* m)
{
  if (m == NULL) return *this;


  openStartElement(ELEM_MODEL);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) m );

  //
  // id  { use="optional" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (Model_isSetId(m))
    {
      attribute(ATTR_ID, m->id);
    }
  }

  //
  // name: SName   { use="optional" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L1v2)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if (fLevel == 1)
  {
    if ( Model_isSetName(m) )
    {
      attribute(ATTR_NAME, m->name);
    }
    else if ( Model_isSetId(m) )
    {
      attribute(ATTR_NAME, m->id);
    }
  }
  else
  {
    if (Model_isSetName(m))
    {
      attribute(ATTR_NAME, m->name);
    }
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

    listOfFunctionDefinitions( m->functionDefinition );
    listOfUnitDefinitions    ( m->unitDefinition     );
    listOfCompartments       ( m->compartment        );
    listOfSpecies            ( m->species            );
    listOfParameters         ( m->parameter          );
    listOfRules              ( m->rule               );
    listOfReactions          ( m->reaction           );
    listOfEvents             ( m->event              );
    
    downIndent();

    endElement(ELEM_MODEL);
  }

  return *this;
}


/**
 * FunctionDefinition
 */
SBMLFormatter&
SBMLFormatter::operator<< (const FunctionDefinition_t* fd)
{
  if (fd == NULL) return *this;


  openStartElement(ELEM_FUNCTION_DEFINITION);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) fd );

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, fd->id);
  }

  //
  // name: string  { use="optional" }  (L2v1)
  //
  if ( FunctionDefinition_isSetName(fd) )
  {
    attribute(ATTR_NAME, fd->name);
  }


  if ( isEmpty(fd) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    notesAndAnnotation( (SBase_t*) fd );

    //
    // math: (lambda:Lambda)  (L2v1)
    //
    fMathFormatter->setIndentLevel(fIndentLevel);
    fMathFormatter->startMath();

    *fMathFormatter << FunctionDefinition_getMath(fd);

    fMathFormatter->endMath();

    downIndent();
    endElement(ELEM_FUNCTION_DEFINITION);
  }

  return *this;
}


/**
 * UnitDefinition
 */
SBMLFormatter&
SBMLFormatter::operator<< (const UnitDefinition_t* ud)
{
  if (ud == NULL) return *this;


  openStartElement(ELEM_UNIT_DEFINITION);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) ud );

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, ud->id);
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( UnitDefinition_isSetName(ud) )
  {
    attribute(ATTR_NAME, ud->name);
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, ud->id);    
  }


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


/**
 * Unit
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Unit_t* u)
{
  if (u == NULL) return *this;


  openStartElement(ELEM_UNIT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) u );

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

  //
  // multiplier  { use="optional" default="1" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_MULTIPLIER, u->multiplier);
  }

  //
  // offset  { use="optional" default="0" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_OFFSET, u->offset);
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


/**
 * Compartment
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Compartment_t* c)
{
  if (c == NULL) return *this;


  openStartElement(ELEM_COMPARTMENT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) c );

  //
  // id  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, c->id);
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( Compartment_isSetName(c) )
  {
    attribute(ATTR_NAME, c->name);
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, c->id);    
  }

  //
  // spatialDimensions  { use="optional" default="3" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (c->spatialDimensions != 3)
    {
      attribute(ATTR_SPATIAL_DIMENSIONS, c->spatialDimensions);
    }
  }

  //
  // volume  { use="optional" default="1" }  (L1v1, L1v2)
  // size    { use="optional" }  (L2v1)
  //
  // Although compartment has a default volume of 1 in SBML L1, IEEE 754
  // doubles (or floats) cannot be reliably compared for equality.  To be
  // safe, output the compartment volume even if equal to the default.
  //
  // However, do not output if unset.
  //
  if (fLevel == 1)
  {
    if ( Compartment_isSetVolume(c) )
    {
      attribute(ATTR_VOLUME, Compartment_getVolume(c));
    }
  }
  else
  {
    if ( Compartment_isSetSize(c) )
    {
      attribute(ATTR_SIZE, Compartment_getSize(c));
    }
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

  //
  // constant  { use="optional" default="true" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (c->constant != 1)
    {
      attribute(ATTR_CONSTANT, (bool) c->constant);
    }
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


/**
 * Species
 */
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

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) s );

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, Species_getId(s));
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( Species_isSetName(s) )
  {
    attribute(ATTR_NAME, Species_getName(s));
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, Species_getId(s));
  }

  //
  // compartment: SName  { use="required" }  (L1v1, L2v1)
  // compartment: SId    { use="required" }  (L2v1)
  //
  attribute(ATTR_COMPARTMENT, s->compartment);

  //
  // initialAmount: double  { use="required" }  (L1v1, L1v2)
  // initialAmount: double  { use="optional" }  (L2v1)
  //
  if ( Species_isSetInitialAmount(s) )
  {
    attribute(ATTR_INITIAL_AMOUNT, Species_getInitialAmount(s));
  }

  //
  // initialConcentration: double  { use="optional" }  (L2v1)
  //
  else if ((fLevel > 1) && Species_isSetInitialConcentration(s))
  {
    attribute(ATTR_INITIAL_CONCENTRATION, Species_getInitialConcentration(s));
  }

  //
  //          units: SName  { use="optional" }  (L1v1, L1v2)
  // substanceUntis: SId    { use="optional" }  (L2v1)
  //
  if ((fLevel > 1) && Species_isSetSubstanceUnits(s))
  {
    attribute(ATTR_SUBSTANCE_UNITS, Species_getSubstanceUnits(s));
  }
  else if ( Species_isSetUnits(s) )
  {
    attribute(ATTR_UNITS, Species_getUnits(s));
  }

  if (fLevel > 1)
  {
    //
    // spatialSizeUnits: SId  { use="optional" }  (L2v1)
    //
    if ( Species_isSetSpatialSizeUnits(s) )
    {
      attribute(ATTR_SPATIAL_SIZE_UNITS, Species_getSpatialSizeUnits(s));
    }

    //
    // hasOnlySubstanceUnits: boolean
    // { use="optional" default="false" (0) }  (L2v1)
    //
    if (Species_getHasOnlySubstanceUnits(s) != 0)
    {
      attribute( ATTR_HAS_ONLY_SUBSTANCE_UNITS,
                 (bool) Species_getHasOnlySubstanceUnits(s) );
    }
  }

  //
  // boundaryCondition: boolean
  // { use="optional" default="false" (0) }  (L1v1, L1v2, L2v1)
  // 
  //
  if (Species_getBoundaryCondition(s) != 0)
  {
    attribute(ATTR_BOUNDARY_CONDITION, (bool) Species_getBoundaryCondition(s));
  }

  //
  // charge: integer  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if ( Species_isSetCharge(s) )
  {
    attribute(ATTR_CHARGE, Species_getCharge(s));
  }

  //
  // constant: boolean  { use="optional" default="false" (0) }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (Species_getConstant(s) != 0)
    {
      attribute(ATTR_CONSTANT, (bool) Species_getConstant(s));
    }
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


/**
 * Parameter
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Parameter_t* p)
{
  if (p == NULL) return *this;


  openStartElement(ELEM_PARAMETER);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) p );

  //
  // id  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, p->id);
  }

  //
  // name: SName  { use="required" }  (L1v1, L1v2)
  // name: SId    { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if (Parameter_isSetName(p))
  {
    attribute(ATTR_NAME, p->name);
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, p->id);    
  }

  //
  // value: double  { use="required" }  (L1v2)
  // value: double  { use="optional" }  (L1v2, L2v1)
  //
  if ((fLevel == 1 && fVersion == 1) || Parameter_isSetValue(p))
  {
    attribute(ATTR_VALUE, p->value);
  }

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  // units: SId    { use="optional" }  (L2v1)
  //
  if (p->units != NULL)
  {
    attribute(ATTR_UNITS, p->units);
  }

  //
  // constant: boolean  { use="optional" default="true" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (p->constant != 1)
    {
      attribute(ATTR_CONSTANT, (bool) p->constant);
    }
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


/**
 * Rule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Rule_t* r)
{
  if (r == NULL) return *this;


  switch (r->typecode)
  {
    case SBML_ASSIGNMENT_RULE:
      *this << (AssignmentRule_t*) r;
      break;

    case SBML_RATE_RULE:
      *this << (RateRule_t*) r;
      break;

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


/**
 * AssignmentRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const AssignmentRule_t* ar)
{
  if (ar == NULL) return *this;


  const XMLCh* elem = ELEM_ASSIGNMENT_RULE;


  //
  // Format L1 <assigmentRule type="rate" ...> as L2 <rateRule ...>
  // 
  if ((fLevel > 1) && (AssignmentRule_getType(ar) == RULE_TYPE_RATE))
  {
    elem = ELEM_RATE_RULE;
  }

  openStartElement(elem);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) ar );

  //
  // variable: SId  { use="required" }  (L2v1)
  //
  attribute(ATTR_VARIABLE, ar->variable);

  if ( isEmpty((Rule_t*) ar) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) ar );

    //
    // math: Math  (L2v1)
    //
    doRuleMath( (Rule_t*) ar );

    downIndent();
    endElement(elem);
  }

  return *this;
}


/**
 * RateRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const RateRule_t* rr)
{
  if (rr == NULL) return *this;


  openStartElement(ELEM_RATE_RULE);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) rr );

  //
  // variable: SId  { use="required" }  (L2v1)
  //
  attribute(ATTR_VARIABLE, rr->variable);

  if ( isEmpty((Rule_t*) rr) )
  {
    slashCloseStartElement();
  }
  else
  {
    upIndent();

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) rr );

    //
    // math: Math  (L2v1)
    //
    doRuleMath( (Rule_t*) rr );

    downIndent();
    endElement(ELEM_RATE_RULE);
  }

  return *this;
}


/**
 * AlgebraicRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const AlgebraicRule_t* ar)
{
  if (ar == NULL) return *this;


  openStartElement(ELEM_ALGEBRAIC_RULE);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) ar );

  if (fLevel == 1)
  {
    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_FORMULA, ar->formula);
  }

  if ( isEmpty((Rule_t*) ar) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) ar );

    //
    // math: Math  (L2v1)
    //
    doRuleMath( (Rule_t*) ar );

    downIndent();
    endElement(ELEM_ALGEBRAIC_RULE);
  }

  return *this;
}


/**
 * SpeciesConcentrationRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SpeciesConcentrationRule_t* scr)
{
  if (scr == NULL) return *this;


  //
  // Level 2
  //
  // A SpeciesConcentrationRule is either an AssignmentRule or a RateRule.
  // operator<< (const AssignmentRule_t*) formats either case.
  //
  if (fLevel > 1)
  {
    *this << (AssignmentRule_t*) scr;
  }

  //
  // Level 1
  //
  else
  {
    const XMLCh* elem = ELEM_SPECIES_CONCENTRATION_RULE;
    const XMLCh* attr = ATTR_SPECIES;


    if ((fLevel == 1) && (fVersion == 1))
    {
      elem = ELEM_SPECIE_CONCENTRATION_RULE;
      attr = ATTR_SPECIE;
    }

    openStartElement(elem);

    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_FORMULA, scr->formula);

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    doRuleType(scr->type);

    //
    // specie : SName   { use="required" }  (L1v1)
    // species: SName   { use="required" }  (L1v2)
    //
    attribute(attr, SpeciesConcentrationRule_getSpecies(scr));

    if ( isEmpty((Rule_t*) scr) )
    {
      slashCloseStartElement();
    }
    else
    {
      endElement(elem, (SBase_t*) scr);
    }
  }

  return *this;
}


/**
 * CompartmentVolumeRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const CompartmentVolumeRule_t* cvr)
{
  if (cvr == NULL) return *this;


  //
  // Level 2
  //
  // A CompartmentVolumeRule is either an AssignmentRule or a RateRule.
  // operator<< (const AssignmentRule_t*) formats either case.
  //
  if (fLevel > 1)
  {
    *this << (AssignmentRule_t*) cvr;
  }

  //
  // Level 1
  //
  else
  {
    openStartElement(ELEM_COMPARTMENT_VOLUME_RULE);

    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_FORMULA, cvr->formula);

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    doRuleType(cvr->type);

    //
    // compartment: SName  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_COMPARTMENT, CompartmentVolumeRule_getCompartment(cvr));

    if ( isEmpty((Rule_t*) cvr) )
    {
      slashCloseStartElement();
    }
    else
    {
      endElement(ELEM_COMPARTMENT_VOLUME_RULE, (SBase_t*) cvr);
    }
  }

  return *this;
}


/**
 * ParameterRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const ParameterRule_t* pr)
{
  if (pr == NULL) return *this;


  //
  // Level 2
  //
  // A ParameterRule is either an AssignmentRule or a RateRule.
  // operator<< (const AssignmentRule_t*) formats either case.
  //
  if (fLevel > 1)
  {
    *this << (AssignmentRule_t*) pr;
  }

  //
  // Level 1
  //
  else
  {
    openStartElement(ELEM_PARAMETER_RULE);

    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_FORMULA, pr->formula);

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    doRuleType(pr->type);

    //
    // name: SName  { use="required" } (L1v1, L1v2)
    //
    attribute(ATTR_NAME, ParameterRule_getName(pr));

    //
    // units  { use="optional" }  (L1v1, L1v2);
    //
    if (pr->units != NULL)
    {
      attribute(ATTR_UNITS, pr->units);
    }

    if ( isEmpty((Rule_t*) pr) )
    {
      slashCloseStartElement();
    }
    else
    {
      endElement(ELEM_PARAMETER_RULE, (SBase_t*) pr);
    }
  }

  return *this;
}


/**
 * Reaction
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Reaction_t* r)
{
  if (r == NULL) return *this;


  openStartElement(ELEM_REACTION);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) r );

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, r->id);
  }

  //
  // name  { use="required" }  (L1v1, L1v2)
  // name  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if (Reaction_isSetName(r))
  {
    attribute(ATTR_NAME, r->name);
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, r->id);    
  }

  //
  // reversible: boolean
  // { use="optional"  default="true" (1) }  (L1v1, L1v2, L2v1)
  //
  if (r->reversible != 1)
  {
    attribute(ATTR_REVERSIBLE, (bool) r->reversible);
  }

  //
  // fast: boolean  { use="optional" default="false" (0) }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }  (L2v1)
  //
  if (fLevel == 1)
  {
    if (r->fast != 0)
    {
      attribute(ATTR_FAST, (bool) r->fast);
    }
  }
  else
  {
    if ( Reaction_isSetFast(r) )
    {
      attribute(ATTR_FAST, (bool) r->fast);
    }
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
    listOfModifiers( r->modifier );

    *this << r->kineticLaw;

    downIndent();
    endElement(ELEM_REACTION);
  }

  return *this;
}


/**
 * SimpleSpeciesReference
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SimpleSpeciesReference_t* ssr)
{
  if (ssr == NULL) return *this;


  switch (ssr->typecode)
  {
    case SBML_SPECIES_REFERENCE:
      *this << (SpeciesReference_t*) ssr;
      break;

    case SBML_MODIFIER_SPECIES_REFERENCE:
      *this << (ModifierSpeciesReference_t*) ssr;
      break;

    default:
      break;
  }

  return *this;
}


/**
 * SpeciesReference
 */
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

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) sr );

  //
  // species: SName  { use="required" }  (L1v1, L1v2)
  // species: SId    { use="required" }  (L2v1)
  //
  attribute(attr, sr->species);

  //
  // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
  // stoichiometry: double   { use="optional" default="1" }  (L2v1)
  //
  if (sr->stoichiometry != 1)
  {
    if (fLevel == 1)
    {
      attribute(ATTR_STOICHIOMETRY, (int) sr->stoichiometry);
    }
    else if ( !SpeciesReference_isSetStoichiometryMath(sr) &&
              SpeciesReference_getDenominator(sr) == 1 )
    {
      attribute(ATTR_STOICHIOMETRY, sr->stoichiometry);
    }
  }

  //
  // denominator  { use="optional" default="1" }  (L1v1, L1v2)
  //
  if (fLevel == 1)
  {
    if (sr->denominator != 1)
    {
      attribute(ATTR_DENOMINATOR, sr->denominator);
    }
  }

  if ( isEmpty(sr) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) sr );

    //
    // stoichiometryMath: StoichiometryMath  { use="optional" } (L2v1)
    //
    doStoichiometryMath(sr);

    downIndent();
    endElement(elem);
  }

  return *this;
}


/**
 * ModifierSpeciesReference
 */
SBMLFormatter&
SBMLFormatter::operator<< (const ModifierSpeciesReference_t* msr)
{
  if (msr == NULL) return *this;


  openStartElement(ELEM_MODIFIER_SPECIES_REFERENCE);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) msr );

  //
  // species: SId  { use="required" }  (L2v1)
  //
  attribute(ELEM_SPECIES, msr->species);

  if ( isEmpty(msr) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_MODIFIER_SPECIES_REFERENCE, (SBase_t*) msr);
  }

  return *this;
}


/**
 * KineticLaw
 */
SBMLFormatter&
SBMLFormatter::operator<< (const KineticLaw_t* kl)
{
  if (kl == NULL) return *this;


  openStartElement(ELEM_KINETIC_LAW);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) kl );

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (fLevel == 1)
  {
    attribute(ATTR_FORMULA, kl->formula);
  }

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

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) kl );

    //
    // math: Math  (L2v1)
    //
    doKineticLawMath(kl);

    listOfParameters(kl->parameter);

    downIndent();
    endElement(ELEM_KINETIC_LAW);
  } 

  return *this;
}


/**
 * Event
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Event_t* e)
{
  if (e == NULL) return *this;


  openStartElement(ELEM_EVENT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) e );

  //
  // id: SId  { use="optional" }  (L2v1)
  //
  if ( Event_isSetId(e) )
  {
    attribute(ATTR_ID, Event_getId(e));
  }

  //
  // name: string  { use="optional" }  (L2v1)
  //
  if ( Event_isSetName(e) )
  {
    attribute(ATTR_NAME, Event_getName(e));
  }

  //
  // timeUnits: SId  { use="optional" }  (L2v1)
  //
  if ( Event_isSetTimeUnits(e) )
  {
    attribute(ATTR_TIME_UNITS, Event_getTimeUnits(e));
  }

  if ( isEmpty(e) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) e );

    //
    // trigger: Math  (L2v1)
    // 
    if ( Event_isSetTrigger(e) )
    {
      startElement(ELEM_TRIGGER);

      fMathFormatter->setIndentLevel(fIndentLevel + 1);
      fMathFormatter->startMath();

      *fMathFormatter << Event_getTrigger(e);

      fMathFormatter->endMath();

      endElement(ELEM_TRIGGER);
    }

    //
    // delay: Math  (L2v1)
    //
    if ( Event_isSetDelay(e) )
    {
      startElement(ELEM_DELAY);

      fMathFormatter->setIndentLevel(fIndentLevel + 1);
      fMathFormatter->startMath();

      *fMathFormatter << Event_getDelay(e);

      fMathFormatter->endMath();

      endElement(ELEM_DELAY);
    }

    //
    // eventAssignment: EventAssignment[1..*]
    //
    listOfEventAssignments( Event_getListOfEventAssignments(e) );

    downIndent();
    endElement(ELEM_EVENT);
  }

  return *this;
}


/**
 * EventAssignment
 */
SBMLFormatter&
SBMLFormatter::operator<< (const EventAssignment_t* ea)
{
  if (ea == NULL) return *this;


  openStartElement(ELEM_EVENT_ASSIGNMENT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId( (SBase_t*) ea );

  //
  // variable: SId  { use="required" }  (L2v1)
  //
  attribute(ATTR_VARIABLE, EventAssignment_getVariable(ea));


  if ( isEmpty(ea) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    //
    //      notes: (ANY)  { minOccurs="0" }
    // annotation: (ANY)  { minOccurs="0" }
    //
    notesAndAnnotation( (SBase_t*) ea );

    //
    // math: Math  (L2v1)
    // 
    if ( EventAssignment_isSetMath(ea) )
    {
      fMathFormatter->setIndentLevel(fIndentLevel);
      fMathFormatter->startMath();

      *fMathFormatter << EventAssignment_getMath(ea);

      fMathFormatter->endMath();
    }

    downIndent();
    endElement(ELEM_EVENT_ASSIGNMENT);
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
#define makeFn(name, element, type)                      \
void                                                     \
SBMLFormatter::name (ListOf_t* list)                     \
{                                                        \
  unsigned int size = ListOf_getNumItems(list);          \
                                                         \
                                                         \
  if (size > 0)                                          \
  {                                                      \
    openStartElement( element );                         \
    doMetaId( (SBase_t*) list );                         \
    closeStartElement();                                 \
                                                         \
    upIndent();                                          \
                                                         \
    if (fLevel > 1) notesAndAnnotation((SBase_t*) list); \
                                                         \
    for (unsigned int n = 0; n < size; n++)              \
    {                                                    \
      *this << ( type * ) ListOf_get(list, n);           \
    }                                                    \
                                                         \
    downIndent();                                        \
                                                         \
    endElement( element );                               \
  }                                                      \
}

makeFn( listOfFunctionDefinitions,
        ELEM_LIST_OF_FUNCTION_DEFINITIONS,
        FunctionDefinition_t )

makeFn( listOfUnitDefinitions, ELEM_LIST_OF_UNIT_DEFINITIONS, UnitDefinition_t )
makeFn( listOfUnits          , ELEM_LIST_OF_UNITS           , Unit_t           )
makeFn( listOfCompartments   , ELEM_LIST_OF_COMPARTMENTS    , Compartment_t    )
makeFn( listOfSpecies        , ELEM_LIST_OF_SPECIES         , Species_t        )
makeFn( listOfParameters     , ELEM_LIST_OF_PARAMETERS      , Parameter_t      )
makeFn( listOfRules          , ELEM_LIST_OF_RULES           , Rule_t           )
makeFn( listOfReactions      , ELEM_LIST_OF_REACTIONS       , Reaction_t       )
makeFn( listOfReactants      , ELEM_LIST_OF_REACTANTS     , SpeciesReference_t )
makeFn( listOfProducts       , ELEM_LIST_OF_PRODUCTS      , SpeciesReference_t )

makeFn( listOfModifiers,
        ELEM_LIST_OF_MODIFIERS,
        ModifierSpeciesReference_t );

makeFn( listOfEvents, ELEM_LIST_OF_EVENTS, Event_t )

makeFn( listOfEventAssignments,
        ELEM_LIST_OF_EVENT_ASSIGNMENTS,
        EventAssignment_t )

#undef makeFn


/**
 * Notes
 */
void
SBMLFormatter::notes (const char* s)
{
  if (isEmpty(s)) return;


  startElement(ELEM_NOTES);

  upIndent();
  indent();

  XMLCh* x = XMLString::transcode(s);
  *fFormatter << x << chLF;
  XMLString::release(&x);

  downIndent();

  endElement(ELEM_NOTES);
}


/**
 * Annotation
 */
void
SBMLFormatter::annotation (const char* s)
{
  if (isEmpty(s)) return;


  indent();

  XMLCh* x = XMLString::transcode(s);
  *fFormatter << x << chLF;
  delete [] x;
}


/**
 * Notes and Annotation
 */
void
SBMLFormatter::notesAndAnnotation(const SBase_t* sb)
{
  notes(sb->notes);
  annotation(sb->annotation);
}


/**
 * Outputs the <math> element for KineticLaw (L2 only).
 *
 * This method does the nescessary conversion if the KineticLaw has only
 * a formula string set.
 */
void
SBMLFormatter::doKineticLawMath (const KineticLaw_t* kl)
{
  ASTNode_t* math;


  //
  // math: Math  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( KineticLaw_isSetMath(kl) || KineticLaw_isSetFormula(kl) )
    {
      fMathFormatter->setIndentLevel(fIndentLevel);
      fMathFormatter->startMath();

      if ( KineticLaw_isSetMath(kl) )
      {
        *fMathFormatter << KineticLaw_getMath(kl);
      }
      else
      {
        math = SBML_parseFormula( KineticLaw_getFormula(kl) );
        *fMathFormatter << math;
        ASTNode_free(math);
      }

      fMathFormatter->endMath();
    }
  }
}


/**
 * Outputs the metaid attribute for the given SBML object (L2 only).
 */
void
SBMLFormatter::doMetaId (const SBase_t* sb)
{
  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( SBase_isSetMetaId(sb) )
    {
      attribute(ATTR_META_ID, SBase_getMetaId(sb));
    }
  }
}


/**
 * Outputs the <math> element for Rules (L2 only).
 *
 * This method does the nescessary conversion if the rule has only a
 * formula string set.
 */
void
SBMLFormatter::doRuleMath (const Rule_t* r)
{
  ASTNode_t* math;


  //
  // math: Math  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( Rule_isSetMath(r) || Rule_isSetFormula(r) )
    {
      fMathFormatter->setIndentLevel(fIndentLevel);
      fMathFormatter->startMath();

      if ( Rule_isSetMath(r) )
      {
        *fMathFormatter << Rule_getMath(r);
      }
      else
      {
        math = SBML_parseFormula( Rule_getFormula(r) );
        *fMathFormatter << math;
        ASTNode_free(math);
      }

      fMathFormatter->endMath();
    }
  }
}


/**
 * Outputs the type attribute for Rules (L1 only).
 */
void
SBMLFormatter::doRuleType (const RuleType_t type)
{
  //
  // type  { use="optional" default="scalar" }  (L1v1, L1v2)
  //
  if (type != RULE_TYPE_SCALAR)
  {
    attribute(ATTR_TYPE, RuleType_toString(type));
  }
}


/**
 * Outputs the <stoichiometryMath> element for SpeciesReference (L2 only).
 */
void
SBMLFormatter::doStoichiometryMath (const SpeciesReference_t* sr)
{
  int denominator = SpeciesReference_getDenominator(sr);

  ASTNode_t* node;
  int        numerator;


  //
  // stoichiometryMath: StoichiometryMath  { use="optional" } (L2v1)
  //
  // Either output the stoichiometryMath field directly or output
  // <cn type='rational'> stoichiometry <sep/> denominator </cn>
  //
  if (fLevel > 1)
  {
    if (SpeciesReference_isSetStoichiometryMath(sr) || denominator != 1)
    {
      startElement(ELEM_STOICHIOMETRY_MATH);
      
      fMathFormatter->setIndentLevel(fIndentLevel + 1);
      fMathFormatter->startMath();

      if ( SpeciesReference_isSetStoichiometryMath(sr) )
      {
        *fMathFormatter << SpeciesReference_getStoichiometryMath(sr);
      }
      else
      {
        numerator   = (long) SpeciesReference_getStoichiometry(sr);
        denominator = (long) SpeciesReference_getDenominator  (sr);

        node = ASTNode_createWithType(AST_RATIONAL);
        ASTNode_setRational(node, numerator, denominator);

        *fMathFormatter << node;

        ASTNode_free(node);
      }

      fMathFormatter->endMath();

      endElement(ELEM_STOICHIOMETRY_MATH);
    }
  }
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


/**
 * @return true if the given Rule contains no child XML elements.
 */
bool
SBMLFormatter::isEmpty (const Rule_t *r)
{
  bool result = isEmpty((SBase_t*) r);


  if (fLevel > 1)
  {
    result = result && !(Rule_isSetFormula(r) || Rule_isSetMath(r));
  }

  return result;
}


/**
 * @return true if the given SpeciesReference contains no child XML
 * elements.
 */
bool
SBMLFormatter::isEmpty (const SpeciesReference_t *sr)
{
  bool result = isEmpty((SBase_t*) sr);


  if (fLevel > 1)
  {
    result = result &&
             
             !( SpeciesReference_isSetStoichiometryMath(sr) ||
                SpeciesReference_getDenominator(sr) != 1 );
  }

  return result;
}


/**
 * @return true if the given KineticLaw contains no child XML elements.
 */
bool
SBMLFormatter::isEmpty (const KineticLaw_t* kl)
{
  bool result = isEmpty((SBase_t*) kl) &&
                (KineticLaw_getNumParameters(kl) == 0);


  if (fLevel > 1)
  {
    result = result &&
             !( KineticLaw_isSetMath(kl) || KineticLaw_isSetFormula(kl) );
  }

  return result;
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
  return isEmpty((SBase_t*) m)                     &&
         (Model_getNumFunctionDefinitions(m) == 0) &&
         (Model_getNumUnitDefinitions    (m) == 0) &&
         (Model_getNumCompartments       (m) == 0) &&
         (Model_getNumSpecies            (m) == 0) &&
         (Model_getNumParameters         (m) == 0) &&
         (Model_getNumRules              (m) == 0) &&
         (Model_getNumReactions          (m) == 0) &&
         (Model_getNumEvents             (m) == 0);
}


bool
SBMLFormatter::isEmpty (const FunctionDefinition_t* fd)
{
  return isEmpty((SBase_t*) fd) && (FunctionDefinition_isSetMath(fd) == 0);
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
         (Reaction_getNumModifiers(r) == 0) &&
         r->kineticLaw == NULL;
}


bool
SBMLFormatter::isEmpty (const Event_t* e)
{
  return isEmpty((SBase_t*) e)                  &&
         (Event_isSetTrigger          (e) == 0) &&
         (Event_isSetDelay            (e) == 0) &&
         (Event_getNumEventAssignments(e) == 0);
}


bool
SBMLFormatter::isEmpty (const EventAssignment_t* ea)
{
  return isEmpty((SBase_t*) ea) && (EventAssignment_isSetMath(ea) == 0);
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
makeFn( ModifierSpeciesReference_t )

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
  if ( isnan(value) )
  {
    attribute(name, VAL_NAN);
  }
  else if ( util_isInf(value) == 1)
  {
    attribute(name, VAL_INF);
  }
  else if ( util_isInf(value) == -1)
  {
    attribute(name, VAL_NEG_INF);
  }
  else if ( util_isNegZero(value) )
  {
    attribute(name, VAL_NEG_ZERO);
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
