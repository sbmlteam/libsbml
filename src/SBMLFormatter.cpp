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
 *   Stephan Hoops
 */


#include "sbml/common.h"


#ifdef USE_EXPAT
#  include "ExpatXMLString.hpp"
#  include "ExpatFormatter.hpp"
#endif  // USE_EXPAT


#include "sbml/SBMLFormatter.hpp"
#include "sbml/SBMLUnicodeConstants.hpp"
#include "sbml/XMLUnicodeConstants.hpp"
#include "sbml/XMLUtil.hpp"


#define NUMBER_BUFFER_SIZE 100


/**
 * Ctor
 *
 * Creates a new SBMLFormatter
 *
 * The underlying character encoding is configurable.
 */
SBMLFormatter::SBMLFormatter (const char* outEncoding, XMLFormatTarget* target)
{
#ifndef USE_EXPAT
  //
  // Initialize() is static and may be called more than once safely.
  //
  XMLPlatformUtils::Initialize();
#endif  // !USE_EXPAT

  fLevel   = 2;
  fVersion = 1;

  fIndentLevel = 0;

  fTarget        = target;
  fMathFormatter = new MathMLFormatter(outEncoding, fTarget, false);
  fFormatter     = XMLUtil::createXMLFormatter(outEncoding, fTarget);
  fNumberBuffer  = new char[ NUMBER_BUFFER_SIZE ];

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
  delete fNumberBuffer;
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
SBMLFormatter::operator<< (const SBMLDocument& d)
{
  fLevel   = d.getLevel();
  fVersion = d.getVersion();

  openStartElement(ELEM_SBML);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(d);

  //
  // xmlns="http://www.sbml.org/level1"  (L1v1, L1v2)
  // xmlns="http://www.sbml.org/level2"  (L2v1)
  //
  attribute(ATTR_XMLNS, (fLevel == 1) ? XMLNS_SBML_L1 : XMLNS_SBML_L2);

  //
  // level: positiveInteger  { use="required" fixed="1" }  (L1v1)
  // level: positiveInteger  { use="required" fixed="2" }  (L2v1)
  //
  attribute(ATTR_LEVEL, d.getLevel());

  //
  // version: positiveInteger  { use="required" fixed="1" }  (L1v1, L2v1)
  // version: positiveInteger  { use="required" fixed="2" }  (L1v2)
  //
  attribute(ATTR_VERSION, d.getVersion());


  if (d.model == NULL)
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    *this << *(d.model);

    downIndent();

    endElement(ELEM_SBML);
  }

  return *this;
}


/**
 * Model
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Model& m)
{
  openStartElement(ELEM_MODEL);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(m);

  //
  // id  { use="optional" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( m.isSetId() )
    {
      attribute(ATTR_ID, m.getId());
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
    if ( m.isSetName() )
    {
      attribute(ATTR_NAME, m.getName());
    }
    else if ( m.isSetId() )
    {
      attribute(ATTR_NAME, m.getId());
    }
  }
  else
  {
    if ( m.isSetName() )
    {
      attribute(ATTR_NAME, m.getName());
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

    notesAndAnnotation(m);

    listOfFunctionDefinitions( m.functionDefinition );
    listOfUnitDefinitions    ( m.unitDefinition     );
    listOfCompartments       ( m.compartment        );
    listOfSpecies            ( m.species            );
    listOfParameters         ( m.parameter          );
    listOfRules              ( m.rule               );
    listOfReactions          ( m.reaction           );
    listOfEvents             ( m.event              );
    
    downIndent();

    endElement(ELEM_MODEL);
  }

  return *this;
}


/**
 * FunctionDefinition
 */
SBMLFormatter&
SBMLFormatter::operator<< (const FunctionDefinition& fd)
{
  openStartElement(ELEM_FUNCTION_DEFINITION);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(fd);

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, fd.getId());
  }

  //
  // name: string  { use="optional" }  (L2v1)
  //
  if ( fd.isSetName() )
  {
    attribute(ATTR_NAME, fd.getName());
  }


  if ( isEmpty(fd) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();
    upIndent();

    notesAndAnnotation(fd);

    //
    // math: (lambda:Lambda)  (L2v1)
    //
    fMathFormatter->setIndentLevel(fIndentLevel);
    fMathFormatter->startMath();

    *fMathFormatter << fd.getMath();

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
SBMLFormatter::operator<< (const UnitDefinition& ud)
{
  openStartElement(ELEM_UNIT_DEFINITION);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(ud);

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, ud.getId());
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( ud.isSetName() )
  {
    attribute(ATTR_NAME, ud.getName());
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, ud.getId());
  }


  if ( isEmpty(ud) )
  {
    slashCloseStartElement();
  }
  else
  {
    closeStartElement();

    upIndent();

    notesAndAnnotation(ud);
    listOfUnits( ud.unit );

    downIndent();

    endElement(ELEM_UNIT_DEFINITION);
  }

  return *this;
}


/**
 * Unit
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Unit& u)
{
  openStartElement(ELEM_UNIT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(u);

  attribute(ATTR_KIND, UnitKind_toString( u.getKind() ));

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1)
  //
  if (u.getExponent() != 1)
  {
    attribute(ATTR_EXPONENT, u.getExponent());
  }

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1)
  //
  if (u.getScale() != 0)
  {
    attribute(ATTR_SCALE, u.getScale());
  }

  //
  // multiplier  { use="optional" default="1" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_MULTIPLIER, u.getMultiplier());
  }

  //
  // offset  { use="optional" default="0" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_OFFSET, u.getOffset());
  }


  if ( isEmpty(u) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_UNIT, u);
  }

  return *this;
}


/**
 * Compartment
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Compartment& c)
{
  openStartElement(ELEM_COMPARTMENT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(c);

  //
  // id  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, c.getId());
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( c.isSetName() )
  {
    attribute(ATTR_NAME, c.getName());
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, c.getId());
  }

  //
  // spatialDimensions  { use="optional" default="3" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (c.getSpatialDimensions() != 3)
    {
      attribute(ATTR_SPATIAL_DIMENSIONS, c.getSpatialDimensions());
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
    if ( c.isSetVolume() )
    {
      attribute(ATTR_VOLUME, c.getVolume());
    }
  }
  else
  {
    if ( c.isSetSize() )
    {
      attribute(ATTR_SIZE, c.getSize());
    }
  }

  //
  // units  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if ( c.isSetUnits() )
  {
    attribute(ATTR_UNITS, c.getUnits());
  }

  //
  // outside  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if ( c.isSetOutside() )
  {
    attribute(ATTR_OUTSIDE, c.getOutside());
  }

  //
  // constant  { use="optional" default="true" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (c.getConstant() != true)
    {
      attribute(ATTR_CONSTANT, c.getConstant());
    }
  }


  if ( isEmpty(c) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_COMPARTMENT, c);
  }

  return *this;
}


/**
 * Species
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Species& s)
{
  const XMLCh* elem = ELEM_SPECIES;


  if ((fLevel == 1) && (fVersion == 1))
  {
    elem = ELEM_SPECIE;
  }

  openStartElement(elem);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(s);

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, s.getId());
  }

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  // name: string  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( s.isSetName() )
  {
    attribute(ATTR_NAME, s.getName());
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, s.getId());
  }

  //
  // compartment: SName  { use="required" }  (L1v1, L2v1)
  // compartment: SId    { use="required" }  (L2v1)
  //
  attribute(ATTR_COMPARTMENT, s.getCompartment());

  //
  // initialAmount: double  { use="required" }  (L1v1, L1v2)
  // initialAmount: double  { use="optional" }  (L2v1)
  //
  if ( s.isSetInitialAmount() )
  {
    attribute(ATTR_INITIAL_AMOUNT, s.getInitialAmount());
  }

  //
  // initialConcentration: double  { use="optional" }  (L2v1)
  //
  else if ((fLevel > 1) && s.isSetInitialConcentration())
  {
    attribute(ATTR_INITIAL_CONCENTRATION, s.getInitialConcentration());
  }

  //
  //          units: SName  { use="optional" }  (L1v1, L1v2)
  // substanceUntis: SId    { use="optional" }  (L2v1)
  //
  if ((fLevel > 1) && s.isSetSubstanceUnits())
  {
    attribute(ATTR_SUBSTANCE_UNITS, s.getSubstanceUnits());
  }
  else if ( s.isSetUnits() )
  {
    attribute(ATTR_UNITS, s.getUnits());
  }

  if (fLevel > 1)
  {
    //
    // spatialSizeUnits: SId  { use="optional" }  (L2v1)
    //
    if ( s.isSetSpatialSizeUnits() )
    {
      attribute(ATTR_SPATIAL_SIZE_UNITS, s.getSpatialSizeUnits());
    }

    //
    // hasOnlySubstanceUnits: boolean
    // { use="optional" default="false" (0) }  (L2v1)
    //
    if (s.getHasOnlySubstanceUnits() != false)
    {
      attribute(ATTR_HAS_ONLY_SUBSTANCE_UNITS, s.getHasOnlySubstanceUnits());
    }
  }

  //
  // boundaryCondition: boolean
  // { use="optional" default="false" (0) }  (L1v1, L1v2, L2v1)
  // 
  //
  if (s.getBoundaryCondition() != false)
  {
    attribute(ATTR_BOUNDARY_CONDITION, s.getBoundaryCondition());
  }

  //
  // charge: integer  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if ( s.isSetCharge() )
  {
    attribute(ATTR_CHARGE, s.getCharge());
  }

  //
  // constant: boolean  { use="optional" default="false" (0) }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (s.getConstant() != false)
    {
      attribute(ATTR_CONSTANT, s.getConstant());
    }
  }

  if ( isEmpty(s) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(elem, s);
  }

  return *this;
}


/**
 * Parameter
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Parameter& p)
{
  openStartElement(ELEM_PARAMETER);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(p);

  //
  // id  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, p.getId());
  }

  //
  // name: SName  { use="required" }  (L1v1, L1v2)
  // name: SId    { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( p.isSetName() )
  {
    attribute(ATTR_NAME, p.getName());
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, p.getId());
  }

  //
  // value: double  { use="required" }  (L1v2)
  // value: double  { use="optional" }  (L1v2, L2v1)
  //
  if ((fLevel == 1 && fVersion == 1) || p.isSetValue())
  {
    attribute(ATTR_VALUE, p.getValue());
  }

  //
  // units: SName  { use="optional" }  (L1v1, L1v2)
  // units: SId    { use="optional" }  (L2v1)
  //
  if ( p.isSetUnits() )
  {
    attribute(ATTR_UNITS, p.getUnits());
  }

  //
  // constant: boolean  { use="optional" default="true" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if (p.getConstant() != true)
    {
      attribute(ATTR_CONSTANT, p.getConstant());
    }
  }


  if ( isEmpty(p) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_PARAMETER, p);
  }

  return *this;
}


/**
 * Rule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Rule& r)
{
  switch (r.getTypeCode())
  {
    case SBML_ASSIGNMENT_RULE:
      *this << static_cast<const AssignmentRule&>(r);
      break;

    case SBML_RATE_RULE:
      *this << static_cast<const RateRule&>(r);
      break;

    case SBML_ALGEBRAIC_RULE:
      *this << static_cast<const AlgebraicRule&>(r);
      break;

    case SBML_SPECIES_CONCENTRATION_RULE:
      *this << static_cast<const SpeciesConcentrationRule&>(r);
      break;
      
    case SBML_COMPARTMENT_VOLUME_RULE:
      *this << static_cast<const CompartmentVolumeRule&>(r);
      break;

    case SBML_PARAMETER_RULE:
      *this << static_cast<const ParameterRule&>(r);
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
SBMLFormatter::operator<< (const AssignmentRule& ar)
{
  const XMLCh* elem = ELEM_ASSIGNMENT_RULE;


  //
  // Format L1 <assigmentRule type="rate" ...> as L2 <rateRule ...>
  // 
  if ((fLevel > 1) && (ar.getType() == RULE_TYPE_RATE))
  {
    elem = ELEM_RATE_RULE;
  }

  openStartElement(elem);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(ar);

  //
  // variable: SId  { use="required" }  (L2v1)
  //
  attribute(ATTR_VARIABLE, ar.getVariable());

  if ( isEmpty(static_cast<Rule>(ar)) )
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
    notesAndAnnotation(ar);

    //
    // math: Math  (L2v1)
    //
    doMath(ar);

    downIndent();
    endElement(elem);
  }

  return *this;
}


/**
 * RateRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const RateRule& rr)
{
  openStartElement(ELEM_RATE_RULE);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(rr);

  //
  // variable: SId  { use="required" }  (L2v1)
  //
  attribute(ATTR_VARIABLE, rr.getVariable());

  if ( isEmpty(static_cast<Rule>(rr)) )
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
    notesAndAnnotation(rr);

    //
    // math: Math  (L2v1)
    //
    doMath(rr);

    downIndent();
    endElement(ELEM_RATE_RULE);
  }

  return *this;
}


/**
 * AlgebraicRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const AlgebraicRule& ar)
{
  openStartElement(ELEM_ALGEBRAIC_RULE);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(ar);

  if (fLevel == 1)
  {
    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_FORMULA, ar.getFormula());
  }

  if ( isEmpty(ar) )
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
    notesAndAnnotation(ar);

    //
    // math: Math  (L2v1)
    //
    doMath(ar);

    downIndent();
    endElement(ELEM_ALGEBRAIC_RULE);
  }

  return *this;
}


/**
 * SpeciesConcentrationRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SpeciesConcentrationRule& scr)
{
  //
  // Level 2
  //
  // A SpeciesConcentrationRule is either an AssignmentRule or a RateRule.
  // operator<< (const AssignmentRule*) formats either case.
  //
  if (fLevel > 1)
  {
    *this << static_cast<const AssignmentRule&>(scr);
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
    attribute(ATTR_FORMULA, scr.getFormula());

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    doRuleType(scr.getType());

    //
    // specie : SName   { use="required" }  (L1v1)
    // species: SName   { use="required" }  (L1v2)
    //
    attribute(attr, scr.getSpecies());

    if ( isEmpty(scr) )
    {
      slashCloseStartElement();
    }
    else
    {
      endElement(elem, scr);
    }
  }

  return *this;
}


/**
 * CompartmentVolumeRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const CompartmentVolumeRule& cvr)
{
  //
  // Level 2
  //
  // A CompartmentVolumeRule is either an AssignmentRule or a RateRule.
  // operator<< (const AssignmentRule*) formats either case.
  //
  if (fLevel > 1)
  {
    *this << static_cast<const AssignmentRule&>(cvr);
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
    attribute(ATTR_FORMULA, cvr.getFormula());

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    doRuleType(cvr.getType());

    //
    // compartment: SName  { use="required" }  (L1v1, L1v2)
    //
    attribute(ATTR_COMPARTMENT, cvr.getCompartment());

    if ( isEmpty(cvr) )
    {
      slashCloseStartElement();
    }
    else
    {
      endElement(ELEM_COMPARTMENT_VOLUME_RULE, cvr);
    }
  }

  return *this;
}


/**
 * ParameterRule
 */
SBMLFormatter&
SBMLFormatter::operator<< (const ParameterRule& pr)
{
  //
  // Level 2
  //
  // A ParameterRule is either an AssignmentRule or a RateRule.
  // operator<< (const AssignmentRule*) formats either case.
  //
  if (fLevel > 1)
  {
    *this << static_cast<const AssignmentRule&>(pr);
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
    attribute(ATTR_FORMULA, pr.getFormula());

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    doRuleType(pr.getType());

    //
    // name: SName  { use="required" } (L1v1, L1v2)
    //
    attribute(ATTR_NAME, pr.getName());

    //
    // units  { use="optional" }  (L1v1, L1v2);
    //
    if ( pr.isSetUnits() )
    {
      attribute(ATTR_UNITS, pr.getUnits());
    }

    if ( isEmpty(pr) )
    {
      slashCloseStartElement();
    }
    else
    {
      endElement(ELEM_PARAMETER_RULE, pr);
    }
  }

  return *this;
}


/**
 * Reaction
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Reaction& r)
{
  openStartElement(ELEM_REACTION);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(r);

  //
  // id: SId  { use="required" }  (L2v1)
  //
  if (fLevel > 1)
  {
    attribute(ATTR_ID, r.getId());
  }

  //
  // name  { use="required" }  (L1v1, L1v2)
  // name  { use="optional" }  (L2v1)
  //
  // For the sake of robustness, if outputting L1 and name is not set,
  // substitute the value of id.
  //
  if ( r.isSetName() )
  {
    attribute(ATTR_NAME, r.getName());
  }
  else if (fLevel == 1)
  {
    attribute(ATTR_NAME, r.getId());
  }

  //
  // reversible: boolean
  // { use="optional"  default="true" (1) }  (L1v1, L1v2, L2v1)
  //
  if (r.getReversible() != true)
  {
    attribute(ATTR_REVERSIBLE, r.getReversible());
  }

  //
  // fast: boolean  { use="optional" default="false" (0) }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }  (L2v1)
  //
  if (fLevel == 1)
  {
    if (r.getFast() != false)
    {
      attribute(ATTR_FAST, r.getFast());
    }
  }
  else
  {
    if ( r.isSetFast() )
    {
      attribute(ATTR_FAST, r.getFast());
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

    notesAndAnnotation(r);

    listOfReactants( r.reactant );
    listOfProducts ( r.product  );
    listOfModifiers( r.modifier );

    if ( r.isSetKineticLaw() )
    {
      *this << *(r.getKineticLaw());
    }

    downIndent();
    endElement(ELEM_REACTION);
  }

  return *this;
}


/**
 * SimpleSpeciesReference
 */
SBMLFormatter&
SBMLFormatter::operator<< (const SimpleSpeciesReference& ssr)
{
  switch (ssr.getTypeCode())
  {
    case SBML_SPECIES_REFERENCE:
      *this << static_cast<const SpeciesReference&>(ssr);
      break;

    case SBML_MODIFIER_SPECIES_REFERENCE:
      *this << static_cast<const ModifierSpeciesReference&>(ssr);
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
SBMLFormatter::operator<< (const SpeciesReference& sr)
{
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
  doMetaId(sr);

  //
  // species: SName  { use="required" }  (L1v1, L1v2)
  // species: SId    { use="required" }  (L2v1)
  //
  attribute(attr, sr.getSpecies());

  //
  // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
  // stoichiometry: double   { use="optional" default="1" }  (L2v1)
  //
  if (sr.getStoichiometry() != 1)
  {
    if (fLevel == 1)
    {
      attribute(ATTR_STOICHIOMETRY, (int) sr.getStoichiometry());
    }
    else if ( !sr.isSetStoichiometryMath() && sr.getDenominator() == 1 )
    {
      attribute(ATTR_STOICHIOMETRY, sr.getStoichiometry());
    }
  }

  //
  // denominator  { use="optional" default="1" }  (L1v1, L1v2)
  //
  if (fLevel == 1)
  {
    if (sr.getDenominator() != 1)
    {
      attribute(ATTR_DENOMINATOR, sr.getDenominator());
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
    notesAndAnnotation(sr);

    //
    // stoichiometryMath: StoichiometryMath  { use="optional" } (L2v1)
    //
    doMath(sr);

    downIndent();
    endElement(elem);
  }

  return *this;
}


/**
 * ModifierSpeciesReference
 */
SBMLFormatter&
SBMLFormatter::operator<< (const ModifierSpeciesReference& msr)
{
  openStartElement(ELEM_MODIFIER_SPECIES_REFERENCE);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(msr);

  //
  // species: SId  { use="required" }  (L2v1)
  //
  attribute(ELEM_SPECIES, msr.getSpecies());

  if ( isEmpty(msr) )
  {
    slashCloseStartElement();
  }
  else
  {
    endElement(ELEM_MODIFIER_SPECIES_REFERENCE, msr);
  }

  return *this;
}


/**
 * KineticLaw
 */
SBMLFormatter&
SBMLFormatter::operator<< (const KineticLaw& kl)
{
  openStartElement(ELEM_KINETIC_LAW);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(kl);

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (fLevel == 1)
  {
    attribute(ATTR_FORMULA, kl.getFormula());
  }

  //
  // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if ( kl.isSetTimeUnits() )
  {
    attribute(ATTR_TIME_UNITS, kl.getTimeUnits());
  }

  //
  // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1)
  //
  if ( kl.isSetSubstanceUnits() )
  {
    attribute(ATTR_SUBSTANCE_UNITS, kl.getSubstanceUnits());
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
    notesAndAnnotation(kl);

    //
    // math: Math  (L2v1)
    //
    doMath(kl);

    listOfParameters( kl.parameter );

    downIndent();
    endElement(ELEM_KINETIC_LAW);
  } 

  return *this;
}


/**
 * Event
 */
SBMLFormatter&
SBMLFormatter::operator<< (const Event& e)
{
  openStartElement(ELEM_EVENT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(e);

  //
  // id: SId  { use="optional" }  (L2v1)
  //
  if ( e.isSetId() )
  {
    attribute(ATTR_ID, e.getId());
  }

  //
  // name: string  { use="optional" }  (L2v1)
  //
  if ( e.isSetName() )
  {
    attribute(ATTR_NAME, e.getName());
  }

  //
  // timeUnits: SId  { use="optional" }  (L2v1)
  //
  if ( e.isSetTimeUnits() )
  {
    attribute(ATTR_TIME_UNITS, e.getTimeUnits());
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
    notesAndAnnotation(e);

    //
    // trigger: Math  (L2v1)
    // 
    if ( e.isSetTrigger() )
    {
      startElement(ELEM_TRIGGER);

      fMathFormatter->setIndentLevel(fIndentLevel + 1);
      fMathFormatter->startMath();

      *fMathFormatter << e.getTrigger();

      fMathFormatter->endMath();

      endElement(ELEM_TRIGGER);
    }

    //
    // delay: Math  (L2v1)
    //
    if ( e.isSetDelay() )
    {
      startElement(ELEM_DELAY);

      fMathFormatter->setIndentLevel(fIndentLevel + 1);
      fMathFormatter->startMath();

      *fMathFormatter << e.getDelay();

      fMathFormatter->endMath();

      endElement(ELEM_DELAY);
    }

    //
    // eventAssignment: EventAssignment[1..*]
    //
    listOfEventAssignments( e.eventAssignment );

    downIndent();
    endElement(ELEM_EVENT);
  }

  return *this;
}


/**
 * EventAssignment
 */
SBMLFormatter&
SBMLFormatter::operator<< (const EventAssignment& ea)
{
  openStartElement(ELEM_EVENT_ASSIGNMENT);

  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  doMetaId(ea);

  //
  // variable: SId  { use="required" }  (L2v1)
  //
  attribute(ATTR_VARIABLE, ea.getVariable());


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
    notesAndAnnotation(ea);

    //
    // math: Math  (L2v1)
    // 
    if ( ea.isSetMath() )
    {
      fMathFormatter->setIndentLevel(fIndentLevel);
      fMathFormatter->startMath();

      *fMathFormatter << ea.getMath();

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
#define makeFn(name, element, type)               \
void                                              \
SBMLFormatter::name (const ListOf& list)          \
{                                                 \
  unsigned int size = list.getNumItems();         \
  SBase* sb;                                      \
                                                  \
                                                  \
  if (size > 0)                                   \
  {                                               \
    openStartElement( element );                  \
    doMetaId( list );                             \
    closeStartElement();                          \
                                                  \
    upIndent();                                   \
                                                  \
    if (fLevel > 1) notesAndAnnotation(list);     \
                                                  \
    for (unsigned int n = 0; n < size; n++)       \
    {                                             \
      sb = list.get(n);                           \
      if (sb != NULL)                             \
      {                                           \
        *this << * static_cast<const type*>(sb);  \
      }                                           \
    }                                             \
                                                  \
    downIndent();                                 \
                                                  \
    endElement( element );                        \
  }                                               \
}

makeFn( listOfFunctionDefinitions, ELEM_LIST_OF_FUNCTION_DEFINITIONS,
        FunctionDefinition )

makeFn( listOfUnitDefinitions, ELEM_LIST_OF_UNIT_DEFINITIONS, UnitDefinition   )
makeFn( listOfUnits          , ELEM_LIST_OF_UNITS           , Unit             )
makeFn( listOfCompartments   , ELEM_LIST_OF_COMPARTMENTS    , Compartment      )
makeFn( listOfSpecies        , ELEM_LIST_OF_SPECIES         , Species          )
makeFn( listOfParameters     , ELEM_LIST_OF_PARAMETERS      , Parameter        )
makeFn( listOfRules          , ELEM_LIST_OF_RULES           , Rule             )
makeFn( listOfReactions      , ELEM_LIST_OF_REACTIONS       , Reaction         )
makeFn( listOfReactants      , ELEM_LIST_OF_REACTANTS       , SpeciesReference )
makeFn( listOfProducts       , ELEM_LIST_OF_PRODUCTS        , SpeciesReference )

makeFn( listOfModifiers, ELEM_LIST_OF_MODIFIERS, ModifierSpeciesReference );
makeFn( listOfEvents, ELEM_LIST_OF_EVENTS, Event )

makeFn( listOfEventAssignments, ELEM_LIST_OF_EVENT_ASSIGNMENTS,
        EventAssignment )

#undef makeFn


/**
 * Notes
 */
void
SBMLFormatter::notes (const std::string& s)
{
  if ( s.empty() ) return;


  startElement(ELEM_NOTES);

  upIndent();
  indent();

  XMLCh* x = XMLString::transcode( s.c_str() );
  *fFormatter << x << chLF;
  XMLString::release(&x);

  downIndent();

  endElement(ELEM_NOTES);
}


/**
 * Annotation
 */
void
SBMLFormatter::annotation (const std::string& s)
{
  if ( s.empty() ) return;


  indent();

  XMLCh* x = XMLString::transcode( s.c_str() );
  *fFormatter << x << chLF;
  delete [] x;
}


/**
 * Notes and Annotation
 */
void
SBMLFormatter::notesAndAnnotation (const SBase& sb)
{
  notes     ( sb.getNotes()      );
  annotation( sb.getAnnotation() );
}


/**
 * Outputs the <math> element for KineticLaw (L2 only).
 *
 * This method does the nescessary conversion if the KineticLaw has only
 * a formula string set.
 */
void
SBMLFormatter::doMath (const KineticLaw& kl)
{
  ASTNode_t* math;


  //
  // math: Math  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( kl.isSetMath() || kl.isSetFormula() )
    {
      fMathFormatter->setIndentLevel(fIndentLevel);
      fMathFormatter->startMath();

      if ( kl.isSetMath() )
      {
        *fMathFormatter << kl.getMath();
      }
      else if ( kl.isSetFormula() )
      {
        math = SBML_parseFormula( kl.getFormula().c_str() );
        *fMathFormatter << math;
        ASTNode_free(math);
      }

      fMathFormatter->endMath();
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
SBMLFormatter::doMath (const Rule& r)
{
  ASTNode_t* math;


  //
  // math: Math  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( r.isSetMath() || r.isSetFormula() )
    {
      fMathFormatter->setIndentLevel(fIndentLevel);
      fMathFormatter->startMath();

      if ( r.isSetMath() )
      {
        *fMathFormatter << r.getMath();
      }
      else if ( r.isSetFormula() )
      {
        math = SBML_parseFormula( r.getFormula().c_str() );
        *fMathFormatter << math;
        ASTNode_free(math);
      }

      fMathFormatter->endMath();
    }
  }
}


/**
 * Outputs the <stoichiometryMath> element for SpeciesReference (L2 only).
 */
void
SBMLFormatter::doMath (const SpeciesReference& sr)
{
  ASTNode_t* node;
  long int denominator;
  long int numerator;


  //
  // stoichiometryMath: StoichiometryMath  { use="optional" } (L2v1)
  //
  // Either output the stoichiometryMath field directly or output
  // <cn type='rational'> stoichiometry <sep/> denominator </cn>
  //
  if (fLevel > 1)
  {
    if (sr.isSetStoichiometryMath() || sr.getDenominator() != 1)
    {
      startElement(ELEM_STOICHIOMETRY_MATH);
      
      fMathFormatter->setIndentLevel(fIndentLevel + 1);
      fMathFormatter->startMath();

      if ( sr.isSetStoichiometryMath() )
      {
        *fMathFormatter << sr.getStoichiometryMath();
      }
      else
      {
        numerator   = (long int) sr.getStoichiometry();
        denominator = (long int) sr.getDenominator();

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


/**
 * Outputs the metaid attribute for the given SBML object (L2 only).
 */
void
SBMLFormatter::doMetaId (const SBase& sb)
{
  //
  // metaid: ID  { use="optional" }  (L2v1)
  //
  if (fLevel > 1)
  {
    if ( sb.isSetMetaId() )
    {
      attribute(ATTR_META_ID, sb.getMetaId());
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




/* ----------------------------------------------------------------------
 *                               isEmpty()
 * ----------------------------------------------------------------------
 */


/**
 * @return true if the given Rule contains no child XML elements.
 */
bool
SBMLFormatter::isEmpty (const Rule& r)
{
  bool result = isEmpty((SBase&) r);


  if (fLevel > 1)
  {
    result = result && !(r.isSetFormula() || r.isSetMath());
  }

  return result;
}


/**
 * @return true if the given SpeciesReference contains no child XML
 * elements.
 */
bool
SBMLFormatter::isEmpty (const SpeciesReference& sr)
{
  bool result = isEmpty((SBase&) sr);


  if (fLevel > 1)
  {
    result = result &&
             !( sr.isSetStoichiometryMath() || sr.getDenominator() != 1 );
  }

  return result;
}


/**
 * @return true if the given KineticLaw contains no child XML elements.
 */
bool
SBMLFormatter::isEmpty (const KineticLaw& kl)
{
  bool result = isEmpty((SBase&) kl) && ! kl.getNumParameters();


  if (fLevel > 1)
  {
    result = result && !( kl.isSetMath() || kl.isSetFormula() );
  }

  return result;
}


//
// In this context "empty" means either no notes, annotations and other
// SBML (XML) subelements.
//

bool
SBMLFormatter::isEmpty (const SBase& sb)
{
  return !sb.isSetNotes() && !sb.isSetAnnotation();
}


bool
SBMLFormatter::isEmpty (const Model& m)
{
  return isEmpty((SBase&) m)                &&
         ( !m.getNumFunctionDefinitions() ) &&
         ( !m.getNumUnitDefinitions    () ) &&
         ( !m.getNumCompartments       () ) &&
         ( !m.getNumSpecies            () ) &&
         ( !m.getNumParameters         () ) &&
         ( !m.getNumRules              () ) &&
         ( !m.getNumReactions          () ) &&
         ( !m.getNumEvents             () );
}


bool
SBMLFormatter::isEmpty (const FunctionDefinition& fd)
{
  return isEmpty((SBase&) fd) && !fd.isSetMath();
}


bool
SBMLFormatter::isEmpty (const UnitDefinition& ud)
{
  return isEmpty((SBase&) ud) && !ud.getNumUnits();
}


bool
SBMLFormatter::isEmpty (const Reaction& r)
{
  return
    isEmpty((SBase&) r)      &&
    ( !r.getNumReactants() ) &&
    ( !r.getNumProducts () ) &&
    ( !r.getNumModifiers() ) &&
    ( !r.isSetKineticLaw() );
}


bool
SBMLFormatter::isEmpty (const Event& e)
{
  return
    isEmpty((SBase&) e)             &&
    ( !e.isSetTrigger          () ) &&
    ( !e.isSetDelay            () ) &&
    ( !e.getNumEventAssignments() );
}


bool
SBMLFormatter::isEmpty (const EventAssignment& ea)
{
  return isEmpty((SBase&) ea) && !ea.isSetMath();
}


//
// The rest of the isEmpty() functions have the same basic form.  In fact,
// they exist simply to avoid an explicit type cast.
//
#define makeFn(type)                   \
bool                                   \
SBMLFormatter::isEmpty (const type& t) \
{                                      \
  return isEmpty((SBase&) t);          \
}

makeFn( Unit                     )
makeFn( Compartment              )
makeFn( Species                  )
makeFn( Parameter                )
makeFn( ModifierSpeciesReference )

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
SBMLFormatter::endElement(const XMLCh* name, const SBase& sb)
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
  attribute(name, (value == true) ? VAL_TRUE : VAL_FALSE );
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
  if (value != value)
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
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a C++
 * string).
 */
void
SBMLFormatter::attribute (const XMLCh* name, const std::string& value)
{
  attribute(name, value.c_str());
}


#ifndef USE_EXPAT
/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a C
 * string).
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
    s = XMLString::transcode( value );
    attribute(name, s);

    delete [] s;
  }
}
#endif // !USE_EXPAT


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
