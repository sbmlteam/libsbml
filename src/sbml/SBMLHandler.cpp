/**
 * \file    SBMLHandler.cpp
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


#include <iostream>

#include "common/common.h"
#include "util/List.h"

#ifdef USE_EXPAT
#  include <expat.h>
#  include "xml/ExpatXMLString.h"
#else
#  include <xercesc/sax2/Attributes.hpp>
#  include <xercesc/util/XMLString.hpp>
#  include <xercesc/framework/LocalFileInputSource.hpp>
#  include <xercesc/framework/MemBufInputSource.hpp>
#  include <xercesc/util/TransService.hpp>
   using namespace xercesc;
#  include "SBMLSchemaInputSource.h"
#endif  // USE_EXPAT

#ifdef USE_LAYOUT
#  include "layout/LayoutHandler.h"
#endif  // USE_LAYOUT

#include "math/FormulaFormatter.h"
#include "math/MathMLHandler.h"

#include "xml/ParseMessage.h"
#include "xml/XMLStringFormatter.h"
#include "xml/XMLUtil.h"

#include "SBMLTypes.h"
#include "SBMLUnicodeConstants.h"
#include "SBMLHandler.h"


using namespace std;


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


static const char XML_NAMESPACE_URI_SEP = ' ';

/* 
 * const char * [] of the sbml schema
 */

// sbml level 1 version 1

const char * xsd[280] = {
"<?xml version='1.0' encoding='UTF-8'?>",
"<xsd:schema xmlns:xsd = 'http://www.w3.org/2001/XMLSchema' targetNamespace = 'http://www.sbml.org/sbml/level1' xmlns:naa = 'http://www.sbml.org/sbml/level1' elementFormDefault = 'qualified'>",
"<xsd:simpleType name='SName'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:pattern value='(_|[a-z]|[A-Z])(_|[a-z]|[A-Z]|[0-9])*'/>",
"</xsd:restriction>",
"</xsd:simpleType><xsd:complexType name='SBase' abstract='true'>",
"<xsd:sequence>",
"<xsd:element name='notes' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:any namespace='http://www.w3.org/1999/xhtml' processContents='skip' minOccurs='0' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='annotations' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:any processContents='skip' minOccurs='0' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"</xsd:complexType>",
"<xsd:complexType name='Parameter'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:attribute name='name' use='required'/>",
"<xsd:attribute name='value' type='xsd:double' use='required'/>",
"<xsd:attribute name='units' type='naa:SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:simpleType name='UnitKind'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:enumeration value='ampere'/>",
"<xsd:enumeration value='becquerel'/>",
"<xsd:enumeration value='candela'/>",
"<xsd:enumeration value='celsius'/>",
"<xsd:enumeration value='coulomb'/>",
"<xsd:enumeration value='dimensionless'/>",
"<xsd:enumeration value='farad'/>",
"<xsd:enumeration value='gram'/>",
"<xsd:enumeration value='gray'/>",
"<xsd:enumeration value='henry'/>",
"<xsd:enumeration value='hertz'/>",
"<xsd:enumeration value='item'/>",
"<xsd:enumeration value='joule'/>",
"<xsd:enumeration value='katal'/>",
"<xsd:enumeration value='kelvin'/>",
"<xsd:enumeration value='kilogram'/>",
"<xsd:enumeration value='liter'/>",
"<xsd:enumeration value='litre'/>",
"<xsd:enumeration value='lumen'/>",
"<xsd:enumeration value='lux'/>",
"<xsd:enumeration value='meter'/>",
"<xsd:enumeration value='metre'/>",
"<xsd:enumeration value='mole'/>",
"<xsd:enumeration value='newton'/>",
"<xsd:enumeration value='ohm'/>",
"<xsd:enumeration value='pascal'/>",
"<xsd:enumeration value='radian'/>",
"<xsd:enumeration value='second'/>",
"<xsd:enumeration value='siemens'/>",
"<xsd:enumeration value='sievert'/>",
"<xsd:enumeration value='steradian'/>",
"<xsd:enumeration value='tesla'/>",
"<xsd:enumeration value='volt'/>",
"<xsd:enumeration value='watt'/>",
"<xsd:enumeration value='weber'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='Unit'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:attribute name='kind' type='naa:UnitKind' use='required'/>",
"<xsd:attribute name='exponent' type='xsd:integer' default='1'/>",
"<xsd:attribute name='scale' type='xsd:integer' default='1'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='UnitDefinition'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfUnits' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='unit' type='naa:Unit' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='name' type='naa:SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Compartment'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:attribute name='name' type='naa:SName' use='required'/>",
"<xsd:attribute name='volume' type='xsd:double' default='1'/>",
"<xsd:attribute name='units' type='naa:SName' use='optional'/>",
"<xsd:attribute name='outside' type='naa:SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Specie'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:attribute name='name' type='naa:SName' use='required'/>",
"<xsd:attribute name='compartment' type='naa:SName' use='required'/>",
"<xsd:attribute name='initialAmount' type='xsd:double' use='required'/>",
"<xsd:attribute name='units' type='naa:SName' use='optional'/>",
"<xsd:attribute name='boundaryCondition' use='optional' type='xsd:boolean' default='false'/>",
"<xsd:attribute name='charge' type='xsd:integer' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:element name='listOfParameters'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='parameter' type='naa:Parameter' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:simpleType name='RuleType'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:enumeration value='scalar'/>",
"<xsd:enumeration value='rate'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='Rule' abstract='true'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:attribute name='formula' type='xsd:string' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='AlgebraicRule'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:Rule'/>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='AssignmentRule' abstract='true'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:Rule'>",
"<xsd:attribute name='type' type='naa:RuleType' default='scalar'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='CompartmentVolumeRule'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:AssignmentRule'>",
"<xsd:attribute name='compartment' type='naa:SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='SpecieConcentrationRule'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:AssignmentRule'>",
"<xsd:attribute name='specie' type='naa:SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ParameterRule'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:AssignmentRule'>",
"<xsd:attribute name='name' type='naa:SName' use='required'/>",
"<xsd:attribute name='units' type='naa:SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:element name='specieReference'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:attribute name='specie' type='xsd:string' use='required'/>",
"<xsd:attribute name='stoichiometry' use='optional' type='xsd:integer' default='1'/>",
"<xsd:attribute name='denominator' use='optional' type='xsd:integer' default='1'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:complexType name='KineticLaw'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:sequence>",
"<xsd:element ref='naa:listOfParameters' minOccurs='0'/>",
"</xsd:sequence>",
"<xsd:attribute name='formula' type='xsd:string' use='required'/>",
"<xsd:attribute name='timeUnits' type='naa:SName' use='optional'/>",
"<xsd:attribute name='substanceUnits' type='naa:SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Reaction'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfReactants'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element ref='naa:specieReference' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfProducts'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element ref='naa:specieReference' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='kineticLaw' type='naa:KineticLaw' minOccurs='0'/>",
"</xsd:sequence>",
"<xsd:attribute name='name' type='naa:SName' use='required'/>",
"<xsd:attribute name='reversible' use='optional' type='xsd:boolean' default='true'/>",
"<xsd:attribute name='fast' use='optional' type='xsd:boolean' default='false'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Model'>",
"<xsd:complexContent>",
"<xsd:extension base='naa:SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfUnitDefinitions' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='unitDefinition' type='naa:UnitDefinition' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfCompartments'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='compartment' type='naa:Compartment' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfSpecies'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='specie' type='naa:Specie' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element ref='naa:listOfParameters' minOccurs='0'/>",
"<xsd:element name='listOfRules' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:choice maxOccurs='unbounded'>",
"<xsd:element name='algebraicRule' type='naa:AlgebraicRule' minOccurs='0'/>",
"<xsd:element name='compartmentVolumeRule' type='naa:CompartmentVolumeRule' minOccurs='0'/>",
"<xsd:element name='specieConcentrationRule' type='naa:SpecieConcentrationRule' minOccurs='0'/>",
"<xsd:element name='parameterRule' type='naa:ParameterRule' minOccurs='0'/>",
"</xsd:choice>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfReactions'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='reaction' type='naa:Reaction' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='name' type='naa:SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='sbmlDocument'>",
"<xsd:sequence>",
"<xsd:element name='model' type='naa:Model'/>",
"</xsd:sequence>",
"<xsd:attribute name='level' type='xsd:positiveInteger' use='required'/>",
"<xsd:attribute name='version' type='xsd:positiveInteger' use='required'/>",
"</xsd:complexType>",
"<xsd:element name='sbml' type='naa:sbmlDocument'/>",
"</xsd:schema>",
"\0"
};

// sbml level 1 version 2

const char * xsd_l1v2[283] = { 
"<?xml version='1.0' encoding='UTF-8'?>",
"<xsd:schema targetNamespace='http://www.sbml.org/sbml/level1' xmlns:xsd='http://www.w3.org/2001/XMLSchema' xmlns='http://www.sbml.org/sbml/level1' elementFormDefault='qualified' version='$Version$'>",
"<xsd:simpleType name='SName'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:pattern value='(_|[a-z]|[A-Z])(_|[a-z]|[A-Z]|[0-9])*'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='SBase' abstract='true'>",
"<xsd:sequence>",
"<xsd:element name='notes' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:any namespace='http://www.w3.org/1999/xhtml' processContents='skip' minOccurs='0' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='annotation' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:any processContents='skip' minOccurs='0' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"</xsd:complexType>",
"<xsd:simpleType name='UnitKind'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:enumeration value='ampere'/>",
"<xsd:enumeration value='becquerel'/>",
"<xsd:enumeration value='candela'/>",
"<xsd:enumeration value='celsius'/>",
"<xsd:enumeration value='coulomb'/>",
"<xsd:enumeration value='dimensionless'/>",
"<xsd:enumeration value='farad'/>",
"<xsd:enumeration value='gram'/>",
"<xsd:enumeration value='gray'/>",
"<xsd:enumeration value='henry'/>",
"<xsd:enumeration value='hertz'/>",
"<xsd:enumeration value='item'/>",
"<xsd:enumeration value='joule'/>",
"<xsd:enumeration value='katal'/>",
"<xsd:enumeration value='kelvin'/>",
"<xsd:enumeration value='kilogram'/>",
"<xsd:enumeration value='liter'/>",
"<xsd:enumeration value='litre'/>",
"<xsd:enumeration value='lumen'/>",
"<xsd:enumeration value='lux'/>",
"<xsd:enumeration value='meter'/>",
"<xsd:enumeration value='metre'/>",
"<xsd:enumeration value='mole'/>",
"<xsd:enumeration value='newton'/>",
"<xsd:enumeration value='ohm'/>",
"<xsd:enumeration value='pascal'/>",
"<xsd:enumeration value='radian'/>",
"<xsd:enumeration value='second'/>",
"<xsd:enumeration value='siemens'/>",
"<xsd:enumeration value='sievert'/>",
"<xsd:enumeration value='steradian'/>",
"<xsd:enumeration value='tesla'/>",
"<xsd:enumeration value='volt'/>",
"<xsd:enumeration value='watt'/>",
"<xsd:enumeration value='weber'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='Unit'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='kind' type='UnitKind' use='required'/>",
"<xsd:attribute name='exponent' type='xsd:integer' default='1'/>",
"<xsd:attribute name='scale' type='xsd:integer' default='0'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='UnitDefinition'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfUnits' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='unit' type='Unit' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='name' type='SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Compartment'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='name' type='SName' use='required'/>",
"<xsd:attribute name='volume' type='xsd:double' default='1'/>",
"<xsd:attribute name='units' type='SName' use='optional'/>",
"<xsd:attribute name='outside' type='SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Species'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='name' type='SName' use='required'/>",
"<xsd:attribute name='compartment' type='SName' use='required'/>",
"<xsd:attribute name='initialAmount' type='xsd:double' use='required'/>",
"<xsd:attribute name='units' type='SName' use='optional'/>",
"<xsd:attribute name='boundaryCondition' type='xsd:boolean' use='optional' default='false'/>",
"<xsd:attribute name='charge' type='xsd:integer' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Parameter'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='name' use='required'/>",
"<xsd:attribute name='value' type='xsd:double' use='optional'/>",
"<xsd:attribute name='units' type='SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:simpleType name='RuleType'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:enumeration value='scalar'/>",
"<xsd:enumeration value='rate'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='Rule' abstract='true'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='formula' type='xsd:string' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='AlgebraicRule'>",
"<xsd:complexContent>",
"<xsd:extension base='Rule'/>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='AssignmentRule' abstract='true'>",
"<xsd:complexContent>",
"<xsd:extension base='Rule'>",
"<xsd:attribute name='type' type='RuleType' default='scalar'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='CompartmentVolumeRule'>",
"<xsd:complexContent>",
"<xsd:extension base='AssignmentRule'>",
"<xsd:attribute name='compartment' type='SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='SpeciesConcentrationRule'>",
"<xsd:complexContent>",
"<xsd:extension base='AssignmentRule'>",
"<xsd:attribute name='species' type='SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ParameterRule'>",
"<xsd:complexContent>",
"<xsd:extension base='AssignmentRule'>",
"<xsd:attribute name='name' type='SName' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='KineticLaw'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfParameters' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='parameter' type='Parameter' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='formula' type='xsd:string' use='required'/>",
"<xsd:attribute name='timeUnits' type='SName' use='optional'/>",
"<xsd:attribute name='substanceUnits' type='SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='SpeciesReference'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='species' type='xsd:string' use='required'/>",
"<xsd:attribute name='stoichiometry' type='xsd:positiveInteger' use='optional' default='1'/>",
"<xsd:attribute name='denominator' type='xsd:positiveInteger' use='optional' default='1'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Reaction'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfReactants' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='speciesReference' type='SpeciesReference' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfProducts' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='speciesReference' type='SpeciesReference' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='kineticLaw' type='KineticLaw' minOccurs='0'/>",
"</xsd:sequence>",
"<xsd:attribute name='name' type='SName' use='required'/>",
"<xsd:attribute name='reversible' type='xsd:boolean' use='optional' default='true'/>",
"<xsd:attribute name='fast' type='xsd:boolean' use='optional' default='false'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Model'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfUnitDefinitions' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='unitDefinition' type='UnitDefinition' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfCompartments' minOccurs='1'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='compartment' type='Compartment' maxOccurs='unbounded' minOccurs='1'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfSpecies' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='species' type='Species' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfParameters' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='parameter' type='Parameter' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfRules' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:choice maxOccurs='unbounded'>",
"<xsd:element name='algebraicRule' type='AlgebraicRule' minOccurs='0'/>",
"<xsd:element name='compartmentVolumeRule' type='CompartmentVolumeRule' minOccurs='0'/>",
"<xsd:element name='speciesConcentrationRule' type='SpeciesConcentrationRule' minOccurs='0'/>",
"<xsd:element name='parameterRule' type='ParameterRule' minOccurs='0'/>",
"</xsd:choice>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfReactions' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:element name='reaction' type='Reaction' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='name' type='SName' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='sbmlDocument'>",
"<xsd:sequence>",
"<xsd:element name='model' type='Model'/>",
"</xsd:sequence>",
"<xsd:attribute name='level' type='xsd:positiveInteger' use='required' fixed='1'/>",
"<xsd:attribute name='version' type='xsd:positiveInteger' use='required'/>",
"</xsd:complexType>",
"<xsd:element name='sbml' type='sbmlDocument'/>",
"</xsd:schema>",
"\0"
};

// sbml level 2 version 1

const char * xsd_l2v1[424] = { 
"<?xml version='1.0' encoding='UTF-8'?>",
"<xsd:schema targetNamespace='http://www.sbml.org/sbml/level2' xmlns:xsd='http://www.w3.org/2001/XMLSchema' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:mml='http://www.w3.org/1998/Math/MathML' xmlns='http://www.sbml.org/sbml/level2' elementFormDefault='qualified' attributeFormDefault='unqualified' version='1'>",
"<xsd:import namespace='http://www.w3.org/1998/Math/MathML' schemaLocation='math'/>",
"<xsd:simpleType name='SId'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:pattern value='(_|[a-z]|[A-Z])(_|[a-z]|[A-Z]|[0-9])*'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='SBase' abstract='true'>",
"<xsd:sequence>",
"<xsd:element name='notes' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:any namespace='http://www.w3.org/1999/xhtml' processContents='skip' minOccurs='0' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='annotation' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:sequence>",
"<xsd:any processContents='skip' minOccurs='0' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='metaid' type='xsd:ID' use='optional'/>",
"</xsd:complexType>",
"<xsd:complexType name='FunctionDefinition'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element ref='mml:math'/>",
"</xsd:sequence>",
"<xsd:attribute name='id' type='SId' use='required'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:simpleType name='UnitKind'>",
"<xsd:restriction base='xsd:string'>",
"<xsd:enumeration value='ampere'/>",
"<xsd:enumeration value='becquerel'/>",
"<xsd:enumeration value='candela'/>",
"<xsd:enumeration value='Celsius'/>",
"<xsd:enumeration value='coulomb'/>",
"<xsd:enumeration value='dimensionless'/>",
"<xsd:enumeration value='farad'/>",
"<xsd:enumeration value='gram'/>",
"<xsd:enumeration value='gray'/>",
"<xsd:enumeration value='henry'/>",
"<xsd:enumeration value='hertz'/>",
"<xsd:enumeration value='item'/>",
"<xsd:enumeration value='joule'/>",
"<xsd:enumeration value='katal'/>",
"<xsd:enumeration value='kelvin'/>",
"<xsd:enumeration value='kilogram'/>",
"<xsd:enumeration value='litre'/>",
"<xsd:enumeration value='lumen'/>",
"<xsd:enumeration value='lux'/>",
"<xsd:enumeration value='metre'/>",
"<xsd:enumeration value='mole'/>",
"<xsd:enumeration value='newton'/>",
"<xsd:enumeration value='ohm'/>",
"<xsd:enumeration value='pascal'/>",
"<xsd:enumeration value='radian'/>",
"<xsd:enumeration value='second'/>",
"<xsd:enumeration value='siemens'/>",
"<xsd:enumeration value='sievert'/>",
"<xsd:enumeration value='steradian'/>",
"<xsd:enumeration value='tesla'/>",
"<xsd:enumeration value='volt'/>",
"<xsd:enumeration value='watt'/>",
"<xsd:enumeration value='weber'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"<xsd:complexType name='Unit'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='kind' type='UnitKind' use='required'/>",
"<xsd:attribute name='exponent' type='xsd:integer' default='1'/>",
"<xsd:attribute name='scale' type='xsd:integer' default='0'/>",
"<xsd:attribute name='multiplier' type='xsd:double' default='1'/>",
"<xsd:attribute name='offset' type='xsd:double' default='0'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ListOfUnits'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='unit' type='Unit' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='UnitDefinition'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfUnits' type='ListOfUnits'/>",
"</xsd:sequence>",
"<xsd:attribute name='id' type='SId' use='required'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Compartment'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='id' type='SId' use='required'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"<xsd:attribute name='size' type='xsd:double' use='optional'/>",
"<xsd:attribute name='spatialDimensions' use='optional' default='3'>",
"<xsd:simpleType>",
"<xsd:restriction base='xsd:integer'>",
"<xsd:minInclusive value='0'/>",
"<xsd:maxInclusive value='3'/>",
"</xsd:restriction>",
"</xsd:simpleType>",
"</xsd:attribute>",
"<xsd:attribute name='units' type='SId' use='optional'/>",
"<xsd:attribute name='outside' type='SId' use='optional'/>",
"<xsd:attribute name='constant' type='xsd:boolean' use='optional' default='true'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Species'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='id' type='SId' use='required'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"<xsd:attribute name='compartment' type='SId'/>",
"<xsd:attribute name='initialAmount' type='xsd:double' use='optional'/>",
"<xsd:attribute name='initialConcentration' type='xsd:double' use='optional'/>",
"<xsd:attribute name='substanceUnits' type='SId' use='optional'/>",
"<xsd:attribute name='spatialSizeUnits' type='SId' use='optional'/>",
"<xsd:attribute name='hasOnlySubstanceUnits' type='xsd:boolean' use='optional' default='false'/>",
"<xsd:attribute name='boundaryCondition' type='xsd:boolean' use='optional' default='false'/>",
"<xsd:attribute name='charge' type='xsd:integer' use='optional'/>",
"<xsd:attribute name='constant' type='xsd:boolean' use='optional' default='false'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Parameter'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='id' type='SId' use='required'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"<xsd:attribute name='value' type='xsd:double' use='optional'/>",
"<xsd:attribute name='units' type='SId' use='optional'/>",
"<xsd:attribute name='constant' type='xsd:boolean' use='optional' default='true'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ListOfParameters'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='parameter' type='Parameter' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Rule' abstract='true'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element ref='mml:math'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='AlgebraicRule'>",
"<xsd:complexContent>",
"<xsd:extension base='Rule'/>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='AssignmentRule'>",
"<xsd:complexContent>",
"<xsd:extension base='Rule'>",
"<xsd:attribute name='variable' type='SId' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='RateRule'>",
"<xsd:complexContent>",
"<xsd:extension base='Rule'>",
"<xsd:attribute name='variable' type='SId' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='KineticLaw'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element ref='mml:math'/>",
"<xsd:element name='listOfParameters' type='ListOfParameters' minOccurs='0'/>",
"</xsd:sequence>",
"<xsd:attribute name='timeUnits' type='SId' use='optional'/>",
"<xsd:attribute name='substanceUnits' type='SId' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='SimpleSpeciesReference' abstract='true'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:attribute name='species' type='SId' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ModifierSpeciesReference'>",
"<xsd:complexContent>",
"<xsd:extension base='SimpleSpeciesReference'/>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ListOfModifierSpeciesReferences'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='modifierSpeciesReference' type='ModifierSpeciesReference' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='StoichiometryMath'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element ref='mml:math'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='SpeciesReference'>",
"<xsd:complexContent>",
"<xsd:extension base='SimpleSpeciesReference'>",
"<xsd:sequence>",
"<xsd:element name='stoichiometryMath' type='StoichiometryMath' minOccurs='0'/>",
"</xsd:sequence>",
"<xsd:attribute name='stoichiometry' type='xsd:double' use='optional' default='1'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ListOfSpeciesReferences'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='speciesReference' type='SpeciesReference' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Reaction'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfReactants' type='ListOfSpeciesReferences' minOccurs='0'/>",
"<xsd:element name='listOfProducts' type='ListOfSpeciesReferences' minOccurs='0'/>",
"<xsd:element name='listOfModifiers' type='ListOfModifierSpeciesReferences' minOccurs='0'/>",
"<xsd:element name='kineticLaw' type='KineticLaw' minOccurs='0'/>",
"</xsd:sequence>",
"<xsd:attribute name='id' type='SId' use='required'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"<xsd:attribute name='reversible' type='xsd:boolean' use='optional' default='true'/>",
"<xsd:attribute name='fast' type='xsd:boolean' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='EventAssignment'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element ref='mml:math'/>",
"</xsd:sequence>",
"<xsd:attribute name='variable' type='SId' use='required'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='ListOfEventAssignments'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='eventAssignment' type='EventAssignment' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='MathField'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element ref='mml:math'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Event'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='trigger' type='MathField'/>",
"<xsd:element name='delay' type='MathField' minOccurs='0'/>",
"<xsd:element name='listOfEventAssignments' type='ListOfEventAssignments'/>",
"</xsd:sequence>",
"<xsd:attribute name='id' type='SId' use='optional'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"<xsd:attribute name='timeUnits' type='SId' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Model'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='listOfFunctionDefinitions' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='functionDefinition' type='FunctionDefinition' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfUnitDefinitions' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='unitDefinition' type='UnitDefinition' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfCompartments' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='compartment' type='Compartment' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfSpecies' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='species' type='Species' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfParameters' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='parameter' type='Parameter' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfRules' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:choice maxOccurs='unbounded'>",
"<xsd:element name='algebraicRule' type='AlgebraicRule' minOccurs='0'/>",
"<xsd:element name='assignmentRule' type='AssignmentRule' minOccurs='0'/>",
"<xsd:element name='rateRule' type='RateRule' minOccurs='0'/>",
"</xsd:choice>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfReactions' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='reaction' type='Reaction' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"<xsd:element name='listOfEvents' minOccurs='0'>",
"<xsd:complexType>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='event' type='Event' maxOccurs='unbounded'/>",
"</xsd:sequence>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"</xsd:element>",
"</xsd:sequence>",
"<xsd:attribute name='id' type='SId' use='optional'/>",
"<xsd:attribute name='name' type='xsd:string' use='optional'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:complexType name='Sbml'>",
"<xsd:complexContent>",
"<xsd:extension base='SBase'>",
"<xsd:sequence>",
"<xsd:element name='model' type='Model'/>",
"</xsd:sequence>",
"<xsd:attribute name='level' type='xsd:positiveInteger' use='required' fixed='2'/>",
"<xsd:attribute name='version' type='xsd:positiveInteger' use='required' fixed='1'/>",
"</xsd:extension>",
"</xsd:complexContent>",
"</xsd:complexType>",
"<xsd:element name='sbml' type='Sbml'/>",
"</xsd:schema>",
"\0"
};

// sbml-mathml (restricted mathml subset specific to sbml)

const char * xsd_math[230] = { 
"<?xml version='1.0' encoding='UTF-8'?>",
"<xs:schema 	xmlns='http://www.w3.org/1998/Math/MathML' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xs='http://www.w3.org/2001/XMLSchema' targetNamespace='http://www.w3.org/1998/Math/MathML' elementFormDefault='qualified' attributeFormDefault='unqualified'>",
"<xs:attributeGroup name='MathAttributes'>",
"<xs:attribute name='class' type='xs:NMTOKENS' use='optional'/>",
"<xs:attribute name='style' type='xs:string' use='optional'/>",
"<xs:attribute name='id' type='xs:ID' use='optional'/>",
"</xs:attributeGroup>",
"<xs:complexType name='MathBase'>",
"<xs:attributeGroup ref='MathAttributes'/>",
"</xs:complexType>",
"<xs:attributeGroup name='CnAttributes'>",
"<xs:attribute name='type'>",
"<xs:simpleType>",
"<xs:restriction base='xs:NMTOKEN'>",
"<xs:enumeration value='e-notation'/>",
"<xs:enumeration value='integer'/>",
"<xs:enumeration value='rational'/>",
"<xs:enumeration value='real'/>",
"</xs:restriction>",
"</xs:simpleType>",
"</xs:attribute>",
"<xs:attributeGroup ref='MathAttributes'/>",
"</xs:attributeGroup>",
"<xs:complexType name='SepType'/>",
"<xs:complexType name='Cn' mixed='true'>",
"<xs:choice minOccurs='0'>",
"<xs:element name='sep' type='SepType'/>",
"</xs:choice>",
"<xs:attributeGroup ref='CnAttributes'/>",
"</xs:complexType>",
"<xs:complexType name='Ci'>",
"<xs:simpleContent>",
"<xs:extension base='xs:string'>",
"<xs:attributeGroup ref='MathAttributes'/>",
"</xs:extension>",
"</xs:simpleContent>",
"</xs:complexType>",
"<xs:simpleType name='CsymbolURI'>",
"<xs:restriction base='xs:string'>",
"<xs:enumeration value='http://www.sbml.org/sbml/symbols/time'/>",
"<xs:enumeration value='http://www.sbml.org/sbml/symbols/delay'/>",
"</xs:restriction>",
"</xs:simpleType>",
"<xs:complexType name='Csymbol'>",
"<xs:simpleContent>",
"<xs:extension base='xs:string'>",
"<xs:attribute name='encoding' use='required' fixed='text'/>",
"<xs:attribute name='definitionURL' type='CsymbolURI' use='required'/>",
"<xs:attributeGroup ref='MathAttributes'/>",
"</xs:extension>",
"</xs:simpleContent>",
"</xs:complexType>",
"<xs:complexType name='NodeContainer'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:group ref='Node'/>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:complexType name='Apply'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:sequence>",
"<xs:choice>",
"<xs:element name='ci' type='Ci'/>",
"<xs:element name='csymbol' type='Csymbol'/>",
"<xs:element name='eq' type='MathBase'/>",
"<xs:element name='neq' type='MathBase'/>",
"<xs:element name='gt' type='MathBase'/>",
"<xs:element name='lt' type='MathBase'/>",
"<xs:element name='geq' type='MathBase'/>",
"<xs:element name='leq' type='MathBase'/>",
"<xs:element name='plus' type='MathBase'/>",
"<xs:element name='minus' type='MathBase'/>",
"<xs:element name='times' type='MathBase'/>",
"<xs:element name='divide' type='MathBase'/>",
"<xs:element name='power' type='MathBase'/>",
"<xs:sequence>",
"<xs:element name='root' type='MathBase'/>",
"<xs:element name='degree' type='NodeContainer' minOccurs='0'/>",
"</xs:sequence>",
"<xs:element name='abs' type='MathBase'/>",
"<xs:element name='exp' type='MathBase'/>",
"<xs:element name='ln' type='MathBase'/>",
"<xs:sequence>",
"<xs:element name='log' type='MathBase'/>",
"<xs:element name='logbase' type='NodeContainer' minOccurs='0'/>",
"</xs:sequence>",
"<xs:element name='floor' type='MathBase'/>",
"<xs:element name='ceiling' type='MathBase'/>",
"<xs:element name='factorial' type='MathBase'/>",
"<xs:element name='and' type='MathBase'/>",
"<xs:element name='or' type='MathBase'/>",
"<xs:element name='xor' type='MathBase'/>",
"<xs:element name='not' type='MathBase'/>",
"<xs:element name='sin' type='MathBase'/>",
"<xs:element name='cos' type='MathBase'/>",
"<xs:element name='tan' type='MathBase'/>",
"<xs:element name='sec' type='MathBase'/>",
"<xs:element name='csc' type='MathBase'/>",
"<xs:element name='cot' type='MathBase'/>",
"<xs:element name='sinh' type='MathBase'/>",
"<xs:element name='cosh' type='MathBase'/>",
"<xs:element name='tanh' type='MathBase'/>",
"<xs:element name='sech' type='MathBase'/>",
"<xs:element name='csch' type='MathBase'/>",
"<xs:element name='coth' type='MathBase'/>",
"<xs:element name='arcsin' type='MathBase'/>",
"<xs:element name='arccos' type='MathBase'/>",
"<xs:element name='arctan' type='MathBase'/>",
"<xs:element name='arcsec' type='MathBase'/>",
"<xs:element name='arccsc' type='MathBase'/>",
"<xs:element name='arccot' type='MathBase'/>",
"<xs:element name='arcsinh' type='MathBase'/>",
"<xs:element name='arccosh' type='MathBase'/>",
"<xs:element name='arctanh' type='MathBase'/>",
"<xs:element name='arcsech' type='MathBase'/>",
"<xs:element name='arccsch' type='MathBase'/>",
"<xs:element name='arccoth' type='MathBase'/>",
"</xs:choice>",
"<xs:group ref='Node' maxOccurs='unbounded'/>",
"</xs:sequence>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:complexType name='Piece'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:group ref='Node' minOccurs='2' maxOccurs='2'/>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:complexType name='Otherwise'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:group ref='Node'/>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:complexType name='Piecewise'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:sequence>",
"<xs:element name='piece' type='Piece' minOccurs='0' maxOccurs='unbounded'/>",
"<xs:element name='otherwise' type='Otherwise' minOccurs='0' maxOccurs='1'/>",
"</xs:sequence>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:attributeGroup name='AnnotationAttributes'>",
"<xs:attributeGroup ref='MathAttributes'/>",
"<xs:attribute name='encoding' type='xs:string' use='required'/>",
"</xs:attributeGroup>",
"<xs:complexType name='Annotation'>",
"<xs:simpleContent>",
"<xs:extension base='xs:string'>",
"<xs:attributeGroup ref='AnnotationAttributes'/>",
"</xs:extension>",
"</xs:simpleContent>",
"</xs:complexType>",
"<xs:complexType name='Annotation-xml'>",
"<xs:sequence maxOccurs='unbounded'>",
"<xs:any processContents='skip'/>",
"</xs:sequence>",
"<xs:attributeGroup ref='AnnotationAttributes'/>",
"</xs:complexType>",
"<xs:complexType name='Semantics'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:sequence>",
"<xs:group ref='Node'/>",
"<xs:sequence maxOccurs='unbounded'>",
"<xs:choice>",
"<xs:element name='annotation' type='Annotation'/>",
"<xs:element name='annotation-xml' type='Annotation-xml'/>",
"</xs:choice>",
"</xs:sequence>",
"</xs:sequence>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:group name='Node'>",
"<xs:choice>",
"<xs:element name='apply' type='Apply'/>",
"<xs:element name='cn' type='Cn'/>",
"<xs:element name='ci' type='Ci'/>",
"<xs:element name='csymbol' type='Csymbol'/>",
"<xs:element name='true' type='MathBase'/>",
"<xs:element name='false' type='MathBase'/>",
"<xs:element name='notanumber' type='MathBase'/>",
"<xs:element name='pi' type='MathBase'/>",
"<xs:element name='infinity' type='MathBase'/>",
"<xs:element name='exponentiale' type='MathBase'/>",
"<xs:element name='semantics' type='Semantics'/>",
"<xs:element name='piecewise' type='Piecewise'/>",
"</xs:choice>",
"</xs:group>",
"<xs:complexType name='Bvar'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:sequence>",
"<xs:element name='ci' type='Ci'/>",
"</xs:sequence>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:complexType name='Lambda'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:sequence>",
"<xs:element name='bvar' type='Bvar' minOccurs='0' maxOccurs='unbounded'/>",
"<xs:group ref='Node'/>",
"</xs:sequence>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:complexType name='Math'>",
"<xs:complexContent>",
"<xs:extension base='MathBase'>",
"<xs:choice>",
"<xs:group ref='Node'/>",
"<xs:element name='lambda' type='Lambda'/>",
"</xs:choice>",
"</xs:extension>",
"</xs:complexContent>",
"</xs:complexType>",
"<xs:element name='math' type='Math'>",
"</xs:element>",
"</xs:schema>",
"\0"
};

// ---------------------------------------------------------------------------
//  Local constant data
//
//      This is the filename that we are looking for in the entity handler, to
//      redirect to the in-memory schema.
//
//  gFilel1v1
//  gFilel1v2
//  gFilel2v1
//  gFilemath
//
// ---------------------------------------------------------------------------
static const XMLCh  gFilel1v1[] =
{
  chLatin_l, chDigit_1, chLatin_v, chDigit_1, chNull
};

static const XMLCh  gFilel1v2[] =
{
  chLatin_l, chDigit_1, chLatin_v, chDigit_2, chNull
};

static const XMLCh  gFilel2v1[] =
{
  chLatin_l, chDigit_2, chLatin_v, chDigit_1, chNull
};

static const XMLCh  gFilemath[] =
{
  chLatin_m, chLatin_a, chLatin_t, chLatin_h, chNull
};

/////////////////////////////////////////////////////////////
/**
 * Ctor
 */
SBMLHandler::SBMLHandler (SBMLDocument* d) : fDocument(d)
{
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

#ifdef USE_LAYOUT
  fLayoutHandler = 0;
  inLayout       = 0;
#endif  // USE_LAYOUT
}


/**
 * Dtor
 */
SBMLHandler::~SBMLHandler ()
{
  Stack_free( fObjStack );
  Stack_free( fTagStack );

  delete fMathHandler;
  delete fMathDocument;
  delete fFormatter;

#ifdef USE_LAYOUT
  delete fLayoutHandler;
#endif  // USE_LAYOUT
}




/* ----------------------------------------------------------------------
 *                         SAX2 Event Handlers
 * ----------------------------------------------------------------------
 */


void
SBMLHandler::startElement (const XMLCh* const  uri,
                           const XMLCh* const  localname,
                           const XMLCh* const  qname,
                           const Attributes&   attrs)
{
  SBase*        obj = NULL;
  SBMLTagCode_t tag = getTagCode(uri, localname);


  /*
  debugPrintStartElement(uri, localname, qname, attrs);
  debugPrintAttrs(attrs);
  */

  //
  // If we are already inside an <annotation>, <notes> or <math> tag
  // delegate to the appropriate sub-handler.
  //
  if (inAnnotation)
  {


#ifdef USE_LAYOUT
    if (inLayout)
    {
      fLayoutHandler->startElement(uri, localname, qname, attrs);
    }
    else
    {
#endif  // USE_LAYOUT


      //
      // Track nested <annotation> tags.
      //
      if (tag == TAG_ANNOTATION || tag == TAG_ANNOTATIONS)
      {
        inAnnotation++;
        fFormatter->startElement(qname, attrs);
      }


#ifdef USE_LAYOUT     
      else if (tag == TAG_LIST_OF_LAYOUTS)
      {
        inLayout++;
        fLayoutHandler->startDocument();
        fLayoutHandler->startElement(uri, localname, qname, attrs);
      }
      else if (tag == TAG_LAYOUTID)
      {
         const string& id = this->doSpeciesReferenceId(attrs);

         SBase* o = static_cast<SBase*>( Stack_peek(fObjStack) );
         if (o->getTypeCode() == SBML_SPECIES_REFERENCE ||
             o->getTypeCode() == SBML_MODIFIER_SPECIES_REFERENCE)
         {
           static_cast<SimpleSpeciesReference*>(o)->setId(id);
         }
      }
#endif  // USE_LAYOUT


      else
      {
        fFormatter->startElement(qname, attrs);
      }


#ifdef USE_LAYOUT
    }
#endif  // USE_LAYOUT


  }
  else if (inNotes)
  {


#ifdef USE_LAYOUT
      if (!inLayout)
      {
#endif  // USE_LAYOUT


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


#ifdef USE_LAYOUT
      }
#endif  // USE_LAYOUT


  }
  else if (inMath)
  {
    fMathHandler->startElement(uri, localname, qname, attrs);
  }


  //
  // Otherwise, check for the special tags <annotation>, <notes> or <math>
  // and delegate to the appropriate sub-handler.
  //
  else if (tag == TAG_ANNOTATION)
  {
    fFormatter->startElement(qname, attrs);
    inAnnotation++;
  }
  else if (tag == TAG_ANNOTATIONS)
  {
    XMLCh* name = removeLastChar(qname);

    fFormatter->startElement(name, attrs);
    inAnnotation++;

    delete [] name;
  }


  else if (tag == TAG_NOTES)
  {
    inNotes++;
  }


  else if (tag == TAG_MATH)
  {
    fMathHandler->startDocument();
    fMathHandler->startElement(uri, localname, qname, attrs);

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

      //
      // xmlns: attributes
      //
      storeNamespaceDefinitions(obj, attrs);
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


  SBase*        obj = (SBase*) Stack_peek(fObjStack);
  SBMLTagCode_t tag = getTagCode(uri, localname);


#ifdef USE_LAYOUT
  if (tag == TAG_LIST_OF_LAYOUTS)
  {
    fLayoutHandler->endElement(uri, localname, qname);
    fLayoutHandler->endDocument();
    inLayout--;
  }
  else if (tag == TAG_LAYOUTID)
  {
  }
  else
#endif  // USE_LAYOUT


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


#ifdef USE_LAYOUT
      if (!inLayout)
      {
#endif  // USE_LAYOUT

      char* notes = const_cast<char*>( fFormatter->getString() );
      SBase_setNotes(obj, util_trim_in_place(notes));
      fFormatter->reset();


#ifdef USE_LAYOUT
      }
#endif  // USE_LAYOUT


    }

    inNotes--;
  }


  //
  // Annotation
  //
  else if (tag == TAG_ANNOTATION || tag == TAG_ANNOTATIONS)
  {
    if (tag == TAG_ANNOTATIONS)
    {
      XMLCh* name = removeLastChar(localname);  // removeLastChar(qname);
      fFormatter->endElement(name);
      delete [] name;
    }
    else
    {
      fFormatter->endElement(localname);  // fFormatter->endElement(qname);
    }

    if (inAnnotation == 1)
    {
      if ((obj->getTypeCode() == SBML_DOCUMENT) && (fDocument->level == 1))
      {
        error(ERRMSG_NO_SBML_ANNOTATION);
      }

      char* annotation = const_cast<char*>( fFormatter->getString() );
      SBase_setAnnotation(obj, util_trim_in_place(annotation));
      fFormatter->reset();
    }

    inAnnotation--;
  }


  //
  // MathML
  //
  else if (tag == TAG_MATH && !(inNotes || inAnnotation))
  {
    fMathHandler->endElement(uri, localname, qname);

    fMathHandler->endDocument();
    setMath(fMathDocument->math);
    fMathDocument->math = NULL;

    inMath--;
  }


#ifdef USE_LAYOUT   
  else if (inLayout)
  {
    fLayoutHandler->endElement(uri, localname, qname);
  }
#endif  // USE_LAYOUT


  else if (inNotes || inAnnotation)
  {
    fFormatter->endElement(qname);
  }


  else if (inMath)
  {
    fMathHandler->endElement(uri, localname, qname);
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
void
SBMLHandler::characters (const XMLCh* const  chars,
                         const unsigned int  length)
{


#ifdef USE_LAYOUT
  if (inLayout)
  {
    fLayoutHandler->characters(chars, length);
  } 
  else
  { 
#endif  // USE_LAYOUT


    if (inNotes || inAnnotation)
    {
      fFormatter->characters(chars, length);
    }
    else if (inMath)
    {
      fMathHandler->characters(chars, length);
    }


#ifdef USE_LAYOUT
  }
#endif // USE_LAYOUT

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
#ifndef USE_EXPAT

/**
 * A SAX2 XMLReader uses this method to resolve the entity (ie schema) with
 * this handler.  Can be used to switch between different schemas
 */
InputSource* 
SBMLHandler::resolveEntity(const XMLCh* const    publicId,
                           const XMLCh* const    systemId)
{
  const XMLCh* l1v1 = gFilel1v1;
  const XMLCh* l1v2 = gFilel1v2;
  const XMLCh* l2v1 = gFilel2v1;
  const XMLCh* math = gFilemath;
  const XMLCh* s2 = systemId;

  /*
   * determine which level and version have been
   * specified and return an InputSource accordingly
   */

  if (XMLString::compareString(l1v1, s2)==0) {
    return new SBMLSchemaInputSource( xsd, "FromString", 9299);
  }
  else if (XMLString::compareString(l1v2, s2) == 0) {
    return new SBMLSchemaInputSource( xsd_l1v2, "FromString", 9303);
  }
  else if (XMLString::compareString(l2v1, s2) == 0) {
    return new SBMLSchemaInputSource( xsd_l2v1, "FromString", 13749);
  }
  else if (XMLString::compareString(math, s2) == 0) {
    return new SBMLSchemaInputSource( xsd_math, "FromString", 7295);
  }
  else {
    return NULL;
  }
}
///////////////////////////////////////////////////////////////////////////////
/* ----------------------------------------------------------------------
 *                         SAX2 Error Handlers
 * ----------------------------------------------------------------------
 */


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
ParseMessage*
SBMLHandler::ParseMessage_createFrom (const char* message)
{
  return new
#ifdef USE_EXPAT
    ParseMessage( 100, message,
                  getCurrentLineNumber(), getCurrentColumnNumber() );
#else
    ParseMessage( 100, message,
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
ParseMessage*
SBMLHandler::ParseMessage_createFrom (const SAXParseException& e)
{
  char*         message;
  ParseMessage* pm;


  message = XMLString::transcode( e.getMessage() );

  pm = new ParseMessage( 100, message, 
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


#ifdef USE_LAYOUT
  fLayoutHandler =
    new LayoutHandler( &fDocument->getModel()->getListOfLayouts() );
#endif  // USE_LAYOUT


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


#ifdef USE_LAYOUT
  if ( tag == TAG_UNKNOWN &&
       !XMLString::compareString(localname, LIST_OF_LAYOUTS) )
  {
    tag = TAG_LIST_OF_LAYOUTS;
  }
  if (tag == TAG_UNKNOWN && !XMLString::compareString(localname, LAYOUTID))
  {
    tag = TAG_LAYOUTID;
  }
#endif  // USE_LAYOUT


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
      static_cast<FunctionDefinition*>(obj)->setMath(math);
      break;

    case SBML_ALGEBRAIC_RULE:
    case SBML_ASSIGNMENT_RULE:
    case SBML_RATE_RULE:
      static_cast<Rule*>(obj)->setMath(math);
      break;

    case SBML_SPECIES_REFERENCE:
      setStoichiometryMath(static_cast<SpeciesReference*>(obj), math);
      break;

    case SBML_KINETIC_LAW:
      static_cast<KineticLaw*>(obj)->setMath(math);
      break;

    case SBML_EVENT:
      if (tag == TAG_TRIGGER)
      {
        static_cast<Event*>(obj)->setTrigger(math);
      }
      else if (tag == TAG_DELAY)
      {
        static_cast<Event*>(obj)->setDelay(math);
      }
      else
      {
        freeMath = true;
      }
      break;

    case SBML_EVENT_ASSIGNMENT:
      static_cast<EventAssignment*>(obj)->setMath(math);
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
SBMLHandler::setStoichiometryMath (SpeciesReference* sr, ASTNode* math)
{
  bool freeMath = true;


  switch ( math->getType() )
  {
    case AST_INTEGER:
      sr->setStoichiometry( math->getInteger() );
      break;

    case AST_REAL:
    case AST_REAL_E:
      sr->setStoichiometry( math->getReal() );
      break;

    case AST_RATIONAL:
      sr->setStoichiometry( math->getNumerator()   );
      sr->setDenominator  ( math->getDenominator() );
      break;

    default:
      sr->setStoichiometryMath(math);
      freeMath = false;
      break;
  }

  if (freeMath)
  {
    delete math;
  }
}


/**
 * @return a duplicate of s with the last character removed.  Free the
 * returned string delete [ ].
 */
XMLCh*
removeLastChar (const XMLCh* const s)
{
  int    len    = XMLString::stringLen(s);
  XMLCh* result = new XMLCh[len];


  XMLString::copyNString(result, s, len - 1);
  result[len - 1] = chNull;

  return result;
}


/**
 * @return true if prefix begins with 'xmlns:' (case-insensitive), false
 * otherwise.
 */
bool
startsWithXMLNS (const XMLCh* const prefix)
{
  return 
    XMLString::stringLen(prefix) > 6 &&
    (prefix[0] == chLatin_x || prefix[0] == chLatin_X) &&
    (prefix[1] == chLatin_m || prefix[1] == chLatin_M) &&
    (prefix[2] == chLatin_l || prefix[2] == chLatin_L) &&
    (prefix[3] == chLatin_n || prefix[3] == chLatin_N) &&
    (prefix[4] == chLatin_s || prefix[4] == chLatin_S) &&
     prefix[5] == chColon;
}


/**
 * Stores any namespace definitions (attribute names that begin with
 * xmlns:) in the SBase object's collection of namespaces.
 */
void
storeNamespaceDefinitions (SBase *obj, const Attributes& a)
{
  for (unsigned int n = 0; n < a.getLength(); n++)
  {
    if (startsWithXMLNS( a.getQName(n) ))
    {
      char* prefix = XMLString::transcode( a.getQName(n) );
      char* URI    = XMLString::transcode( a.getValue(n) );

      obj->getNamespaces().add(prefix, URI);

      XMLString::release(&prefix);
      XMLString::release(&URI);
    }
  }
}




#ifdef USE_LAYOUT


/**
 * Sets the id attribute of a SpeciesReference.
 */
string
SBMLHandler::doSpeciesReferenceId (const Attributes& a)
{
  string s;  
  XMLUtil::scanAttr(a, ATTR_ID, s);
  return s;
}


#endif  // USE_LAYOUT




/**
 * Prints, to stdout, the value of parameters passed to startElement().
 *
void
SBMLHandler::debugPrintStartElement (const XMLCh* const  uri,
                                     const XMLCh* const  localname,
                                     const XMLCh* const  qname,
                                     const Attributes&   attrs)
{
  unsigned int line;
  unsigned int col;

  const XMLCh* s;


#if USE_EXPAT
  line = getCurrentLineNumber  ();
  col  = getCurrentColumnNumber();
#else
  line = fLocator->getLineNumber  ();
  col  = fLocator->getColumnNumber();
#endif  // USE_EXPAT


  cerr << endl;
  cerr << "SBMLHandler::startElement(...): " << endl;

  s = XMLString::transcode(uri);
  s = s ? s: "(null)";
  cerr << "        uri   = [" << s << "]" << endl;

  s = XMLString::transcode(localname);
  s = s ? s: "(null)";
  cerr << "  localname   = [" << s << "]" << endl;

  s = XMLString::transcode(qname);
  s = s ? s: "(null)";
  cerr << "      qname   = [" << s << "]" << endl;

  cerr << "       line   = [" << line << "]" << endl;
  cerr << "       col    = [" << col  << "]" << endl;

  cerr << "SBMLTagCode_t = [" << getTagCode(uri, localname) << "]" << endl;

  cerr << endl;
}


void
SBMLHandler::debugPrintAttrs (const Attributes& attrs)
{
  int n;
  int size = attrs.getLength();

  const XMLCh* s;

  cerr << "attr.getLength() = [" << size << "]" << endl;

  for (n = 0; n < size; n++)
  {
    cerr << "attr(" << n << "):" << endl;

    s = XMLString::transcode( attrs.getURI(n) );
    s = s ? s : "(null)";
    cerr << "  attr.getURI(" << n << ")       = [" << s << "]" << endl;

    s = XMLString::transcode( attrs.getLocalName(n) );
    s = s ? s : "(null)";
    cerr << "  attr.getLocalName(" << n << ") = [" << s << "]" << endl;

    s = XMLString::transcode( attrs.getQName(n) );
    s = s ? s : "(null)";
    cerr << "  attr.getQName(" << n << ")     = [" << s << "]" << endl;

    s = XMLString::transcode( attrs.getValue(n) );
    s = s ? s : "(null)";
    cerr << "  attr.getValue(" << n << ")     = [" << s << "]" << endl;

    cerr << endl;
  }

  cerr << endl << endl;
}
*/
