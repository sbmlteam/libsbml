#|
\file    boolean-functions.lisp
\brief
\author  Martin Ginkel <mginkel@mpi-mageburg.mpg.de>

$Id$
$Source$

Copyright 2004 Max-Planck-Institute Magdeburg

This is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or
any later version.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
|#


(cl:in-package :libsbml)


(cl:defvar  LIBSBML::*BOOLEAN-FUNCTIONS*)
(setf LIBSBML::*BOOLEAN-FUNCTIONS*
    '("SBase_isSetMetaId" "ASTNode_isConstant"
    "ASTNode_isFunction" "ASTNode_isInteger" "ASTNode_isLambda"
    "ASTNode_isLog10" "ASTNode_isLogical" "ASTNode_isName" "ASTNode_isNumber"
    "ASTNode_isOperator" "ASTNode_isRational" "ASTNode_isReal"
    "ASTNode_isRelational" "ASTNode_isSqrt" "ASTNode_isUMinus"
    "ASTNode_isUnknown" 
    
    "FunctionDefinition_isSetId"
    "FunctionDefinition_isSetName" "FunctionDefinition_isSetMath"
    "Unit_isSetKind" "UnitDefinition_isSetId" "UnitDefinition_isSetName"

    "Compartment_isSetId" "Compartment_getConstant"
    "Compartment_isSetName" "Compartment_isSetSize" "Compartment_isSetVolume"
    "Compartment_isSetUnits" "Compartment_isSetOutside" 
    
    "Species_isSetId" "Species_isSetName" "Species_isSetCompartment" 
    "Species_isSetInitialAmount" "Species_isSetInitialConcentration" 
    "Species_isSetSpatialSizeUnits" "Species_isSetUnits" "Species_isSetCharge" 
    "Species_setName"
    ("Species_setHasOnlySubstanceUnits" "value") 
    ("Species_setBoundaryCondition" "value")
    ("Species_setConstant" "value")
    "Species_getConstant" 
    "Species_getBoundaryCondition"
    "Species_getHasOnlySubstanceUnits"
    
    "Parameter_getConstant" "Parameter_isSetId" "Parameter_isSetName" "Parameter_isSetValue" "Parameter_isSetUnits" 
    ("Parameter_setConstant" "value")
    "Rule_isSetFormula" "Rule_isSetMath" "AssignmentRule_isSetVariable"
    "RateRule_isSetVariable" "CompartmentVolumeRule_isSetCompartment"
    "ParameterRule_isSetName" "ParameterRule_isSetUnits"
    "SpeciesConcentrationRule_isSetSpecies" 
    "KineticLaw_isSetFormula"
    "KineticLaw_isSetMath" "KineticLaw_isSetTimeUnits"
    "KineticLaw_isSetSubstanceUnits" 
    
    "SimpleSpeciesReference_isSetSpecies"
    "ModifierSpeciesReference_isSetSpecies" "SpeciesReference_isSetSpecies"
    "SpeciesReference_isSetStoichiometryMath" 
    
     "Reaction_getFast" "Reaction_isSetId" 
     "Reaction_isSetName" "Reaction_isSetKineticLaw" "Reaction_isSetFast" 
     "Reaction_getReversible"  
     ("Reaction_setReversible" "value")
     ("Reaction_setFast" "value")
     
     "EventAssignment_isSetVariable" 
     "EventAssignment_isSetMath" 
     "Event_isSetId" "Event_isSetName" 
     "Event_isSetTrigger" "Event_isSetDelay" 
     "Event_isSetTimeUnits" 
     
     "Model_isSetId" "Model_isSetName" 
     ))


    
