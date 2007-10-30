# Microsoft Developer Studio Project File - Name="libsbml_expat_static" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libsbml_expat_static - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libsbml_expat_static.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libsbml_expat_static.mak" CFG="libsbml_expat_static - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libsbml_expat_static - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libsbml_expat_static - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "libsbml_expat_static - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "C:\Program Files\Expat 2.0.1\Source\lib" /I "../../include." /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "LIBSBML_STATIC" /D "XML_STATIC" /D "LIBLAX_STATIC" /D "USE_EXPAT" /D "USE_LAYOUT" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\bin\libsbml_expat_static.lib"

!ELSEIF  "$(CFG)" == "libsbml_expat_static - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "C:\Program Files\Expat 2.0.1\Source\lib" /I "../../include." /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "LIBSBML_STATIC" /D "XML_STATIC" /D "LIBLAX_STATIC" /D "USE_EXPAT" /D "USE_LAYOUT" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"Debug\libsbml_expat_staticD.lib"

!ENDIF 

# Begin Target

# Name "libsbml_expat_static - Win32 Release"
# Name "libsbml_expat_static - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\src\validator\constraints\ArgumentsUnitsCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ArgumentsUnitsCheckWarnings.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\AssignmentCycles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CiElementMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CompartmentOutsideCycles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\CompartmentType.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ConsistencyConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Constraint.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Delay.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\EqualityArgsMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\EventAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatAttributes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatParser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ExponentUnitsCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\FormulaFormatter.c
# End Source File
# Begin Source File

SOURCE=..\..\src\math\FormulaParser.c
# End Source File
# Begin Source File

SOURCE=..\..\src\math\FormulaTokenizer.c
# End Source File
# Begin Source File

SOURCE=..\..\src\units\FormulaUnitsData.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionApplyMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\FunctionDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionDefinitionVars.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionReferredToExists.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdentifierConsistencyConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\IdentifierConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\InitialAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\KineticLawVars.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\L1CompatibilityConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L1CompatibilityValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\L2v1CompatibilityConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L2v1CompatibilityValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\L2v2CompatibilityConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L2v2CompatibilityValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\L2v3CompatibilityConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L2v3CompatibilityValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LambdaMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\List.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ListOf.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LocalParameterMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LocalParameterShadowsIdInModel.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LogicalArgsMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathML.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\MathMLBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\MathMLConsistencyConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\MathMLConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\memory.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ModelingPracticeConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ModelingPracticeValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\NumberArgsMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\NumericArgsMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\NumericReturnMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\OverDeterminedCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\OverdeterminedConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\OverdeterminedValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PieceBooleanMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PiecewiseValueMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PowerUnitsCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Rule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLConvert.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLError.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLErrorLog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTypeCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLVisitor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBO.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\SBOConsistencyConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\SBOConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Species.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\SpeciesReactionOrRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesType.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\Stack.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\StoichiometryMath.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\StoichiometryMathVars.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Trigger.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdsForUnitDefinitions.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdsInKineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdsInModel.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueMetaId.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueSpeciesTypesInCompartment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueSymbolsInInitialAssignments.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInEventAssignments.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInEventsAndRules.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInInitialAssignmentsAndRules.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInRules.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Unit.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UnitConsistencyConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\UnitConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\UnitDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\units\UnitFormulaFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\UnitKind.c
# End Source File
# Begin Source File

SOURCE=..\..\src\units\UnitKindList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UnitsBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\util.c
# End Source File
# Begin Source File

SOURCE=..\..\src\units\Utils_Unit.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\units\Utils_UnitDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\Validator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\VConstraint.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLAttributes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLBuffer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLError.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLErrorLog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLFileBuffer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLInputStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLMemoryBuffer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLNamespaces.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLOutputStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLParser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLToken.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLTokenizer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLTriple.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\src\validator\constraints\ArgumentsUnitsCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ArgumentsUnitsCheckWarnings.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\AssignmentCycles.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\ASTNode.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CiElementMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\common\common.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Compartment.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CompartmentOutsideCycles.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\CompartmentType.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Constraint.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ConstraintMacros.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Delay.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\EqualityArgsMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Event.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\EventAssignment.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatParser.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ExponentUnitsCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\common\extern.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\FormulaFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\FormulaParser.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\FormulaTokenizer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\units\FormulaUnitsData.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionApplyMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\FunctionDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionDefinitionVars.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionReferredToExists.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\IdentifierConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdList.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\InitialAssignment.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\KineticLaw.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\KineticLawVars.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L1CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L2v1CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L2v2CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L2v3CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LambdaMathCheck.h
# End Source File
# Begin Source File

SOURCE="..\..\src\common\libsbml-config-unix.h"
# End Source File
# Begin Source File

SOURCE="..\..\src\common\libsbml-config-win.h"
# End Source File
# Begin Source File

SOURCE="..\..\src\common\libsbml-config.h"
# End Source File
# Begin Source File

SOURCE="..\..\src\common\libsbml-package.h"
# End Source File
# Begin Source File

SOURCE="..\..\src\common\libsbml-version.h"
# End Source File
# Begin Source File

SOURCE=..\..\src\util\List.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ListOf.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LocalParameterMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LocalParameterShadowsIdInModel.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\LogicalArgsMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathML.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\MathMLBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\MathMLConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\memory.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Model.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ModelingPracticeValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\NumberArgsMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\NumericArgsMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\NumericReturnMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\OverDeterminedCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\OverdeterminedValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Parameter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PieceBooleanMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PiecewiseValueMathCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PowerUnitsCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Reaction.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Rule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLError.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLErrorLog.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLErrorTable.h
# End Source File
# Begin Source File

SOURCE=..\..\src\common\sbmlfwd.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLReader.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTypeCodes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTypes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLVisitor.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBO.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\SBOConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Species.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\SpeciesReactionOrRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesType.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\Stack.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\StoichiometryMath.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\StoichiometryMathVars.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\StringBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Trigger.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdsForUnitDefinitions.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdsInKineticLaw.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueIdsInModel.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueMetaId.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueSpeciesTypesInCompartment.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueSymbolsInInitialAssignments.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInEventAssignments.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInEventsAndRules.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInInitialAssignmentsAndRules.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInRules.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Unit.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\UnitConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\UnitDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\src\units\UnitFormulaFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\UnitKind.h
# End Source File
# Begin Source File

SOURCE=..\..\src\units\UnitKindList.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UnitsBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\util.h
# End Source File
# Begin Source File

SOURCE=..\..\src\units\Utils_Unit.h
# End Source File
# Begin Source File

SOURCE=..\..\src\units\Utils_UnitDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\Validator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\VConstraint.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLError.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLErrorLog.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLExtern.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLFileBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLInputStream.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLMemoryBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLNamespaces.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLNode.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLOutputStream.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLParser.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLToken.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLTokenizer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLTriple.h
# End Source File
# End Group
# End Target
# End Project
