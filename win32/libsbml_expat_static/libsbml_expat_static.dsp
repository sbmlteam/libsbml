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
CPP=cl.exe
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
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "C:\Copasi\Expat-2.0.0\Source\lib" /I "../../include." /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "LIBSBML_STATIC" /D "XML_STATIC" /D "LIBLAX_STATIC" /D "USE_EXPAT" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
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
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "C:\Program Files\Expat-2.0.0\Source\lib" /I "../../include" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "LIBSBML_STATIC" /D "XML_STATIC" /D "USE_EXPAT" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

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

SOURCE=..\..\src\validator\constraints\AssignmentCycles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\BoundingBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CiElementMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\CompartmentGlyph.cpp
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

SOURCE=..\..\src\sbml\layout\CubicBezier.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\Curve.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\annotation\CVTerm.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Delay.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\Dimensions.cpp
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

SOURCE=..\..\src\sbml\layout\GraphicalObject.cpp
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

SOURCE=..\..\src\validator\constraints\LambdaMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\Layout.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\annotation\LayoutAnnotation.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\LayoutUtilities.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\LineSegment.cpp
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

SOURCE=..\..\src\annotation\ModelHistory.cpp
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

SOURCE=..\..\src\sbml\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PieceBooleanMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PiecewiseValueMathCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\Point.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\PowerUnitsCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\annotation\RDFAnnotation.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\ReactionGlyph.cpp
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

SOURCE=..\..\src\sbml\layout\SpeciesGlyph.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\SpeciesReactionOrRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\layout\SpeciesReferenceGlyph.cpp
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

SOURCE=..\..\src\sbml\layout\TextGlyph.cpp
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

SOURCE=..\..\include\sbml\math\ASTNode.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\BoundingBox.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\common\common.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Compartment.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\CompartmentGlyph.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\CompartmentType.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\ConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Constraint.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\ConstraintMacros.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\CubicBezier.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\Curve.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\annotation\CVTerm.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Delay.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\Dimensions.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Event.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\EventAssignment.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\ExpatAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\ExpatHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\ExpatParser.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\common\extern.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\math\FormulaFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\math\FormulaParser.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\math\FormulaTokenizer.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\units\FormulaUnitsData.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\FunctionDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\GraphicalObject.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\IdentifierConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\InitialAssignment.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\KineticLaw.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\L1CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\L2v1CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\L2v2CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\Layout.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\annotation\LayoutAnnotation.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\layoutfwd.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\LayoutUtilities.h
# End Source File
# Begin Source File

SOURCE="..\..\include\sbml\common\libsbml-config-win.h"
# End Source File
# Begin Source File

SOURCE="..\..\include\sbml\common\libsbml-config.h"
# End Source File
# Begin Source File

SOURCE="..\..\include\sbml\common\libsbml-package.h"
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\LibXMLAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\LibXMLHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\LibXMLNamespaces.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\LibXMLParser.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\LibXMLTranscode.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\LineSegment.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\util\List.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\ListOf.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\math\MathML.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\MathMLConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\util\memory.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Model.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\annotation\ModelHistory.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Parameter.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\Point.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\annotation\RDFAnnotation.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Reaction.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\ReactionGlyph.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Rule.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBase.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLError.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLErrorLog.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\common\sbmlfwd.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLReader.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLTypeCodes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLTypes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLVisitor.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SBO.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\SBOConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Species.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\SpeciesGlyph.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\SpeciesReferenceGlyph.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\SpeciesReferenceRole.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\SpeciesType.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\util\Stack.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\StoichiometryMath.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\util\StringBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\layout\TextGlyph.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Trigger.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\Unit.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\UnitConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\UnitDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\units\UnitFormulaFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\UnitKind.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\units\UnitKindList.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\util\util.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\units\Utils_Unit.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\units\Utils_UnitDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\Validator.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\validator\VConstraint.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XercesAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XercesHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XercesNamespaces.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XercesParser.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XercesTranscode.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLError.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLErrorLog.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLExtern.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLFileBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLInputStream.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLMemoryBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLNamespaces.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLNode.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLOutputStream.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLParser.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLToken.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLTokenizer.h
# End Source File
# Begin Source File

SOURCE=..\..\include\sbml\xml\XMLTriple.h
# End Source File
# End Group
# End Target
# End Project
