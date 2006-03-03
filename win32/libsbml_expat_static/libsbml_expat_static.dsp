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
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "C:\Program Files\Expat-1.95.7\Source\lib" /I "..\..\src" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "LIBSBML_STATIC" /D "XML_STATIC" /D "USE_EXPAT" /YX /FD /c
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
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "C:\Program Files\Expat-1.95.7\Source\lib" /I "..\..\src" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "LIBSBML_STATIC" /D "XML_STATIC" /D "USE_EXPAT" /YX /FD /GZ /c
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

SOURCE=..\..\src\sbml\AlgebraicRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\AssignmentRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CompartmentOutsideCycles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\CompartmentVolumeRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\ConsistencyConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\Constraint.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\EventAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\Expat.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatAttributes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatToXerces.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatXMLString.cpp
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

SOURCE=..\..\src\sbml\FunctionDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionDefinitionVars.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\L1CompatibilityConstraints.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L1CompatibilityValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\List.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ListOf.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\memory.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ModifierSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ParameterRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ParseMessage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\RateRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Rule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\RuleType.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLParser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTagCodes.cpp
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

SOURCE=..\..\src\sbml\SimpleSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Species.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesConcentrationRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\Stack.c
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\StreamFormatTarget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\util\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\..\src\util\StringMap.c
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

SOURCE=..\..\src\validator\constraints\UniqueVarsInEventAssignments.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInRules.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Unit.cpp
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

SOURCE=..\..\src\xml\XMLNamespace.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLNamespaceList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLStringFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLUtil.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\src\sbml\AlgebraicRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\AssignmentRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\ASTNode.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\ASTNodeType.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\common.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Compartment.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\CompartmentOutsideCycles.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\CompartmentVolumeRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ConsistencyValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\Constraint.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\ConstraintMacros.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Event.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\EventAssignment.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\Expat.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatAttributes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatToXerces.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatUnicodeChars.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ExpatXMLString.h
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

SOURCE=..\..\src\sbml\FunctionDefinition.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\FunctionDefinitionVars.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\IdList.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\KineticLaw.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\L1CompatibilityValidator.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\List.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ListOf.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLReader.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLTagCodes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLUnicodeConstants.h
# End Source File
# Begin Source File

SOURCE=..\..\src\math\MathMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\memory.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Model.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ModifierSpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Parameter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\ParameterRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\ParseMessage.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\RateRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Reaction.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Rule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\RuleType.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBase.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLConvert.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLReader.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTagCodes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTypeCodes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLTypes.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLUnicodeConstants.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLVisitor.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SBMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SimpleSpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Species.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesConcentrationRule.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\SpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\Stack.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\StreamFormatTarget.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\StringBuffer.h
# End Source File
# Begin Source File

SOURCE=..\..\src\util\StringMap.h
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

SOURCE=..\..\src\validator\constraints\UniqueVarsInEventAssignments.h
# End Source File
# Begin Source File

SOURCE=..\..\src\validator\constraints\UniqueVarsInRules.h
# End Source File
# Begin Source File

SOURCE=..\..\src\sbml\Unit.h
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

SOURCE=..\..\src\xml\XMLNamespace.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLNamespaceList.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLSchemaValidation.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLStringFormatter.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLUnicodeConstants.h
# End Source File
# Begin Source File

SOURCE=..\..\src\xml\XMLUtil.h
# End Source File
# End Group
# End Target
# End Project
