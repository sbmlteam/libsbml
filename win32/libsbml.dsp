# Microsoft Developer Studio Project File - Name="libsbml" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=libsbml - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libsbml.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libsbml.mak" CFG="libsbml - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libsbml - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libsbml - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libsbml - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "bin"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 bin/xerces-c_2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "libsbml - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "bin"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 bin/xerces-c_2D.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"bin/libsbmlD.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "libsbml - Win32 Release"
# Name "libsbml - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\AlgebraicRule.c
# End Source File
# Begin Source File

SOURCE=..\src\AlgebraicRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AssignmentRule.c
# End Source File
# Begin Source File

SOURCE=..\src\AssignmentRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ASTNode.c
# End Source File
# Begin Source File

SOURCE=..\src\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Compartment.c
# End Source File
# Begin Source File

SOURCE=..\src\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\src\CompartmentVolumeRule.c
# End Source File
# Begin Source File

SOURCE=..\src\CompartmentVolumeRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Event.c
# End Source File
# Begin Source File

SOURCE=..\src\Event.cpp
# End Source File
# Begin Source File

SOURCE=..\src\EventAssignment.c
# End Source File
# Begin Source File

SOURCE=..\src\EventAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\src\FormulaFormatter.c
# End Source File
# Begin Source File

SOURCE=..\src\FormulaParser.c
# End Source File
# Begin Source File

SOURCE=..\src\FormulaTokenizer.c
# End Source File
# Begin Source File

SOURCE=..\src\FunctionDefinition.c
# End Source File
# Begin Source File

SOURCE=..\src\FunctionDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\src\KineticLaw.c
# End Source File
# Begin Source File

SOURCE=..\src\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\src\List.c
# End Source File
# Begin Source File

SOURCE=..\src\List.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ListOf.c
# End Source File
# Begin Source File

SOURCE=..\src\ListOf.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MathMLDocument.c
# End Source File
# Begin Source File

SOURCE=..\src\MathMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MathMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MathMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MathMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MathMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MathMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\memory.c
# End Source File
# Begin Source File

SOURCE=..\src\Model.c
# End Source File
# Begin Source File

SOURCE=..\src\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ModifierSpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\ModifierSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Parameter.c
# End Source File
# Begin Source File

SOURCE=..\src\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ParameterRule.c
# End Source File
# Begin Source File

SOURCE=..\src\ParameterRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ParseMessage.c
# End Source File
# Begin Source File

SOURCE=..\src\ParseMessage.cpp
# End Source File
# Begin Source File

SOURCE=..\src\RateRule.c
# End Source File
# Begin Source File

SOURCE=..\src\RateRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Reaction.c
# End Source File
# Begin Source File

SOURCE=..\src\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Rule.c
# End Source File
# Begin Source File

SOURCE=..\src\Rule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\RuleType.c
# End Source File
# Begin Source File

SOURCE=..\src\SAX2AttributesMock.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBase.c
# End Source File
# Begin Source File

SOURCE=..\src\SBase.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\src\SBMLDocument.c
# End Source File
# Begin Source File

SOURCE=..\src\SBMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SBMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SimpleSpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\SimpleSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Species.c
# End Source File
# Begin Source File

SOURCE=..\src\Species.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SpeciesConcentrationRule.c
# End Source File
# Begin Source File

SOURCE=..\src\SpeciesConcentrationRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Stack.c
# End Source File
# Begin Source File

SOURCE=..\src\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\src\StringMap.c
# End Source File
# Begin Source File

SOURCE=..\src\TestAlgebraicRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestAssignmentRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestASTNode.c
# End Source File
# Begin Source File

SOURCE=..\src\TestCompartment.c
# End Source File
# Begin Source File

SOURCE=..\src\TestCompartmentVolumeRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestConsistencyChecks.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestEvent.c
# End Source File
# Begin Source File

SOURCE=..\src\TestEventAssignment.c
# End Source File
# Begin Source File

SOURCE=..\src\TestFormulaFormatter.c
# End Source File
# Begin Source File

SOURCE=..\src\TestFormulaParser.c
# End Source File
# Begin Source File

SOURCE=..\src\TestFormulaTokenizer.c
# End Source File
# Begin Source File

SOURCE=..\src\TestFunctionDefinition.c
# End Source File
# Begin Source File

SOURCE=..\src\TestKineticLaw.c
# End Source File
# Begin Source File

SOURCE=..\src\TestList.c
# End Source File
# Begin Source File

SOURCE=..\src\TestListOf.c
# End Source File
# Begin Source File

SOURCE=..\src\TestMathMLDocument.c
# End Source File
# Begin Source File

SOURCE=..\src\TestMathMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestMathMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestMathMLWriter.c
# End Source File
# Begin Source File

SOURCE=..\src\TestMemory.c
# End Source File
# Begin Source File

SOURCE=..\src\TestModel.c
# End Source File
# Begin Source File

SOURCE=..\src\TestModifierSpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\TestParameter.c
# End Source File
# Begin Source File

SOURCE=..\src\TestParameterRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestParseMessage.c
# End Source File
# Begin Source File

SOURCE=..\src\TestRateRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestReaction.c
# End Source File
# Begin Source File

SOURCE=..\src\TestReadFromFile1.c
# End Source File
# Begin Source File

SOURCE=..\src\TestReadFromFile2.c
# End Source File
# Begin Source File

SOURCE=..\src\TestReadFromFile3.c
# End Source File
# Begin Source File

SOURCE=..\src\TestReadFromFile4.c
# End Source File
# Begin Source File

SOURCE=..\src\TestRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestRuleType.c
# End Source File
# Begin Source File

SOURCE=..\src\TestRunner.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSAX2AttributesMock.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestSBase.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestSBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSBMLDocument.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSBMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestSBMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestSBMLReader.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSBMLWriter.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSimpleSpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSpecies.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSpeciesConcentrationRule.c
# End Source File
# Begin Source File

SOURCE=..\src\TestSpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\TestStack.c
# End Source File
# Begin Source File

SOURCE=..\src\TestStringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\src\TestStringMap.c
# End Source File
# Begin Source File

SOURCE=..\src\TestUnit.c
# End Source File
# Begin Source File

SOURCE=..\src\TestUnitDefinition.c
# End Source File
# Begin Source File

SOURCE=..\src\TestUnitKind.c
# End Source File
# Begin Source File

SOURCE=..\src\TestUtil.c
# End Source File
# Begin Source File

SOURCE=..\src\TestValidator.c
# End Source File
# Begin Source File

SOURCE=..\src\TestXMLStringFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TestXMLUtil.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Unit.c
# End Source File
# Begin Source File

SOURCE=..\src\Unit.cpp
# End Source File
# Begin Source File

SOURCE=..\src\UnitDefinition.c
# End Source File
# Begin Source File

SOURCE=..\src\UnitDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\src\UnitKind.c
# End Source File
# Begin Source File

SOURCE=..\src\util.c
# End Source File
# Begin Source File

SOURCE=..\src\ValidationRules.c
# End Source File
# Begin Source File

SOURCE=..\src\Validator.c
# End Source File
# Begin Source File

SOURCE=..\src\XMLStringFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\XMLUtil.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\src\sbml\AlgebraicRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\AlgebraicRule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\AssignmentRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\AssignmentRule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ASTNode.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ASTNode.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ASTNodeType.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\common.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\common.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Compartment.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Compartment.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\CompartmentVolumeRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\CompartmentVolumeRule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\config.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Event.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Event.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\EventAssignment.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\EventAssignment.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\extern.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\FormulaFormatter.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\FormulaParser.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\FormulaTokenizer.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\FunctionDefinition.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\FunctionDefinition.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\KineticLaw.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\KineticLaw.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\List.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\List.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ListOf.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ListOf.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLDocument.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLFormatter.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLHandler.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLReader.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLTagCodes.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLUnicodeConstants.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\memory.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Model.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Model.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ModifierSpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ModifierSpeciesReference.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Parameter.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Parameter.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParameterRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParameterRule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParseMessage.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParseMessage.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RateRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RateRule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Reaction.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Reaction.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Rule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Rule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RuleType.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SAX2AttributesMock.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBase.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBase.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLConvert.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLDocument.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLFormatter.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLHandler.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLReader.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLReader.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTagCodes.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTypeCodes.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTypes.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTypes.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLUnicodeConstants.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SimpleSpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SimpleSpeciesReference.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Species.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Species.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesConcentrationRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesConcentrationRule.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesReference.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Stack.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\StringBuffer.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\StringMap.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Unit.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Unit.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitDefinition.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitDefinition.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitKind.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\util.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Validator.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\XMLSchemaValidation.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\XMLStringFormatter.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\XMLUnicodeConstants.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\XMLUtil.hpp
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=..\src\sbml\config.h.in
# End Source File
# End Target
# End Project
