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

SOURCE=..\src\sbml\AlgebraicRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\AssignmentRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\CompartmentVolumeRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Event.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\EventAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\FormulaFormatter.c
# End Source File
# Begin Source File

SOURCE=..\src\math\FormulaParser.c
# End Source File
# Begin Source File

SOURCE=..\src\math\FormulaTokenizer.c
# End Source File
# Begin Source File

SOURCE=..\src\sbml\FunctionDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\src\util\List.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ListOf.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\MathMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\MathMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\MathMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\MathMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\MathMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\src\math\MathMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\util\memory.c
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ModifierSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParameterRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\xml\ParseMessage.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RateRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Rule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RuleType.c
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBase.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLVisitor.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SimpleSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Species.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesConcentrationRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\src\util\Stack.c
# End Source File
# Begin Source File

SOURCE=..\src\xml\StreamFormatTarget.cpp
# End Source File
# Begin Source File

SOURCE=..\src\util\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\src\util\StringMap.c
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Unit.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitKind.c
# End Source File
# Begin Source File

SOURCE=..\src\util\util.c
# End Source File
# Begin Source File

SOURCE=..\src\validator\ValidationRules.c
# End Source File
# Begin Source File

SOURCE=..\src\validator\Validator.c
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLNamespace.cpp
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLNamespaceList.cpp
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLStringFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLUtil.cpp
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

SOURCE=..\src\sbml\XMLNamespace.hpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\XMLNamespaceList.hpp
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
# End Target
# End Project
