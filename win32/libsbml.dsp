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

SOURCE=..\src\AssignmentRule.c
# End Source File
# Begin Source File

SOURCE=..\src\ASTNode.c
# End Source File
# Begin Source File

SOURCE=..\src\Compartment.c
# End Source File
# Begin Source File

SOURCE=..\src\CompartmentVolumeRule.c
# End Source File
# Begin Source File

SOURCE=..\src\Event.c
# End Source File
# Begin Source File

SOURCE=..\src\EventAssignment.c
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

SOURCE=..\src\KineticLaw.c
# End Source File
# Begin Source File

SOURCE=..\src\List.c
# End Source File
# Begin Source File

SOURCE=..\src\ListOf.c
# End Source File
# Begin Source File

SOURCE=..\src\MathMLDocument.c
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

SOURCE=..\src\ModifierSpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\Parameter.c
# End Source File
# Begin Source File

SOURCE=..\src\ParameterRule.c
# End Source File
# Begin Source File

SOURCE=..\src\ParseMessage.c
# End Source File
# Begin Source File

SOURCE=..\src\RateRule.c
# End Source File
# Begin Source File

SOURCE=..\src\Reaction.c
# End Source File
# Begin Source File

SOURCE=..\src\Rule.c
# End Source File
# Begin Source File

SOURCE=..\src\RuleType.c
# End Source File
# Begin Source File

SOURCE=..\src\SBase.c
# End Source File
# Begin Source File

SOURCE=..\src\SBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\src\SBMLDocument.c
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

SOURCE=..\src\Species.c
# End Source File
# Begin Source File

SOURCE=..\src\SpeciesConcentrationRule.c
# End Source File
# Begin Source File

SOURCE=..\src\SpeciesReference.c
# End Source File
# Begin Source File

SOURCE=..\src\Stack.c
# End Source File
# Begin Source File

SOURCE=..\src\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\src\Unit.c
# End Source File
# Begin Source File

SOURCE=..\src\UnitDefinition.c
# End Source File
# Begin Source File

SOURCE=..\src\UnitKind.c
# End Source File
# Begin Source File

SOURCE=..\src\util.c
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

SOURCE=..\src\sbml\AssignmentRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ASTNode.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\common.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Compartment.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\CompartmentVolumeRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\config.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Event.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\EventAssignment.h
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

SOURCE=..\src\sbml\KineticLaw.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\List.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ListOf.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\MathMLReader.h
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

SOURCE=..\src\sbml\ModifierSpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Parameter.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParameterRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\ParseMessage.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RateRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Reaction.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Rule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\RuleType.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBase.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLConvert.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLDocument.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLReader.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTypeCodes.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLTypes.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SBMLWriter.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SimpleSpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Species.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesConcentrationRule.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\SpeciesReference.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Stack.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\StringBuffer.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\Unit.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitDefinition.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\UnitKind.h
# End Source File
# Begin Source File

SOURCE=..\src\sbml\util.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
