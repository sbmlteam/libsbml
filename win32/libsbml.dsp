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
# ADD CPP /nologo /MDd /W3 /Gm /GR /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /GZ /c
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

SOURCE=..\src\validator\constraints\CompartmentOutsideCycles.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\CompartmentVolumeRule.cpp
# End Source File
# Begin Source File

SOURCE=..\src\validator\ConsistencyValidator.cpp
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

SOURCE=..\src\validator\constraints\IdList.cpp
# End Source File
# Begin Source File

SOURCE=..\src\sbml\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\src\validator\L1CompatibilityValidator.cpp
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

SOURCE=..\src\validator\Validator.cpp
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
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
