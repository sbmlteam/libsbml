# Microsoft Developer Studio Project File - Name="Java_binding_win32" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Java_binding_win32 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Java_binding_win32.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Java_binding_win32.mak" CFG="Java_binding_win32 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Java_binding_win32 - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Java_binding_win32 - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Java_binding_win32 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "libsbml"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "JAVA_BINDING_WIN32_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(JAVA_INCLUDE)" /I "$(JAVA_INCLUDE)\win32" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib xerces-c_2.lib /nologo /dll /machine:I386 /out:"sbmlj.dll"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=java post build
PostBuild_Cmds=echo on	%JAVA_BIN%\javac *.java
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Java_binding_win32 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "JAVA_BINDING_WIN32_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "$(JAVA_INCLUDE)" /I "$(JAVA_INCLUDE)\win32" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib xerces-c_2D.lib /nologo /dll /debug /machine:I386 /out:"sbmljD.dll" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=java
PostBuild_Cmds=echo on	%JAVA_BIN%\javac *.java
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "Java_binding_win32 - Win32 Release"
# Name "Java_binding_win32 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\src\AlgebraicRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\AssignmentRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\CompartmentVolumeRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\EventAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\FormulaFormatter.c
# End Source File
# Begin Source File

SOURCE=..\..\src\FormulaParser.c
# End Source File
# Begin Source File

SOURCE=..\..\src\FormulaTokenizer.c
# End Source File
# Begin Source File

SOURCE=..\..\src\FunctionDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=.\libsbml_wrap.cxx
# End Source File
# Begin Source File

SOURCE=..\..\src\List.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ListOf.cpp
# End Source File
# Begin Source File

SOURCE=.\local.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\memory.c
# End Source File
# Begin Source File

SOURCE=..\..\src\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ModifierSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ParameterRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ParseMessage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\RateRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Rule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\RuleType.c
# End Source File
# Begin Source File

SOURCE=..\..\src\SAX2AttributesMock.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SimpleSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Species.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SpeciesConcentrationRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Stack.c
# End Source File
# Begin Source File

SOURCE=..\..\src\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\..\src\StringMap.c
# End Source File
# Begin Source File

SOURCE=..\..\src\Unit.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\UnitDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\UnitKind.c
# End Source File
# Begin Source File

SOURCE=..\..\src\util.c
# End Source File
# Begin Source File

SOURCE=..\..\src\ValidationRules.c
# End Source File
# Begin Source File

SOURCE=..\..\src\Validator.c
# End Source File
# Begin Source File

SOURCE=..\..\src\XMLStringFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\XMLUtil.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\libsbml.i

!IF  "$(CFG)" == "Java_binding_win32 - Win32 Release"

# Begin Custom Build
OutDir=.\libsbml
InputPath=.\libsbml.i
InputName=libsbml

"$(InputName)_wrap.cxx" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	C:\swig\swig-1.3.21\swig.exe -c++  -I..\..\src -java -package libsbml -outdir $(OutDir)  $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "Java_binding_win32 - Win32 Debug"

# Begin Custom Build
OutDir=.\Debug
InputPath=.\libsbml.i
InputName=libsbml

"$(InputName)_wrap.cxx" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	C:\swig\swig-1.3.21\swig.exe -c++ -java -package libsbml -outdir $(OutDir) $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\local.i
# End Source File
# End Target
# End Project
