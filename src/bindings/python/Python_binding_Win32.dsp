# Microsoft Developer Studio Project File - Name="Python_binding_Win32" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Python_binding_Win32 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Python_binding_Win32.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Python_binding_Win32.mak" CFG="Python_binding_Win32 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Python_binding_Win32 - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Python_binding_Win32 - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "PYTHON_BINDING_WIN32_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(PYTHON_INCLUDE)" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "LIBSBML_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib xerces-c_2.lib "$(PYTHON_LIB)" /nologo /subsystem:windows /dll /machine:I386 /out:"_libsbml.dll" /export:init_libsbml
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

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
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "PYTHON_BINDING_WIN32_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /GX /ZI /Od /I "$(PYTHON_INCLUDE)" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "LIBSBML_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib xerces-c_2D.lib "$(PYTHON_LIB)" /nologo /dll /debug /machine:I386 /out:"_libsbmlD.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Python_binding_Win32 - Win32 Release"
# Name "Python_binding_Win32 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\sbml\AlgebraicRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\AssignmentRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\ASTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Compartment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\validator\constraints\CompartmentOutsideCycles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\CompartmentVolumeRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\validator\ConsistencyValidator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Event.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\EventAssignment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\FormulaFormatter.c
# End Source File
# Begin Source File

SOURCE=..\..\math\FormulaParser.c
# End Source File
# Begin Source File

SOURCE=..\..\math\FormulaTokenizer.c
# End Source File
# Begin Source File

SOURCE=..\..\sbml\FunctionDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\validator\constraints\IdList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\KineticLaw.cpp
# End Source File
# Begin Source File

SOURCE=..\..\validator\L1CompatibilityValidator.cpp
# End Source File
# Begin Source File

SOURCE=.\libsbml_wrap.cxx
# End Source File
# Begin Source File

SOURCE=..\..\util\List.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\ListOf.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\MathMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\MathMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\MathMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\MathMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\MathMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\math\MathMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\util\memory.c
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\ModifierSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Parameter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\ParameterRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\xml\ParseMessage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\RateRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Rule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\RuleType.c
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLConvert.c
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLTagCodes.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLVisitor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SBMLWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SimpleSpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Species.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SpeciesConcentrationRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\SpeciesReference.cpp
# End Source File
# Begin Source File

SOURCE=..\..\util\Stack.c
# End Source File
# Begin Source File

SOURCE=..\..\xml\StreamFormatTarget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\util\StringBuffer.c
# End Source File
# Begin Source File

SOURCE=..\..\util\StringMap.c
# End Source File
# Begin Source File

SOURCE=..\..\sbml\Unit.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\UnitDefinition.cpp
# End Source File
# Begin Source File

SOURCE=..\..\sbml\UnitKind.c
# End Source File
# Begin Source File

SOURCE=..\..\util\util.c
# End Source File
# Begin Source File

SOURCE=..\..\validator\Validator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\xml\XMLNamespace.cpp
# End Source File
# Begin Source File

SOURCE=..\..\xml\XMLNamespaceList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\xml\XMLStringFormatter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\xml\XMLUtil.cpp
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

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# Begin Custom Build
InputPath=.\libsbml.i
InputName=libsbml

"$(InputName)_wrap.cxx" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	echo In order to function correctly, please ensure the following environment variables are correctly set: 
	echo PYTHON_INCLUDE: %PYTHON_INCLUDE% 
	echo PYTHON_LIB: %PYTHON_LIB% 
	echo on 
	C:\swig\swig-1.3.24\swig -c++ -I..\..\src -python $(InputPath) 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\local.i
# End Source File
# End Target
# End Project
