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
# ADD CPP /nologo /MDd /W3 /Gm- /GX /ZI /Od /I "$(PYTHON_INCLUDE)" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "LIBSBML_EXPORTS" /YX /FD /GZ /c
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

SOURCE=..\..\src\AlgebraicRule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\AssignmentRule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\ASTNode.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Compartment.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\CompartmentVolumeRule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Event.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\EventAssignment.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\FormulaFormatter.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\FormulaParser.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\FormulaTokenizer.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\FunctionDefinition.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\KineticLaw.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\libsbml_wrap.cxx

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\List.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\ListOf.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLDocument.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLFormatter.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLHandler.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLReader.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLTagCodes.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\MathMLWriter.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\memory.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Model.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\ModifierSpeciesReference.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Parameter.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\ParameterRule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\ParseMessage.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\RateRule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Reaction.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Rule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\RuleType.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SAX2AttributesMock.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBase.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLConvert.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLDocument.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLFormatter.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLHandler.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLReader.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLTagCodes.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SBMLWriter.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SimpleSpeciesReference.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Species.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SpeciesConcentrationRule.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\SpeciesReference.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Stack.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\StringBuffer.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\StringMap.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Unit.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\UnitDefinition.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\UnitKind.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\util.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\ValidationRules.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\Validator.c

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\XMLStringFormatter.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\XMLUtil.cpp

!IF  "$(CFG)" == "Python_binding_Win32 - Win32 Release"

# ADD CPP /D "__WIN32__"

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

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
	C:\swig\swig-1.3.21\swig -c++ -I..\..\src -python $(InputPath) 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "Python_binding_Win32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\local.i
# End Source File
# End Target
# End Project
