# Microsoft Developer Studio Generated NMAKE File, Based on validateSBML.dsp
!IF "$(CFG)" == ""
CFG=validateSBML - Win32 Debug
!MESSAGE No configuration specified. Defaulting to validateSBML - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "validateSBML - Win32 Release" && "$(CFG)" != "validateSBML - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "validateSBML.mak" CFG="validateSBML - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "validateSBML - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "validateSBML - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "validateSBML - Win32 Release"

OUTDIR=.\bin
INTDIR=.\validateSBML___Win32_Release
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\validateSBML.exe"

!ELSE 

ALL : "libsbml - Win32 Release" "$(OUTDIR)\validateSBML.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\validateSBML.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\validateSBML.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\validateSBML.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\validateSBML.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbml.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\validateSBML.pdb" /machine:I386 /out:"$(OUTDIR)\validateSBML.exe" 
LINK32_OBJS= \
	"$(INTDIR)\util.obj" \
	"$(INTDIR)\validateSBML.obj" \
	"$(OUTDIR)\libsbml.lib"

"$(OUTDIR)\validateSBML.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "validateSBML - Win32 Debug"

OUTDIR=.\bin
INTDIR=.\validateSBML___Win32_Debug
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\validateSBMLD.exe"

!ELSE 

ALL : "libsbml - Win32 Debug" "$(OUTDIR)\validateSBMLD.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\validateSBML.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\validateSBMLD.exe"
	-@erase "$(OUTDIR)\validateSBMLD.ilk"
	-@erase "$(OUTDIR)\validateSBMLD.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\validateSBML.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\validateSBML.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbmlD.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\validateSBMLD.pdb" /debug /machine:I386 /out:"$(OUTDIR)\validateSBMLD.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\util.obj" \
	"$(INTDIR)\validateSBML.obj" \
	"$(OUTDIR)\libsbmlD.lib"

"$(OUTDIR)\validateSBMLD.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("validateSBML.dep")
!INCLUDE "validateSBML.dep"
!ELSE 
!MESSAGE Warning: cannot find "validateSBML.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "validateSBML - Win32 Release" || "$(CFG)" == "validateSBML - Win32 Debug"
SOURCE=..\examples\util.c

"$(INTDIR)\util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\examples\validateSBML.c

"$(INTDIR)\validateSBML.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "validateSBML - Win32 Release"

"libsbml - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" 
   cd "."

"libsbml - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" RECURSE=1 CLEAN 
   cd "."

!ELSEIF  "$(CFG)" == "validateSBML - Win32 Debug"

"libsbml - Win32 Debug" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Debug" 
   cd "."

"libsbml - Win32 DebugCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Debug" RECURSE=1 CLEAN 
   cd "."

!ENDIF 


!ENDIF 

