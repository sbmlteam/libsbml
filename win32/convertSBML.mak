# Microsoft Developer Studio Generated NMAKE File, Based on convertSBML.dsp
!IF "$(CFG)" == ""
CFG=convertSBML - Win32 Debug
!MESSAGE No configuration specified. Defaulting to convertSBML - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "convertSBML - Win32 Release" && "$(CFG)" != "convertSBML - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "convertSBML.mak" CFG="convertSBML - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "convertSBML - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "convertSBML - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "convertSBML - Win32 Release"

OUTDIR=.\bin
INTDIR=.\convertSBML___Win32_Release
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\convertSBML.exe"

!ELSE 

ALL : "libsbml - Win32 Release" "$(OUTDIR)\convertSBML.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\convertSBML.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\convertSBML.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\convertSBML.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\convertSBML.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbml.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\convertSBML.pdb" /machine:I386 /out:"$(OUTDIR)\convertSBML.exe" 
LINK32_OBJS= \
	"$(INTDIR)\convertSBML.obj" \
	"$(INTDIR)\util.obj" \
	"$(OUTDIR)\libsbml.lib"

"$(OUTDIR)\convertSBML.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "convertSBML - Win32 Debug"

OUTDIR=.\bin
INTDIR=.\convertSBML___Win32_Debug
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\convertSBMLD.exe"

!ELSE 

ALL : "libsbml - Win32 Debug" "$(OUTDIR)\convertSBMLD.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\convertSBML.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\convertSBMLD.exe"
	-@erase "$(OUTDIR)\convertSBMLD.ilk"
	-@erase "$(OUTDIR)\convertSBMLD.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\convertSBML.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\convertSBML.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbmlD.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\convertSBMLD.pdb" /debug /machine:I386 /out:"$(OUTDIR)\convertSBMLD.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\convertSBML.obj" \
	"$(INTDIR)\util.obj" \
	"$(OUTDIR)\libsbmlD.lib"

"$(OUTDIR)\convertSBMLD.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("convertSBML.dep")
!INCLUDE "convertSBML.dep"
!ELSE 
!MESSAGE Warning: cannot find "convertSBML.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "convertSBML - Win32 Release" || "$(CFG)" == "convertSBML - Win32 Debug"
SOURCE=..\examples\convertSBML.c

"$(INTDIR)\convertSBML.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\examples\util.c

"$(INTDIR)\util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "convertSBML - Win32 Release"

"libsbml - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" 
   cd "."

"libsbml - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" RECURSE=1 CLEAN 
   cd "."

!ELSEIF  "$(CFG)" == "convertSBML - Win32 Debug"

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

