# Microsoft Developer Studio Generated NMAKE File, Based on translateMath.dsp
!IF "$(CFG)" == ""
CFG=translateMath - Win32 Debug
!MESSAGE No configuration specified. Defaulting to translateMath - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "translateMath - Win32 Release" && "$(CFG)" != "translateMath - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "translateMath.mak" CFG="translateMath - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "translateMath - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "translateMath - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "translateMath - Win32 Release"

OUTDIR=.\bin
INTDIR=.\translateMath___Win32_Release
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\translateMath.exe"

!ELSE 

ALL : "libsbml - Win32 Release" "$(OUTDIR)\translateMath.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\translateMath.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\translateMath.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\translateMath.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\translateMath.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbml.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\translateMath.pdb" /machine:I386 /out:"$(OUTDIR)\translateMath.exe" 
LINK32_OBJS= \
	"$(INTDIR)\translateMath.obj" \
	"$(INTDIR)\util.obj" \
	"$(OUTDIR)\libsbml.lib"

"$(OUTDIR)\translateMath.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "translateMath - Win32 Debug"

OUTDIR=.\bin
INTDIR=.\translateMath___Win32_Debug
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\translateMathD.exe"

!ELSE 

ALL : "libsbml - Win32 Debug" "$(OUTDIR)\translateMathD.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\translateMath.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\translateMathD.exe"
	-@erase "$(OUTDIR)\translateMathD.ilk"
	-@erase "$(OUTDIR)\translateMathD.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\translateMath.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\translateMath.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbmlD.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\translateMathD.pdb" /debug /machine:I386 /out:"$(OUTDIR)\translateMathD.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\translateMath.obj" \
	"$(INTDIR)\util.obj" \
	"$(OUTDIR)\libsbmlD.lib"

"$(OUTDIR)\translateMathD.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

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


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("translateMath.dep")
!INCLUDE "translateMath.dep"
!ELSE 
!MESSAGE Warning: cannot find "translateMath.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "translateMath - Win32 Release" || "$(CFG)" == "translateMath - Win32 Debug"
SOURCE=..\examples\translateMath.c

"$(INTDIR)\translateMath.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\examples\util.c

"$(INTDIR)\util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "translateMath - Win32 Release"

"libsbml - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" 
   cd "."

"libsbml - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" RECURSE=1 CLEAN 
   cd "."

!ELSEIF  "$(CFG)" == "translateMath - Win32 Debug"

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

