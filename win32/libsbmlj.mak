# Microsoft Developer Studio Generated NMAKE File, Based on libsbmlj.dsp
!IF "$(CFG)" == ""
CFG=libsbmlj - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libsbmlj - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libsbmlj - Win32 Release" && "$(CFG)" != "libsbmlj - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libsbmlj.mak" CFG="libsbmlj - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libsbmlj - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libsbmlj - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "libsbmlj - Win32 Release"

OUTDIR=.\bin
INTDIR=.\libsbmlj___Win32_Release
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\libsbmlj.dll"

!ELSE 

ALL : "libsbml - Win32 Release" "$(OUTDIR)\libsbmlj.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\libsbml_wrap.obj"
	-@erase "$(INTDIR)\local.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\libsbmlj.dll"
	-@erase "$(OUTDIR)\libsbmlj.exp"
	-@erase "$(OUTDIR)\libsbmlj.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBMLJ_EXPORTS" /Fp"$(INTDIR)\libsbmlj.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libsbmlj.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbml.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\libsbmlj.pdb" /machine:I386 /out:"$(OUTDIR)\libsbmlj.dll" /implib:"$(OUTDIR)\libsbmlj.lib" 
LINK32_OBJS= \
	"$(INTDIR)\libsbml_wrap.obj" \
	"$(INTDIR)\local.obj" \
	"$(OUTDIR)\libsbml.lib"

"$(OUTDIR)\libsbmlj.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libsbmlj - Win32 Debug"

OUTDIR=.\bin
INTDIR=.\libsbmlj___Win32_Debug
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\libsbmljD.dll"

!ELSE 

ALL : "libsbml - Win32 Debug" "$(OUTDIR)\libsbmljD.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libsbml - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\libsbml_wrap.obj"
	-@erase "$(INTDIR)\local.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libsbmljD.dll"
	-@erase "$(OUTDIR)\libsbmljD.exp"
	-@erase "$(OUTDIR)\libsbmljD.ilk"
	-@erase "$(OUTDIR)\libsbmljD.lib"
	-@erase "$(OUTDIR)\libsbmljD.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBMLJ_EXPORTS" /Fp"$(INTDIR)\libsbmlj.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libsbmlj.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/libsbmlD.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\libsbmljD.pdb" /debug /machine:I386 /out:"$(OUTDIR)\libsbmljD.dll" /implib:"$(OUTDIR)\libsbmljD.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\libsbml_wrap.obj" \
	"$(INTDIR)\local.obj" \
	"$(OUTDIR)\libsbmlD.lib"

"$(OUTDIR)\libsbmljD.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("libsbmlj.dep")
!INCLUDE "libsbmlj.dep"
!ELSE 
!MESSAGE Warning: cannot find "libsbmlj.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libsbmlj - Win32 Release" || "$(CFG)" == "libsbmlj - Win32 Debug"
SOURCE=..\bindings\java\libsbml_wrap.cpp

"$(INTDIR)\libsbml_wrap.obj" : $(SOURCE) "$(INTDIR)" "..\bindings\java\local.cpp"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\bindings\java\local.cpp

"$(INTDIR)\local.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "libsbmlj - Win32 Release"

"libsbml - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" 
   cd "."

"libsbml - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F ".\libsbml.mak" CFG="libsbml - Win32 Release" RECURSE=1 CLEAN 
   cd "."

!ELSEIF  "$(CFG)" == "libsbmlj - Win32 Debug"

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

