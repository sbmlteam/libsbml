# Microsoft Developer Studio Generated NMAKE File, Based on libsbml.dsp
!IF "$(CFG)" == ""
CFG=libsbml - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libsbml - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libsbml - Win32 Release" && "$(CFG)" != "libsbml - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libsbml - Win32 Release"

OUTDIR=.\bin
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

ALL : "$(OUTDIR)\libsbml.dll"


CLEAN :
	-@erase "$(INTDIR)\AlgebraicRule.obj"
	-@erase "$(INTDIR)\AssignmentRule.obj"
	-@erase "$(INTDIR)\ASTNode.obj"
	-@erase "$(INTDIR)\Compartment.obj"
	-@erase "$(INTDIR)\CompartmentVolumeRule.obj"
	-@erase "$(INTDIR)\Event.obj"
	-@erase "$(INTDIR)\EventAssignment.obj"
	-@erase "$(INTDIR)\FormulaFormatter.obj"
	-@erase "$(INTDIR)\FormulaParser.obj"
	-@erase "$(INTDIR)\FormulaTokenizer.obj"
	-@erase "$(INTDIR)\FunctionDefinition.obj"
	-@erase "$(INTDIR)\KineticLaw.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\ListOf.obj"
	-@erase "$(INTDIR)\MathMLDocument.obj"
	-@erase "$(INTDIR)\MathMLFormatter.obj"
	-@erase "$(INTDIR)\MathMLHandler.obj"
	-@erase "$(INTDIR)\MathMLReader.obj"
	-@erase "$(INTDIR)\MathMLTagCodes.obj"
	-@erase "$(INTDIR)\MathMLWriter.obj"
	-@erase "$(INTDIR)\memory.obj"
	-@erase "$(INTDIR)\Model.obj"
	-@erase "$(INTDIR)\ModifierSpeciesReference.obj"
	-@erase "$(INTDIR)\Parameter.obj"
	-@erase "$(INTDIR)\ParameterRule.obj"
	-@erase "$(INTDIR)\ParseMessage.obj"
	-@erase "$(INTDIR)\RateRule.obj"
	-@erase "$(INTDIR)\Reaction.obj"
	-@erase "$(INTDIR)\Rule.obj"
	-@erase "$(INTDIR)\RuleType.obj"
	-@erase "$(INTDIR)\SBase.obj"
	-@erase "$(INTDIR)\SBMLConvert.obj"
	-@erase "$(INTDIR)\SBMLDocument.obj"
	-@erase "$(INTDIR)\SBMLFormatter.obj"
	-@erase "$(INTDIR)\SBMLHandler.obj"
	-@erase "$(INTDIR)\SBMLReader.obj"
	-@erase "$(INTDIR)\SBMLTagCodes.obj"
	-@erase "$(INTDIR)\SBMLWriter.obj"
	-@erase "$(INTDIR)\SimpleSpeciesReference.obj"
	-@erase "$(INTDIR)\Species.obj"
	-@erase "$(INTDIR)\SpeciesConcentrationRule.obj"
	-@erase "$(INTDIR)\SpeciesReference.obj"
	-@erase "$(INTDIR)\Stack.obj"
	-@erase "$(INTDIR)\StringBuffer.obj"
	-@erase "$(INTDIR)\StringMap.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\UnitDefinition.obj"
	-@erase "$(INTDIR)\UnitKind.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\ValidationRules.obj"
	-@erase "$(INTDIR)\Validator.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\XMLNamespace.obj"
	-@erase "$(INTDIR)\XMLNamespaceList.obj"
	-@erase "$(INTDIR)\XMLStringFormatter.obj"
	-@erase "$(INTDIR)\XMLUtil.obj"
	-@erase "$(OUTDIR)\libsbml.dll"
	-@erase "$(OUTDIR)\libsbml.exp"
	-@erase "$(OUTDIR)\libsbml.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /Fp"$(INTDIR)\libsbml.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libsbml.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/xerces-c_2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\libsbml.pdb" /machine:I386 /out:"$(OUTDIR)\libsbml.dll" /implib:"$(OUTDIR)\libsbml.lib" 
LINK32_OBJS= \
	"$(INTDIR)\AlgebraicRule.obj" \
	"$(INTDIR)\AssignmentRule.obj" \
	"$(INTDIR)\ASTNode.obj" \
	"$(INTDIR)\Compartment.obj" \
	"$(INTDIR)\CompartmentVolumeRule.obj" \
	"$(INTDIR)\Event.obj" \
	"$(INTDIR)\EventAssignment.obj" \
	"$(INTDIR)\FormulaFormatter.obj" \
	"$(INTDIR)\FormulaParser.obj" \
	"$(INTDIR)\FormulaTokenizer.obj" \
	"$(INTDIR)\FunctionDefinition.obj" \
	"$(INTDIR)\KineticLaw.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\ListOf.obj" \
	"$(INTDIR)\MathMLDocument.obj" \
	"$(INTDIR)\MathMLFormatter.obj" \
	"$(INTDIR)\MathMLHandler.obj" \
	"$(INTDIR)\MathMLReader.obj" \
	"$(INTDIR)\MathMLTagCodes.obj" \
	"$(INTDIR)\MathMLWriter.obj" \
	"$(INTDIR)\memory.obj" \
	"$(INTDIR)\Model.obj" \
	"$(INTDIR)\ModifierSpeciesReference.obj" \
	"$(INTDIR)\Parameter.obj" \
	"$(INTDIR)\ParameterRule.obj" \
	"$(INTDIR)\ParseMessage.obj" \
	"$(INTDIR)\RateRule.obj" \
	"$(INTDIR)\Reaction.obj" \
	"$(INTDIR)\Rule.obj" \
	"$(INTDIR)\RuleType.obj" \
	"$(INTDIR)\SBase.obj" \
	"$(INTDIR)\SBMLConvert.obj" \
	"$(INTDIR)\SBMLDocument.obj" \
	"$(INTDIR)\SBMLFormatter.obj" \
	"$(INTDIR)\SBMLHandler.obj" \
	"$(INTDIR)\SBMLReader.obj" \
	"$(INTDIR)\SBMLTagCodes.obj" \
	"$(INTDIR)\SBMLWriter.obj" \
	"$(INTDIR)\SimpleSpeciesReference.obj" \
	"$(INTDIR)\Species.obj" \
	"$(INTDIR)\SpeciesConcentrationRule.obj" \
	"$(INTDIR)\SpeciesReference.obj" \
	"$(INTDIR)\Stack.obj" \
	"$(INTDIR)\StringBuffer.obj" \
	"$(INTDIR)\StringMap.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\UnitDefinition.obj" \
	"$(INTDIR)\UnitKind.obj" \
	"$(INTDIR)\util.obj" \
	"$(INTDIR)\ValidationRules.obj" \
	"$(INTDIR)\Validator.obj" \
	"$(INTDIR)\XMLStringFormatter.obj" \
	"$(INTDIR)\XMLUtil.obj" \
	"$(INTDIR)\XMLNamespace.obj" \
	"$(INTDIR)\XMLNamespaceList.obj"

"$(OUTDIR)\libsbml.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libsbml - Win32 Debug"

OUTDIR=.\bin
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\bin
# End Custom Macros

ALL : "$(OUTDIR)\libsbmlD.dll"


CLEAN :
	-@erase "$(INTDIR)\AlgebraicRule.obj"
	-@erase "$(INTDIR)\AssignmentRule.obj"
	-@erase "$(INTDIR)\ASTNode.obj"
	-@erase "$(INTDIR)\Compartment.obj"
	-@erase "$(INTDIR)\CompartmentVolumeRule.obj"
	-@erase "$(INTDIR)\Event.obj"
	-@erase "$(INTDIR)\EventAssignment.obj"
	-@erase "$(INTDIR)\FormulaFormatter.obj"
	-@erase "$(INTDIR)\FormulaParser.obj"
	-@erase "$(INTDIR)\FormulaTokenizer.obj"
	-@erase "$(INTDIR)\FunctionDefinition.obj"
	-@erase "$(INTDIR)\KineticLaw.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\ListOf.obj"
	-@erase "$(INTDIR)\MathMLDocument.obj"
	-@erase "$(INTDIR)\MathMLFormatter.obj"
	-@erase "$(INTDIR)\MathMLHandler.obj"
	-@erase "$(INTDIR)\MathMLReader.obj"
	-@erase "$(INTDIR)\MathMLTagCodes.obj"
	-@erase "$(INTDIR)\MathMLWriter.obj"
	-@erase "$(INTDIR)\memory.obj"
	-@erase "$(INTDIR)\Model.obj"
	-@erase "$(INTDIR)\ModifierSpeciesReference.obj"
	-@erase "$(INTDIR)\Parameter.obj"
	-@erase "$(INTDIR)\ParameterRule.obj"
	-@erase "$(INTDIR)\ParseMessage.obj"
	-@erase "$(INTDIR)\RateRule.obj"
	-@erase "$(INTDIR)\Reaction.obj"
	-@erase "$(INTDIR)\Rule.obj"
	-@erase "$(INTDIR)\RuleType.obj"
	-@erase "$(INTDIR)\SBase.obj"
	-@erase "$(INTDIR)\SBMLConvert.obj"
	-@erase "$(INTDIR)\SBMLDocument.obj"
	-@erase "$(INTDIR)\SBMLFormatter.obj"
	-@erase "$(INTDIR)\SBMLHandler.obj"
	-@erase "$(INTDIR)\SBMLReader.obj"
	-@erase "$(INTDIR)\SBMLTagCodes.obj"
	-@erase "$(INTDIR)\SBMLWriter.obj"
	-@erase "$(INTDIR)\SimpleSpeciesReference.obj"
	-@erase "$(INTDIR)\Species.obj"
	-@erase "$(INTDIR)\SpeciesConcentrationRule.obj"
	-@erase "$(INTDIR)\SpeciesReference.obj"
	-@erase "$(INTDIR)\Stack.obj"
	-@erase "$(INTDIR)\StringBuffer.obj"
	-@erase "$(INTDIR)\StringMap.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\UnitDefinition.obj"
	-@erase "$(INTDIR)\UnitKind.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\ValidationRules.obj"
	-@erase "$(INTDIR)\Validator.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\XMLNamespace.obj"
	-@erase "$(INTDIR)\XMLNamespaceList.obj"
	-@erase "$(INTDIR)\XMLStringFormatter.obj"
	-@erase "$(INTDIR)\XMLUtil.obj"
	-@erase "$(OUTDIR)\libsbmlD.dll"
	-@erase "$(OUTDIR)\libsbmlD.exp"
	-@erase "$(OUTDIR)\libsbmlD.ilk"
	-@erase "$(OUTDIR)\libsbmlD.lib"
	-@erase "$(OUTDIR)\libsbmlD.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSBML_EXPORTS" /Fp"$(INTDIR)\libsbml.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libsbml.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=bin/xerces-c_2D.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\libsbmlD.pdb" /debug /machine:I386 /out:"$(OUTDIR)\libsbmlD.dll" /implib:"$(OUTDIR)\libsbmlD.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\AlgebraicRule.obj" \
	"$(INTDIR)\AssignmentRule.obj" \
	"$(INTDIR)\ASTNode.obj" \
	"$(INTDIR)\Compartment.obj" \
	"$(INTDIR)\CompartmentVolumeRule.obj" \
	"$(INTDIR)\Event.obj" \
	"$(INTDIR)\EventAssignment.obj" \
	"$(INTDIR)\FormulaFormatter.obj" \
	"$(INTDIR)\FormulaParser.obj" \
	"$(INTDIR)\FormulaTokenizer.obj" \
	"$(INTDIR)\FunctionDefinition.obj" \
	"$(INTDIR)\KineticLaw.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\ListOf.obj" \
	"$(INTDIR)\MathMLDocument.obj" \
	"$(INTDIR)\MathMLFormatter.obj" \
	"$(INTDIR)\MathMLHandler.obj" \
	"$(INTDIR)\MathMLReader.obj" \
	"$(INTDIR)\MathMLTagCodes.obj" \
	"$(INTDIR)\MathMLWriter.obj" \
	"$(INTDIR)\memory.obj" \
	"$(INTDIR)\Model.obj" \
	"$(INTDIR)\ModifierSpeciesReference.obj" \
	"$(INTDIR)\Parameter.obj" \
	"$(INTDIR)\ParameterRule.obj" \
	"$(INTDIR)\ParseMessage.obj" \
	"$(INTDIR)\RateRule.obj" \
	"$(INTDIR)\Reaction.obj" \
	"$(INTDIR)\Rule.obj" \
	"$(INTDIR)\RuleType.obj" \
	"$(INTDIR)\SBase.obj" \
	"$(INTDIR)\SBMLConvert.obj" \
	"$(INTDIR)\SBMLDocument.obj" \
	"$(INTDIR)\SBMLFormatter.obj" \
	"$(INTDIR)\SBMLHandler.obj" \
	"$(INTDIR)\SBMLReader.obj" \
	"$(INTDIR)\SBMLTagCodes.obj" \
	"$(INTDIR)\SBMLWriter.obj" \
	"$(INTDIR)\SimpleSpeciesReference.obj" \
	"$(INTDIR)\Species.obj" \
	"$(INTDIR)\SpeciesConcentrationRule.obj" \
	"$(INTDIR)\SpeciesReference.obj" \
	"$(INTDIR)\Stack.obj" \
	"$(INTDIR)\StringBuffer.obj" \
	"$(INTDIR)\StringMap.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\UnitDefinition.obj" \
	"$(INTDIR)\UnitKind.obj" \
	"$(INTDIR)\util.obj" \
	"$(INTDIR)\ValidationRules.obj" \
	"$(INTDIR)\Validator.obj" \
	"$(INTDIR)\XMLStringFormatter.obj" \
	"$(INTDIR)\XMLUtil.obj" \
	"$(INTDIR)\XMLNamespace.obj" \
	"$(INTDIR)\XMLNamespaceList.obj"

"$(OUTDIR)\libsbmlD.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("libsbml.dep")
!INCLUDE "libsbml.dep"
!ELSE 
!MESSAGE Warning: cannot find "libsbml.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libsbml - Win32 Release" || "$(CFG)" == "libsbml - Win32 Debug"
SOURCE=..\src\AlgebraicRule.cpp

"$(INTDIR)\AlgebraicRule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\AssignmentRule.cpp

"$(INTDIR)\AssignmentRule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\ASTNode.cpp

"$(INTDIR)\ASTNode.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Compartment.cpp

"$(INTDIR)\Compartment.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\CompartmentVolumeRule.cpp

"$(INTDIR)\CompartmentVolumeRule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Event.cpp

"$(INTDIR)\Event.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\EventAssignment.cpp

"$(INTDIR)\EventAssignment.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\FormulaFormatter.c

"$(INTDIR)\FormulaFormatter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\FormulaParser.c

"$(INTDIR)\FormulaParser.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\FormulaTokenizer.c

"$(INTDIR)\FormulaTokenizer.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\FunctionDefinition.cpp

"$(INTDIR)\FunctionDefinition.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\KineticLaw.cpp

"$(INTDIR)\KineticLaw.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\List.cpp

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\ListOf.cpp

"$(INTDIR)\ListOf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\MathMLDocument.cpp

"$(INTDIR)\MathMLDocument.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\MathMLFormatter.cpp

"$(INTDIR)\MathMLFormatter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\MathMLHandler.cpp

"$(INTDIR)\MathMLHandler.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\MathMLReader.cpp

"$(INTDIR)\MathMLReader.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\MathMLTagCodes.cpp

"$(INTDIR)\MathMLTagCodes.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\MathMLWriter.cpp

"$(INTDIR)\MathMLWriter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\memory.c

"$(INTDIR)\memory.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Model.cpp

"$(INTDIR)\Model.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\ModifierSpeciesReference.cpp

"$(INTDIR)\ModifierSpeciesReference.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Parameter.cpp

"$(INTDIR)\Parameter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\ParameterRule.cpp

"$(INTDIR)\ParameterRule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\ParseMessage.cpp

"$(INTDIR)\ParseMessage.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\RateRule.cpp

"$(INTDIR)\RateRule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Reaction.cpp

"$(INTDIR)\Reaction.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Rule.cpp

"$(INTDIR)\Rule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\RuleType.c

"$(INTDIR)\RuleType.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBase.cpp

"$(INTDIR)\SBase.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLConvert.c

"$(INTDIR)\SBMLConvert.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLDocument.cpp

"$(INTDIR)\SBMLDocument.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLFormatter.cpp

"$(INTDIR)\SBMLFormatter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLHandler.cpp

"$(INTDIR)\SBMLHandler.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLReader.cpp

"$(INTDIR)\SBMLReader.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLTagCodes.cpp

"$(INTDIR)\SBMLTagCodes.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SBMLWriter.cpp

"$(INTDIR)\SBMLWriter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SimpleSpeciesReference.cpp

"$(INTDIR)\SimpleSpeciesReference.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Species.cpp

"$(INTDIR)\Species.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SpeciesConcentrationRule.cpp

"$(INTDIR)\SpeciesConcentrationRule.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\SpeciesReference.cpp

"$(INTDIR)\SpeciesReference.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Stack.c

"$(INTDIR)\Stack.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\StringBuffer.c

"$(INTDIR)\StringBuffer.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\StringMap.c

"$(INTDIR)\StringMap.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Unit.cpp

"$(INTDIR)\Unit.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\UnitDefinition.cpp

"$(INTDIR)\UnitDefinition.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\UnitKind.c

"$(INTDIR)\UnitKind.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\util.c

"$(INTDIR)\util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\ValidationRules.c

"$(INTDIR)\ValidationRules.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\Validator.c

"$(INTDIR)\Validator.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\XMLNamespace.cpp

"$(INTDIR)\XMLNamespace.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\XMLNamespaceList.cpp

"$(INTDIR)\XMLNamespaceList.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\XMLStringFormatter.cpp

"$(INTDIR)\XMLStringFormatter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\src\XMLUtil.cpp

"$(INTDIR)\XMLUtil.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

