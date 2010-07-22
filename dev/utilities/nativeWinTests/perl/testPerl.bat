echo off
cd ..
cd ..
cd ..
cd ..
cd src
cd bindings
cd perl
copy Libsbml.pm C:\Perl\lib\Libsbml.pm
cd ..
cd ..
cd ..
cd win
cd bin
cd perl
copy Libsbml.dll C:\Perl\lib\auto\libSBML\Libsbml.dll
cd ..
cd ..
cd ..
cd src/bindings/perl
set "str2="

set "str3=LoadModule"
perl t/01__LoadModule.t
if ERRORLEVEL 1 set "str2=%str3% failed"

set "str3=CreateDocument"
perl t/02__CreateDocument.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadFromFile"
perl t/02__ReadFromFile.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadFromFileL3"
perl t/02__ReadFromFileL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=WriteToFile"
perl t/02__WriteToFile.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ChangeValues"
perl t/03__ChangeValues.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=DowncastRule"
perl t/03__DowncastRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=DowncastSBase"
perl t/03__DowncastSBase.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=DowncastSBaseL3"
perl t/03__DowncastSBaseL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ListOf"
perl t/04__ListOf.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ListOfL3"
perl t/04__ListOfL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=AlgebraicRule"
perl t/05__AlgebraicRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=AssignmentRule"
perl t/05__AssignmentRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=AssignmentRuleL3"
perl t/05__AssignmentRuleL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Compartment"
perl t/05__Compartment.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=CompartmentL3"
perl t/05__CompartmentL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=CompartmentVolumeRule"
perl t/05__CompartmentVolumeRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=EventAssignment"
perl t/05__EventAssignment.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Event"
perl t/05__Event.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=EventL3"
perl t/05__EventL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Formula"
perl t/05__Formula.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=FunctionDefinition"
perl t/05__FunctionDefinition.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=KineticLaw"
perl t/05__KineticLaw.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=KineticLawL3"
perl t/05__KineticLawL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Model"
perl t/05__Model.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ModifierSpeciesReference"
perl t/05__ModifierSpeciesReference.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ParameterRule"
perl t/05__ParameterRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Parameter"
perl t/05__Parameter.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ParameterL3"
perl t/05__ParameterL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=RateRule"
perl t/05__RateRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=SBMLDocument"
perl t/05__SBMLDocument.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=SBMLDocumentVerbose"
perl t/05__SBMLDocumentVerbose.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadSBML"
perl t/06__TestReadSBML.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=WriteSBML"
perl t/06__TestWriteSBML.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=CVTerms"
perl t/07__TestCVTerms.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ModelHistory"
perl t/07__TestModelHistory.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=RDFAnnotation2"
perl t/07__TestRDFAnnotation2.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"


echo %str2%

cd ..
cd ..
cd ..
cd dev
cd utilities
cd nativeWinTests
cd perl



