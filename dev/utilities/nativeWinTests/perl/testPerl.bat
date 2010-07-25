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
echo %str3%
perl t/01__LoadModule.t
if ERRORLEVEL 1 set "str2=%str3% failed"

set "str3=CreateDocument"
echo %str3%
perl t/02__CreateDocument.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadFromFile"
echo %str3%
perl t/02__ReadFromFile.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadFromFileL3"
echo %str3%
perl t/02__ReadFromFileL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=WriteToFile"
echo %str3%
perl t/02__WriteToFile.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=WriteToFileL3"
echo %str3%
perl t/02__WriteToFileL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ChangeValues"
echo %str3%
perl t/03__ChangeValues.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=DowncastRule"
echo %str3%
perl t/03__DowncastRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=DowncastSBase"
echo %str3%
perl t/03__DowncastSBase.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=DowncastSBaseL3"
echo %str3%
perl t/03__DowncastSBaseL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ListOf"
echo %str3%
perl t/04__ListOf.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ListOfL3"
echo %str3%
perl t/04__ListOfL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=AlgebraicRule"
echo %str3%
perl t/05__AlgebraicRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=AssignmentRule"
echo %str3%
perl t/05__AssignmentRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=AssignmentRuleL3"
echo %str3%
perl t/05__AssignmentRuleL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Compartment"
echo %str3%
perl t/05__Compartment.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=CompartmentL3"
echo %str3%
perl t/05__CompartmentL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=CompartmentVolumeRule"
echo %str3%
perl t/05__CompartmentVolumeRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=EventAssignment"
echo %str3%
perl t/05__EventAssignment.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Event"
echo %str3%
perl t/05__Event.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=EventL3"
echo %str3%
perl t/05__EventL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Formula"
echo %str3%
perl t/05__Formula.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=FunctionDefinition"
echo %str3%
perl t/05__FunctionDefinition.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=KineticLaw"
echo %str3%
perl t/05__KineticLaw.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=KineticLawL3"
echo %str3%
perl t/05__KineticLawL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Model"
echo %str3%
perl t/05__Model.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ModifierSpeciesReference"
echo %str3%
perl t/05__ModifierSpeciesReference.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ParameterRule"
echo %str3%
perl t/05__ParameterRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=Parameter"
echo %str3%
perl t/05__Parameter.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ParameterL3"
echo %str3%
perl t/05__ParameterL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=RateRule"
echo %str3%
perl t/05__RateRule.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=SBMLDocument"
echo %str3%
perl t/05__SBMLDocument.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=SBMLDocumentVerbose"
echo %str3%
perl t/05__SBMLDocumentVerbose.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadSBML"
echo %str3%
perl t/06__TestReadSBML.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ReadSBMLL3"
echo %str3%
perl t/06__TestReadSBMLL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=WriteSBML"
echo %str3%
perl t/06__TestWriteSBML.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=WriteSBMLL3"
echo %str3%
perl t/06__TestWriteSBMLL3.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=CVTerms"
echo %str3%
perl t/07__TestCVTerms.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=ModelHistory"
echo %str3%
perl t/07__TestModelHistory.t
if ERRORLEVEL 1 set "str2=%str2% %str3% failed"

set "str3=RDFAnnotation2"
echo %str3%
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



